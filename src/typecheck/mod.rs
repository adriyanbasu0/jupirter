use crate::ast::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug)]
pub struct TypeError {
    pub message: String,
    pub location: String,
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Type error at {}: {}", self.location, self.message)
    }
}

impl std::error::Error for TypeError {}

pub fn typecheck(program: &Program) -> Result<Program, TypeError> {
    let mut ctx = TypeContext::new();
    ctx.typecheck_program(program)?;
    Ok(program.clone())
}

struct TypeContext {
    scopes: Vec<HashMap<String, (Type, bool)>>,
    struct_types: HashMap<String, Struct>,
    union_types: HashMap<String, Union>,
    enum_types: HashMap<String, Enum>,
    capability_types: HashMap<String, PhysAddrType>,
    topology_types: HashMap<String, MemoryTopology>,
    bit_region_types: HashMap<String, BitRegionType>,
    entropy_types: HashMap<String, EntropyType>,
    current_function: Option<String>,
}

impl TypeContext {
    fn new() -> Self {
        let mut ctx = TypeContext {
            scopes: Vec::new(),
            struct_types: HashMap::new(),
            union_types: HashMap::new(),
            enum_types: HashMap::new(),
            capability_types: HashMap::new(),
            topology_types: HashMap::new(),
            bit_region_types: HashMap::new(),
            entropy_types: HashMap::new(),
            current_function: None,
        };
        ctx.push_scope();
        ctx
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn add_variable(&mut self, name: String, ty: Type, is_const: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, (ty, is_const));
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<&(Type, bool)> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return Some(binding);
            }
        }
        None
    }

    fn add_struct(&mut self, s: Struct) {
        self.struct_types.insert(s.name.clone(), s);
    }

    fn add_union(&mut self, u: Union) {
        self.union_types.insert(u.name.clone(), u);
    }

    fn add_enum(&mut self, e: Enum) {
        self.enum_types.insert(e.name.clone(), e);
    }

    fn lookup_struct(&self, name: &str) -> Option<&Struct> {
        self.struct_types.get(name)
    }

    fn lookup_union(&self, name: &str) -> Option<&Union> {
        self.union_types.get(name)
    }

    fn lookup_enum(&self, name: &str) -> Option<&Enum> {
        self.enum_types.get(name)
    }

    fn typecheck_program(&mut self, program: &Program) -> Result<(), TypeError> {
        for item in &program.items {
            self.typecheck_item(item)?;
        }
        Ok(())
    }

    fn typecheck_item(&mut self, item: &Item) -> Result<(), TypeError> {
        match item {
            Item::Function(f) => self.typecheck_function(f),
            Item::Struct(s) => {
                self.add_struct(s.clone());
                Ok(())
            }
            Item::Union(u) => {
                self.add_union(u.clone());
                Ok(())
            }
            Item::Enum(e) => {
                self.add_enum(e.clone());
                Ok(())
            }
            Item::Const(c) => self.typecheck_const_decl(c, true),
            Item::Var(v) => self.typecheck_var_decl(v, true),
            Item::CapabilityDecl(c) => self.typecheck_capability_decl(c),
            Item::TopologyDecl(t) => self.typecheck_topology_decl(t),
            Item::BitRegionDecl(b) => self.typecheck_bit_region_decl(b),
            Item::EntropyDecl(e) => self.typecheck_entropy_decl(e),
        }
    }

    fn typecheck_function(&mut self, f: &Function) -> Result<(), TypeError> {
        let prev_fn = self.current_function.replace(f.name.clone());
        self.push_scope();
        for param in &f.params {
            self.add_variable(param.name.clone(), *param.ty.clone(), true);
        }
        for stmt in &f.body {
            self.typecheck_stmt(stmt)?;
        }
        self.pop_scope();
        self.current_function = prev_fn;
        Ok(())
    }

    fn typecheck_const_decl(&mut self, c: &ConstDecl, _global: bool) -> Result<(), TypeError> {
        let value_type = self.typecheck_expr(&c.value)?;
        if let Some(expected_ty) = &c.ty {
            if **expected_ty != value_type {
                return Err(TypeError {
                    message: format!(
                        "Type mismatch in const: expected {:?}, got {:?}",
                        expected_ty, value_type
                    ),
                    location: format!("const {}", c.name),
                });
            }
        }
        Ok(())
    }

    fn typecheck_var_decl(&mut self, v: &VarDecl, _global: bool) -> Result<(), TypeError> {
        let value_type = self.typecheck_expr(&v.value)?;
        if let Some(expected_ty) = &v.ty {
            if **expected_ty != value_type {
                return Err(TypeError {
                    message: format!(
                        "Type mismatch in var: expected {:?}, got {:?}",
                        expected_ty, value_type
                    ),
                    location: format!("var {}", v.name),
                });
            }
        }
        Ok(())
    }

    fn typecheck_capability_decl(&mut self, c: &CapabilityDecl) -> Result<(), TypeError> {
        let phys_type = PhysAddrType {
            base_address: c.base_address,
            length: c.length,
            mode: c.mode.clone(),
            element_type: c.element_type.clone(),
        };
        self.capability_types
            .insert(c.name.clone(), phys_type.clone());
        self.add_variable(c.name.clone(), Type::PhysAddr(phys_type), true);
        Ok(())
    }

    fn typecheck_topology_decl(&mut self, t: &TopologyDecl) -> Result<(), TypeError> {
        self.topology_types
            .insert(t.name.clone(), t.topology.clone());
        let topo_type = Type::Topology(
            TopologyType {
                topology: t.topology.clone(),
            },
            Box::new(Type::U64),
        );
        self.add_variable(t.name.clone(), topo_type, true);
        Ok(())
    }

    fn typecheck_bit_region_decl(&mut self, b: &BitRegionDecl) -> Result<(), TypeError> {
        let region_type = BitRegionType {
            base_type: b.base_type.clone(),
            regions: b.regions.clone(),
        };
        self.bit_region_types
            .insert(b.name.clone(), region_type.clone());
        self.add_variable(b.name.clone(), Type::BitRegion(region_type), true);
        Ok(())
    }

    fn typecheck_entropy_decl(&mut self, e: &EntropyDecl) -> Result<(), TypeError> {
        let entropy_type = EntropyType {
            base_type: e.ty.clone(),
            state: e.initial_entropy.clone(),
        };
        self.entropy_types
            .insert(e.name.clone(), entropy_type.clone());
        self.add_variable(e.name.clone(), Type::Entropy(entropy_type), true);
        Ok(())
    }

    fn typecheck_stmt(&mut self, stmt: &Stmt) -> Result<(), TypeError> {
        match stmt {
            Stmt::Let(l) => self.typecheck_let_stmt(l),
            Stmt::Const(c) => self.typecheck_const_stmt(c),
            Stmt::Expr(e) => {
                self.typecheck_expr(e)?;
                Ok(())
            }
            Stmt::Return(r) => {
                if let Some(expr) = r {
                    self.typecheck_expr(expr)?;
                }
                Ok(())
            }
            Stmt::Break => Ok(()),
            Stmt::Continue => Ok(()),
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.typecheck_stmt(s)?;
                }
                Ok(())
            }
            Stmt::If(if_stmt) => self.typecheck_if_stmt(if_stmt),
            Stmt::While(w) => self.typecheck_while_stmt(w),
            Stmt::For(f) => self.typecheck_for_stmt(f),
            Stmt::Asm(_) => Ok(()),
            Stmt::Defer(d) => self.typecheck_stmt(d),
            Stmt::EntropyAssert(ea) => self.typecheck_entropy_assert(ea),
        }
    }

    fn typecheck_let_stmt(&mut self, l: &LetStmt) -> Result<(), TypeError> {
        let value_type = self.typecheck_expr(&l.value)?;
        if let Some(expected_ty) = &l.ty {
            if **expected_ty != value_type {
                return Err(TypeError {
                    message: format!(
                        "Type mismatch in let: expected {:?}, got {:?}",
                        expected_ty, value_type
                    ),
                    location: format!("let {}", l.name),
                });
            }
        }
        self.add_variable(l.name.clone(), value_type, l.is_const);
        Ok(())
    }

    fn typecheck_const_stmt(&mut self, c: &ConstStmt) -> Result<(), TypeError> {
        let value_type = self.typecheck_expr(&c.value)?;
        if let Some(expected_ty) = &c.ty {
            if **expected_ty != value_type {
                return Err(TypeError {
                    message: format!(
                        "Type mismatch in const: expected {:?}, got {:?}",
                        expected_ty, value_type
                    ),
                    location: format!("const {}", c.name),
                });
            }
        }
        self.add_variable(c.name.clone(), value_type, true);
        Ok(())
    }

    fn typecheck_if_stmt(&mut self, if_stmt: &IfStmt) -> Result<(), TypeError> {
        let cond_type = self.typecheck_expr(&if_stmt.condition)?;
        if cond_type != Type::Bool {
            return Err(TypeError {
                message: format!("If condition must be bool, got {:?}", cond_type),
                location: "if condition".to_string(),
            });
        }
        for stmt in &if_stmt.then_branch {
            self.typecheck_stmt(stmt)?;
        }
        if let Some(else_branch) = &if_stmt.else_branch {
            for stmt in else_branch {
                self.typecheck_stmt(stmt)?;
            }
        }
        Ok(())
    }

    fn typecheck_while_stmt(&mut self, w: &WhileStmt) -> Result<(), TypeError> {
        let cond_type = self.typecheck_expr(&w.condition)?;
        if cond_type != Type::Bool {
            return Err(TypeError {
                message: format!("While condition must be bool, got {:?}", cond_type),
                location: "while condition".to_string(),
            });
        }
        for stmt in &w.body {
            self.typecheck_stmt(stmt)?;
        }
        Ok(())
    }

    fn typecheck_for_stmt(&mut self, f: &ForStmt) -> Result<(), TypeError> {
        self.typecheck_stmt(&f.init)?;
        let cond_type = self.typecheck_expr(&f.condition)?;
        if cond_type != Type::Bool {
            return Err(TypeError {
                message: format!("For condition must be bool, got {:?}", cond_type),
                location: "for condition".to_string(),
            });
        }
        self.typecheck_stmt(&f.update)?;
        for stmt in &f.body {
            self.typecheck_stmt(stmt)?;
        }
        Ok(())
    }

    fn typecheck_entropy_assert(&mut self, ea: &EntropyAssertStmt) -> Result<(), TypeError> {
        let expr_ty = self.typecheck_expr(&ea.expr)?;
        match &expr_ty {
            Type::Entropy(entropy) => {
                if entropy.state != ea.expected {
                    return Err(TypeError {
                        message: format!(
                            "Entropy assertion failed: expected {:?}, got {:?}",
                            ea.expected, entropy.state
                        ),
                        location: "entropy_assert".to_string(),
                    });
                }
            }
            _ => {
                return Err(TypeError {
                    message: "Entropy assert on non-entropy type".to_string(),
                    location: "entropy_assert".to_string(),
                });
            }
        }
        Ok(())
    }

    fn typecheck_expr(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Literal(l) => self.typecheck_literal(l),
            Expr::Identifier(name) => {
                if let Some((ty, _)) = self.lookup_variable(name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError {
                        message: format!("Undefined variable: {}", name),
                        location: name.clone(),
                    })
                }
            }
            Expr::Unary(op, e) => self.typecheck_unary(op, e),
            Expr::Binary(op, l, r) => self.typecheck_binary(op, l, r),
            Expr::Call(f, args) => self.typecheck_call(f, args),
            Expr::Syscall(name, args) => {
                for arg in args {
                    self.typecheck_expr(arg)?;
                }
                Ok(Type::Isize)
            }
            Expr::Index(arr, idx) => self.typecheck_index(arr, idx),
            Expr::Field(e, field) => self.typecheck_field(e, field),
            Expr::PtrField(e, field) => self.typecheck_ptr_field(e, field),
            Expr::Cast(e, ty) => {
                self.typecheck_expr(e)?;
                Ok(ty.clone())
            }
            Expr::Sizeof(_ty) => Ok(Type::Usize),
            Expr::Alignof(_ty) => Ok(Type::Usize),
            Expr::Offsetof(_ty, _field) => Ok(Type::Usize),
            Expr::Assign(l, r) => self.typecheck_assign(l, r),
            Expr::AddrOf(e) => {
                let inner = self.typecheck_expr(e)?;
                Ok(Type::MutPtr(Box::new(inner)))
            }
            Expr::Deref(e) => {
                let ptr_type = self.typecheck_expr(e)?;
                match ptr_type {
                    Type::MutPtr(inner) | Type::ConstPtr(inner) => Ok(*inner),
                    Type::PhysAddr(p) => {
                        if let Some(elem) = p.element_type {
                            Ok(*elem)
                        } else {
                            Ok(Type::U8)
                        }
                    }
                    _ => Err(TypeError {
                        message: format!("Cannot dereference non-pointer type {:?}", ptr_type),
                        location: "deref".to_string(),
                    }),
                }
            }
            Expr::Block(stmts, result) => {
                for s in stmts {
                    self.typecheck_stmt(s)?;
                }
                if let Some(r) = result {
                    self.typecheck_expr(r)
                } else {
                    Ok(Type::Void)
                }
            }
            Expr::Alloc(ty, size) => {
                self.typecheck_expr(size)?;
                Ok(Type::MutPtr(Box::new((**ty).clone())))
            }
            Expr::Free(ptr, size) => {
                self.typecheck_expr(ptr)?;
                self.typecheck_expr(size)?;
                Ok(Type::Void)
            }
            Expr::If(if_expr) => self.typecheck_if_expr(if_expr),
            Expr::PhysAddr(p) => {
                let phys_type = PhysAddrType {
                    base_address: p.base_address,
                    length: p.length,
                    mode: p.mode.clone(),
                    element_type: p.element_type.clone(),
                };
                Ok(Type::PhysAddr(phys_type))
            }
            Expr::EntropyCheck(ec) => self.typecheck_entropy_check(ec),
            Expr::BitRegionAccess(bra) => self.typecheck_bit_region_access(bra),
            Expr::TopologyCast(tc) => self.typecheck_topology_cast(tc),
        }
    }

    fn typecheck_literal(&mut self, l: &Literal) -> Result<Type, TypeError> {
        match l {
            Literal::Int(_, suffix) => match suffix {
                IntSuffix::I8 => Ok(Type::I8),
                IntSuffix::I16 => Ok(Type::I16),
                IntSuffix::I32 => Ok(Type::I32),
                IntSuffix::I64 => Ok(Type::I64),
                IntSuffix::U8 => Ok(Type::U8),
                IntSuffix::U16 => Ok(Type::U16),
                IntSuffix::U32 => Ok(Type::U32),
                IntSuffix::U64 => Ok(Type::U64),
                IntSuffix::Usize => Ok(Type::Usize),
                IntSuffix::Isize => Ok(Type::Isize),
                IntSuffix::None => Ok(Type::I32),
            },
            Literal::Float(_, suffix) => match suffix {
                FloatSuffix::F32 => Ok(Type::F32),
                FloatSuffix::F64 => Ok(Type::F64),
                FloatSuffix::None => Ok(Type::F64),
            },
            Literal::Bool(_) => Ok(Type::Bool),
            Literal::String(_) => Ok(Type::MutPtr(Box::new(Type::U8))),
            Literal::Char(_) => Ok(Type::U8),
        }
    }

    fn typecheck_unary(&mut self, op: &UnaryOp, e: &Expr) -> Result<Type, TypeError> {
        let ty = self.typecheck_expr(e)?;
        match op {
            UnaryOp::Neg => {
                if ty.is_integer() || ty.is_float() {
                    Ok(ty)
                } else {
                    Err(TypeError {
                        message: format!("Cannot negate type {:?}", ty),
                        location: "neg".to_string(),
                    })
                }
            }
            UnaryOp::Not => {
                if ty == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError {
                        message: format!("Cannot logical NOT type {:?}", ty),
                        location: "not".to_string(),
                    })
                }
            }
            UnaryOp::BitNot => {
                if ty.is_integer() {
                    Ok(ty)
                } else {
                    Err(TypeError {
                        message: format!("Cannot bitwise NOT type {:?}", ty),
                        location: "bitnot".to_string(),
                    })
                }
            }
            UnaryOp::Deref => match ty {
                Type::MutPtr(inner) | Type::ConstPtr(inner) => Ok(*inner),
                Type::PhysAddr(p) => {
                    if let Some(elem) = p.element_type {
                        Ok(*elem)
                    } else {
                        Ok(Type::U8)
                    }
                }
                _ => Err(TypeError {
                    message: format!("Cannot dereference non-pointer type {:?}", ty),
                    location: "deref".to_string(),
                }),
            },
            UnaryOp::AddrOf => Ok(Type::MutPtr(Box::new(ty))),
        }
    }

    fn typecheck_binary(&mut self, op: &BinaryOp, l: &Expr, r: &Expr) -> Result<Type, TypeError> {
        let left = self.typecheck_expr(l)?;
        let right = self.typecheck_expr(r)?;

        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                if left.is_integer() && right.is_integer() {
                    Ok(left)
                } else if left.is_float() && right.is_float() {
                    Ok(left)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Invalid operand types for arithmetic: {:?} and {:?}",
                            left, right
                        ),
                        location: format!("{:?}", op),
                    })
                }
            }
            BinaryOp::LShift | BinaryOp::RShift => {
                if left.is_integer() && right.is_integer() {
                    Ok(left)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Invalid operand types for shift: {:?} and {:?}",
                            left, right
                        ),
                        location: format!("{:?}", op),
                    })
                }
            }
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor => {
                if left.is_integer() && right.is_integer() {
                    Ok(left)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Invalid operand types for bitwise: {:?} and {:?}",
                            left, right
                        ),
                        location: format!("{:?}", op),
                    })
                }
            }
            BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => {
                if (left.is_integer() || left.is_float())
                    && (right.is_integer() || right.is_float())
                {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Invalid operand types for comparison: {:?} and {:?}",
                            left, right
                        ),
                        location: format!("{:?}", op),
                    })
                }
            }
            BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                if left == Type::Bool && right == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Invalid operand types for logical: {:?} and {:?}",
                            left, right
                        ),
                        location: format!("{:?}", op),
                    })
                }
            }
        }
    }

    fn typecheck_call(&mut self, f: &Expr, args: &[Expr]) -> Result<Type, TypeError> {
        let func_type = self.typecheck_expr(f)?;

        match func_type {
            Type::Func(params, ret) => {
                if params.len() != args.len() {
                    return Err(TypeError {
                        message: format!(
                            "Wrong number of arguments: expected {}, got {}",
                            params.len(),
                            args.len()
                        ),
                        location: "function call".to_string(),
                    });
                }

                for (i, (arg, expected)) in args.iter().zip(params.iter()).enumerate() {
                    let arg_type = self.typecheck_expr(arg)?;
                    if arg_type != *expected {
                        return Err(TypeError {
                            message: format!(
                                "Argument {} type mismatch: expected {:?}, got {:?}",
                                i, expected, arg_type
                            ),
                            location: format!("argument {}", i),
                        });
                    }
                }

                Ok(*ret)
            }
            _ => Err(TypeError {
                message: format!("Cannot call non-function type {:?}", func_type),
                location: "function call".to_string(),
            }),
        }
    }

    fn typecheck_index(&mut self, arr: &Expr, idx: &Expr) -> Result<Type, TypeError> {
        let arr_type = self.typecheck_expr(arr)?;
        let idx_type = self.typecheck_expr(idx)?;

        match arr_type {
            Type::Array(_, elem_type) => {
                if idx_type.is_integer() {
                    Ok(*elem_type)
                } else {
                    Err(TypeError {
                        message: format!("Array index must be integer, got {:?}", idx_type),
                        location: "array index".to_string(),
                    })
                }
            }
            Type::MutPtr(inner) | Type::ConstPtr(inner) => {
                if idx_type.is_integer() {
                    Ok(*inner)
                } else {
                    Err(TypeError {
                        message: format!("Pointer index must be integer, got {:?}", idx_type),
                        location: "pointer index".to_string(),
                    })
                }
            }
            Type::PhysAddr(p) => {
                if idx_type.is_integer() {
                    if let Some(elem) = p.element_type {
                        Ok(*elem)
                    } else {
                        Ok(Type::U8)
                    }
                } else {
                    Err(TypeError {
                        message: format!("Physical capability index must be integer"),
                        location: "physaddr index".to_string(),
                    })
                }
            }
            _ => Err(TypeError {
                message: format!("Cannot index non-array/non-pointer type {:?}", arr_type),
                location: "array index".to_string(),
            }),
        }
    }

    fn typecheck_field(&mut self, e: &Expr, field: &str) -> Result<Type, TypeError> {
        let base_type = self.typecheck_expr(e)?;

        match base_type {
            Type::Named(name) => {
                if let Some(s) = self.lookup_struct(&name) {
                    for f in &s.fields {
                        if f.name == field {
                            return Ok(*f.ty.clone());
                        }
                    }
                    Err(TypeError {
                        message: format!("Struct {} has no field {}", name, field),
                        location: format!(".{}", field),
                    })
                } else {
                    Err(TypeError {
                        message: format!("Unknown struct type {}", name),
                        location: format!(".{}", field),
                    })
                }
            }
            Type::BitRegion(br) => {
                for r in &br.regions {
                    if r.name == field {
                        return Ok(Type::U32);
                    }
                }
                Err(TypeError {
                    message: format!("Bit region {} has no field {}", br.base_type.size(), field),
                    location: format!(".{}", field),
                })
            }
            _ => Err(TypeError {
                message: format!("Cannot access field on non-struct type {:?}", base_type),
                location: format!(".{}", field),
            }),
        }
    }

    fn typecheck_ptr_field(&mut self, e: &Expr, field: &str) -> Result<Type, TypeError> {
        let ptr_type = self.typecheck_expr(e)?;

        match ptr_type {
            Type::MutPtr(inner) | Type::ConstPtr(inner) => {
                let inner_type = *inner;
                match inner_type {
                    Type::Named(name) => {
                        if let Some(s) = self.lookup_struct(&name) {
                            for f in &s.fields {
                                if f.name == field {
                                    return Ok(*f.ty.clone());
                                }
                            }
                            Err(TypeError {
                                message: format!("Struct {} has no field {}", name, field),
                                location: format!("->{}", field),
                            })
                        } else {
                            Err(TypeError {
                                message: format!("Unknown struct type {}", name),
                                location: format!("->{}", field),
                            })
                        }
                    }
                    _ => Err(TypeError {
                        message: "Cannot access field through non-struct pointer".to_string(),
                        location: format!("->{}", field),
                    }),
                }
            }
            _ => Err(TypeError {
                message: "Cannot use -> on non-pointer type".to_string(),
                location: format!("->{}", field),
            }),
        }
    }

    fn typecheck_assign(&mut self, l: &Expr, r: &Expr) -> Result<Type, TypeError> {
        let left_type = self.typecheck_expr(l)?;
        let right_type = self.typecheck_expr(r)?;

        match l {
            Expr::Identifier(name) => {
                if let Some((_, is_const_binding)) = self.lookup_variable(name) {
                    if !*is_const_binding {
                        if left_type == right_type {
                            Ok(Type::Void)
                        } else {
                            Err(TypeError {
                                message: format!(
                                    "Type mismatch in assignment: {:?} and {:?}",
                                    left_type, right_type
                                ),
                                location: "assignment".to_string(),
                            })
                        }
                    } else {
                        Err(TypeError {
                            message: format!(
                                "Cannot assign to var binding '{}' (immutable value)",
                                name
                            ),
                            location: "assignment".to_string(),
                        })
                    }
                } else {
                    Err(TypeError {
                        message: format!("Undefined variable: {}", name),
                        location: "assignment".to_string(),
                    })
                }
            }
            Expr::Field(_, _) | Expr::PtrField(_, _) => {
                if left_type == right_type {
                    Ok(Type::Void)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Type mismatch in assignment: {:?} and {:?}",
                            left_type, right_type
                        ),
                        location: "assignment".to_string(),
                    })
                }
            }
            Expr::Index(_, _) => {
                if left_type == right_type {
                    Ok(Type::Void)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Type mismatch in assignment: {:?} and {:?}",
                            left_type, right_type
                        ),
                        location: "assignment".to_string(),
                    })
                }
            }
            Expr::Deref(_) => {
                if left_type == right_type {
                    Ok(Type::Void)
                } else {
                    Err(TypeError {
                        message: format!(
                            "Type mismatch in assignment: {:?} and {:?}",
                            left_type, right_type
                        ),
                        location: "assignment".to_string(),
                    })
                }
            }
            _ => Err(TypeError {
                message: "Invalid assignment target".to_string(),
                location: "assignment".to_string(),
            }),
        }
    }

    fn typecheck_if_expr(&mut self, if_expr: &IfExpr) -> Result<Type, TypeError> {
        let cond_type = self.typecheck_expr(&if_expr.condition)?;
        if cond_type != Type::Bool {
            return Err(TypeError {
                message: format!("If condition must be bool, got {:?}", cond_type),
                location: "if expression".to_string(),
            });
        }

        let then_type = self.typecheck_expr(&if_expr.then_expr)?;
        let else_type = self.typecheck_expr(&if_expr.else_expr)?;

        if then_type == else_type {
            Ok(then_type)
        } else {
            Err(TypeError {
                message: format!(
                    "If expression branches have different types: {:?} and {:?}",
                    then_type, else_type
                ),
                location: "if expression".to_string(),
            })
        }
    }

    fn typecheck_entropy_check(&mut self, ec: &EntropyCheckExpr) -> Result<Type, TypeError> {
        let ty = self.typecheck_expr(&ec.expr)?;
        match &ty {
            Type::Entropy(entropy) => {
                if entropy.state == ec.expected {
                    Ok(Type::Bool)
                } else {
                    Ok(Type::Bool)
                }
            }
            _ => Err(TypeError {
                message: "Entropy check on non-entropy type".to_string(),
                location: "entropy_check".to_string(),
            }),
        }
    }

    fn typecheck_bit_region_access(
        &mut self,
        bra: &BitRegionAccessExpr,
    ) -> Result<Type, TypeError> {
        let base_ty = self.typecheck_expr(&bra.base)?;
        match &base_ty {
            Type::BitRegion(br) => {
                for region in &br.regions {
                    if region.name == bra.region_name {
                        return Ok(Type::U32);
                    }
                }
                Err(TypeError {
                    message: format!("No bit region {}", bra.region_name),
                    location: "bit_region_access".to_string(),
                })
            }
            _ => Err(TypeError {
                message: "Bit region access on non-bit-region type".to_string(),
                location: "bit_region_access".to_string(),
            }),
        }
    }

    fn typecheck_topology_cast(&mut self, tc: &TopologyCastExpr) -> Result<Type, TypeError> {
        self.typecheck_expr(&tc.expr)?;
        Ok(Type::Topology(
            TopologyType {
                topology: tc.target_topology.clone(),
            },
            Box::new(self.typecheck_expr(&tc.expr)?),
        ))
    }
}
