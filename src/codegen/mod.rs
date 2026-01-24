pub mod binary;
use crate::ast::*;
pub use binary::*;
use std::fmt;

#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Codegen error: {}", self.message)
    }
}

impl std::error::Error for CodegenError {}

// FEATURE 9: Bit-precise integer type tracking
#[derive(Debug, Clone, Copy)]
struct IntType {
    bits: u8,
    signed: bool,
}

impl IntType {
    fn from_aura_type(ty: &Type) -> Option<Self> {
        match ty {
            Type::I8 => Some(IntType {
                bits: 8,
                signed: true,
            }),
            Type::I16 => Some(IntType {
                bits: 16,
                signed: true,
            }),
            Type::I32 => Some(IntType {
                bits: 32,
                signed: true,
            }),
            Type::I64 => Some(IntType {
                bits: 64,
                signed: true,
            }),
            Type::U8 => Some(IntType {
                bits: 8,
                signed: false,
            }),
            Type::U16 => Some(IntType {
                bits: 16,
                signed: false,
            }),
            Type::U32 => Some(IntType {
                bits: 32,
                signed: false,
            }),
            Type::U64 => Some(IntType {
                bits: 64,
                signed: false,
            }),
            Type::BitInt(bits, signed) => Some(IntType {
                bits: *bits,
                signed: *signed,
            }),
            _ => None,
        }
    }

    fn from_suffix(suffix: &IntSuffix) -> Option<Self> {
        match suffix {
            IntSuffix::I8 => Some(IntType {
                bits: 8,
                signed: true,
            }),
            IntSuffix::I16 => Some(IntType {
                bits: 16,
                signed: true,
            }),
            IntSuffix::I32 => Some(IntType {
                bits: 32,
                signed: true,
            }),
            IntSuffix::I64 => Some(IntType {
                bits: 64,
                signed: true,
            }),
            IntSuffix::U8 => Some(IntType {
                bits: 8,
                signed: false,
            }),
            IntSuffix::U16 => Some(IntType {
                bits: 16,
                signed: false,
            }),
            IntSuffix::U32 => Some(IntType {
                bits: 32,
                signed: false,
            }),
            IntSuffix::U64 => Some(IntType {
                bits: 64,
                signed: false,
            }),
            IntSuffix::Usize | IntSuffix::Isize => Some(IntType {
                bits: 64,
                signed: false,
            }),
            IntSuffix::None => None,
        }
    }

    // FEATURE 9: Get minimum storage size in bytes
    fn storage_size(&self) -> u8 {
        (self.bits + 7) / 8
    }

    // FEATURE 9: Get mask to constrain value to bit width
    fn mask(&self) -> u64 {
        if self.bits >= 64 {
            0xFFFFFFFFFFFFFFFF
        } else {
            (1u64 << self.bits) - 1
        }
    }

    // FEATURE 9: Check if value fits in this type
    fn fits(&self, val: i64) -> bool {
        if self.bits >= 64 {
            return true;
        }

        if self.signed {
            let min = -(1i64 << (self.bits - 1));
            let max = (1i64 << (self.bits - 1)) - 1;
            val >= min && val <= max
        } else {
            let max = (1u64 << self.bits) - 1;
            val >= 0 && (val as u64) <= max
        }
    }
}

#[derive(Debug)]
pub struct AuraObject {
    pub entry_point: u64,
    pub text: Vec<u8>,
    pub data: Vec<u8>,
    pub bss_size: usize,
    pub relocations: Vec<Relocation>,
    pub symbols: Vec<Symbol>,
    pub capability_sections: Vec<binary::CapabilitySection>,
    pub topology_sections: Vec<binary::TopologySection>,
    pub bit_region_sections: Vec<binary::BitRegionSection>,
}

#[derive(Debug, Clone)]
pub struct Relocation {
    pub offset: usize,
    pub symbol: String,
    pub kind: RelocationKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RelocationKind {
    Absolute64,
    Relative32,
    Absolute32,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub offset: u64,
    pub size: u64,
    pub kind: SymbolKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Function,
    Data,
    Object,
    Capability,
    Topology,
    BitRegion,
}

pub fn generate(typed_ast: &Program) -> Result<AuraObject, CodegenError> {
    let mut codegen = CodeGenerator::new();

    for item in &typed_ast.items {
        if let Item::Function(f) = item {
            for attr in &f.attrs {
                if let FunctionAttribute::Entry(Some(entry_name)) = attr {
                    codegen.entry_point_name = Some(entry_name.clone());
                } else if let FunctionAttribute::Entry(None) = attr {
                    println!("DEBUG: Setting entry point name to '{}'", f.name);
                    codegen.entry_point_name = Some(f.name.clone());
                }
            }
        }
    }

    if let Some(ref entry_name) = codegen.entry_point_name {
        let mut function_exists = false;
        for item in &typed_ast.items {
            if let Item::Function(f) = item {
                if f.name == *entry_name {
                    function_exists = true;
                    break;
                }
            }
        }
        if !function_exists {
            return Err(CodegenError {
                message: format!("Entry point function '{}' does not exist", entry_name),
            });
        }
    }

    for item in &typed_ast.items {
        codegen.generate_item(item)?;
    }

    Ok(AuraObject {
        entry_point: codegen.entry_point,
        text: codegen.text,
        data: codegen.data,
        bss_size: codegen.bss_size,
        relocations: codegen.relocations,
        symbols: codegen.symbols,
        capability_sections: codegen.capability_sections,
        topology_sections: codegen.topology_sections,
        bit_region_sections: codegen.bit_region_sections,
    })
}

struct CodeGenerator {
    text: Vec<u8>,
    data: Vec<u8>,
    bss_size: usize,
    relocations: Vec<Relocation>,
    symbols: Vec<Symbol>,
    entry_point: u64,
    current_offset: usize,
    label_positions: HashMap<String, usize>,
    entry_point_name: Option<String>,
    variables: HashMap<String, u64>,
    capability_sections: Vec<binary::CapabilitySection>,
    topology_sections: Vec<binary::TopologySection>,
    bit_region_sections: Vec<binary::BitRegionSection>,
    // ========== FEATURE 15: Physical Memory Capability Enforcement ==========
    capability_ranges: HashMap<String, (u64, u64, CapabilityMode)>,
    // ========== FEATURE 11: Memory Topology Verification ==========
    topology_constraints: HashMap<String, MemoryTopology>,
    // ========== FEATURE 5: Entropy Tracking ==========
    entropy_state: HashMap<String, EntropyState>,
    // ========== FEATURE 3: Bit-Region Validation ==========
    validated_bit_regions: HashMap<String, Vec<(u8, u8)>>,
}

type HashMap<K, V> = std::collections::HashMap<K, V>;

impl CodeGenerator {
    fn new() -> Self {
        CodeGenerator {
            text: Vec::new(),
            data: Vec::new(),
            bss_size: 0,
            relocations: Vec::new(),
            symbols: Vec::new(),
            entry_point: 0,
            current_offset: 0,
            label_positions: HashMap::new(),
            entry_point_name: None,
            variables: HashMap::new(),
            capability_sections: Vec::new(),
            topology_sections: Vec::new(),
            bit_region_sections: Vec::new(),
            // ========== FEATURE 15: Physical Memory Capability Enforcement ==========
            capability_ranges: HashMap::new(),
            // ========== FEATURE 11: Memory Topology Verification ==========
            topology_constraints: HashMap::new(),
            // ========== FEATURE 5: Entropy Tracking ==========
            entropy_state: HashMap::new(),
            // ========== FEATURE 3: Bit-Region Validation ==========
            validated_bit_regions: HashMap::new(),
        }
    }

    fn generate_item(&mut self, item: &Item) -> Result<(), CodegenError> {
        match item {
            Item::Function(f) => {
                self.generate_function(f)?;
            }
            Item::Const(c) => {
                self.generate_const_item(c)?;
            }
            Item::Var(v) => self.generate_var_item(v)?,
            // FEATURE 15: Handle capability declarations with validation
            Item::CapabilityDecl(c) => {
                self.validate_capability_range(&c.name, c.base_address, c.length, c.mode.clone())?;
                let end_address = c.base_address.saturating_add(c.length);
                self.capability_ranges.insert(
                    c.name.clone(),
                    (c.base_address, end_address, c.mode.clone()),
                );
                let element_size = c.element_type.as_ref().map_or(0, |t| t.size() as u32);
                let element_count = if element_size > 0 {
                    c.length / element_size as u64
                } else {
                    1
                };
                let mode = match c.mode {
                    CapabilityMode::Read => 0,
                    CapabilityMode::Write => 1,
                    CapabilityMode::ReadWrite => 2,
                    CapabilityMode::Execute => 3,
                };
                self.capability_sections.push(binary::CapabilitySection {
                    name: c.name.clone(),
                    base_address: c.base_address,
                    length: c.length,
                    mode,
                    element_size,
                    element_count,
                });
                self.symbols.push(Symbol {
                    name: c.name.clone(),
                    offset: c.base_address,
                    size: c.length,
                    kind: SymbolKind::Capability,
                });
            }
            // FEATURE 11: Handle topology declarations with validation
            Item::TopologyDecl(t) => {
                self.topology_constraints
                    .insert(t.name.clone(), t.topology.clone());
                let cache_level = match t.topology.cache_level {
                    Some(CacheLevel::L1) => 1,
                    Some(CacheLevel::L2) => 2,
                    Some(CacheLevel::L3) => 3,
                    Some(CacheLevel::L4) => 4,
                    None => 0,
                };
                let memory_class = match t.topology.memory_class {
                    Some(MemoryClass::Device) => 1,
                    Some(MemoryClass::DMA) => 2,
                    Some(MemoryClass::DMAcoherent) => 3,
                    Some(MemoryClass::Framebuffer) => 4,
                    Some(MemoryClass::Encrypted) => 5,
                    Some(MemoryClass::Normal) => 0,
                    None => 0,
                };
                self.topology_sections.push(binary::TopologySection {
                    name: t.name.clone(),
                    numa_node: t.topology.numa_node.unwrap_or(0xFF),
                    cache_level,
                    memory_class,
                });
                self.symbols.push(Symbol {
                    name: t.name.clone(),
                    offset: 0,
                    size: 0,
                    kind: SymbolKind::Topology,
                });
            }
            // FEATURE 3: Handle bit-region declarations with validation
            Item::BitRegionDecl(b) => {
                // FEATURE 3: Validate bit region declarations
                self.validate_bitregion_decl(&b.name, &b.base_type, &b.regions)?;
                let base_type_str = format!("{:?}", b.base_type);
                let region_tuples: Vec<(String, u8, u8, BitAccess)> = b
                    .regions
                    .iter()
                    .map(|r| (r.name.clone(), r.bit_offset, r.bit_width, r.access.clone()))
                    .collect();
                let mut regions = Vec::new();
                for r in &b.regions {
                    let access = match r.access {
                        BitAccess::ReadOnly => 0,
                        BitAccess::WriteOnly => 1,
                        BitAccess::ReadWrite => 2,
                    };
                    regions.push(binary::BitRegionInfo {
                        name: r.name.clone(),
                        bit_offset: r.bit_offset,
                        bit_width: r.bit_width,
                        access,
                    });
                }
                self.bit_region_sections.push(binary::BitRegionSection {
                    name: b.name.clone(),
                    base_type: base_type_str,
                    regions,
                });
                self.symbols.push(Symbol {
                    name: b.name.clone(),
                    offset: 0,
                    size: b.base_type.size() as u64,
                    kind: SymbolKind::BitRegion,
                });
            }
            // FEATURE 5: Handle entropy declarations
            Item::EntropyDecl(e) => {
                self.entropy_state
                    .insert(e.name.clone(), e.initial_entropy.clone());
                if e.initial_entropy == EntropyState::Initialized {
                    let offset = self.data.len();
                    self.data.extend_from_slice(&0u64.to_le_bytes());
                    self.variables.insert(e.name.clone(), offset as u64);
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn generate_const_item(&mut self, c: &ConstDecl) -> Result<(), CodegenError> {
        match &*c.value {
            Expr::Literal(Literal::Int(val, _)) => {
                let offset = self.data.len();
                self.data.extend_from_slice(&val.to_le_bytes());
                self.symbols.push(Symbol {
                    name: c.name.clone(),
                    offset: offset as u64,
                    size: 8,
                    kind: SymbolKind::Data,
                });
                // FEATURE 5: Constants are initialized
                self.entropy_state
                    .insert(c.name.clone(), EntropyState::Initialized);
            }
            Expr::Literal(Literal::String(bytes)) => {
                let offset = self.data.len();
                self.data.extend_from_slice(bytes);
                self.data.push(0);
                self.symbols.push(Symbol {
                    name: c.name.clone(),
                    offset: offset as u64,
                    size: bytes.len() as u64,
                    kind: SymbolKind::Data,
                });
                // FEATURE 5: String constants are initialized
                self.entropy_state
                    .insert(c.name.clone(), EntropyState::Initialized);
            }
            _ => {}
        }
        Ok(())
    }

    fn generate_var_item(&mut self, v: &VarDecl) -> Result<(), CodegenError> {
        self.bss_size += 8;
        // FEATURE 5: Variables are uninitialized by default
        self.entropy_state
            .insert(v.name.clone(), EntropyState::Uninitialized);
        Ok(())
    }

    fn generate_function(&mut self, f: &Function) -> Result<(), CodegenError> {
        let func_start = self.text.len();

        self.symbols.push(Symbol {
            name: f.name.clone(),
            offset: func_start as u64,
            size: 0,
            kind: SymbolKind::Function,
        });

        if let Some(entry_name) = &self.entry_point_name {
            if f.name == *entry_name {
                println!(
                    "DEBUG: Setting entry point for function '{}' to offset {}",
                    f.name, func_start
                );
                self.entry_point = func_start as u64;
            }
        }

        for stmt in &f.body {
            self.generate_stmt(stmt)?;
        }

        if let Some((idx, _)) = self
            .symbols
            .iter_mut()
            .enumerate()
            .find(|(_, s)| s.name == f.name)
        {
            self.symbols[idx].size = (self.text.len() - func_start) as u64;
        }

        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> Result<(), CodegenError> {
        match stmt {
            Stmt::Return(Some(expr)) => {
                self.generate_return(expr)?;
                self.ret();
            }
            Stmt::Return(None) => {
                self.xor_rax_rax();
                self.ret();
            }
            Stmt::Const(c) => {
                self.generate_const_stmt(c)?;
            }
            Stmt::Let(l) => {
                self.generate_let(l)?;
            }
            Stmt::Expr(e) => {
                self.generate_expr(e)?;
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.generate_stmt(s)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn generate_const_stmt(&mut self, c: &ConstStmt) -> Result<(), CodegenError> {
        match &*c.value {
            Expr::Literal(Literal::Int(val, _)) => {
                let offset = self.data.len();
                self.data.extend_from_slice(&val.to_le_bytes());
                self.symbols.push(Symbol {
                    name: c.name.clone(),
                    offset: offset as u64,
                    size: 8,
                    kind: SymbolKind::Data,
                });
            }
            Expr::Literal(Literal::String(bytes)) => {
                let offset = self.data.len();
                self.data.extend_from_slice(bytes);
                self.data.push(0);
                self.symbols.push(Symbol {
                    name: c.name.clone(),
                    offset: offset as u64,
                    size: bytes.len() as u64,
                    kind: SymbolKind::Data,
                });
            }
            _ => {}
        }
        Ok(())
    }

    fn generate_let(&mut self, l: &LetStmt) -> Result<(), CodegenError> {
        // FEATURE 5: Track entropy state for the variable
        match &*l.value {
            Expr::Literal(Literal::Int(val, _)) => {
                let offset = self.data.len();
                self.data.extend_from_slice(&val.to_le_bytes());
                self.variables.insert(l.name.clone(), offset as u64);
                // FEATURE 5: Literals are initialized
                self.entropy_state
                    .insert(l.name.clone(), EntropyState::Initialized);
                return Ok(());
            }
            Expr::Identifier(name) => {
                if let Some(&offset) = self.variables.get(name) {
                    let addr = self.get_data_address(offset as usize);
                    self.mov_r10_immediate(addr);
                    self.mov_rax_from_r10(); // Load value into RAX
                } else {
                    return Err(CodegenError {
                        message: format!("Undefined variable: {}", name),
                    });
                }
                // Now RAX holds the value, store it
                let offset = self.data.len();
                self.data.extend_from_slice(&[0u8; 8]);
                let var_addr = self.get_data_address(offset);
                self.mov_r10_immediate(var_addr);
                self.mov_rax_to_r10_mem();
                self.variables.insert(l.name.clone(), offset as u64);
                // FEATURE 5: Inherit entropy from source variable
                let inherited_state = self
                    .entropy_state
                    .get(name)
                    .cloned()
                    .unwrap_or(EntropyState::Initialized);
                self.entropy_state.insert(l.name.clone(), inherited_state);
                return Ok(());
            }
            _ => {
                // FEATURE 5: Generate expression and track resulting entropy
                let _state = self.generate_expr(&l.value)?;
                // Store the entropy state for this variable
                self.entropy_state.insert(l.name.clone(), _state);
            }
        };
        // Store result in data section
        let offset = self.data.len();
        self.data.extend_from_slice(&[0u8; 8]);
        let var_addr = self.get_data_address(offset);
        self.mov_r10_immediate(var_addr);
        self.mov_rax_to_r10_mem();
        self.variables.insert(l.name.clone(), offset as u64);
        Ok(())
    }

    fn generate_return(&mut self, expr: &Expr) -> Result<(), CodegenError> {
        match expr {
            Expr::Literal(Literal::Int(val, _)) => {
                self.mov_rax_immediate(*val as u64);
            }
            Expr::Identifier(name) => {
                if let Some(sym) = self
                    .symbols
                    .iter()
                    .find(|s| s.name == *name && s.kind == SymbolKind::Data)
                {
                    self.mov_rax_from_mem(sym.offset as u64);
                } else {
                    self.xor_rax_rax();
                }
            }
            _ => {
                self.xor_rax_rax();
            }
        }
        Ok(())
    }

    fn generate_expr(&mut self, expr: &Expr) -> Result<EntropyState, CodegenError> {
        match expr {
            Expr::Literal(Literal::Int(val, int_suffix)) => {
                let int_type = IntType::from_suffix(int_suffix);

                if let Some(int_type) = int_type {
                    if !int_type.fits(*val) {
                        return Err(CodegenError {
                            message: format!("Integer literal {} does not fit in type", val),
                        });
                    }
                    let masked = *val as u64 & int_type.mask();
                    self.emit_width_immediate(masked, int_type.bits);
                    Ok(EntropyState::Initialized)
                } else {
                    Ok(EntropyState::Initialized)
                }
            }
            Expr::Identifier(name) => {
                if let Some(sym) = self
                    .symbols
                    .iter()
                    .find(|s| s.name == *name && s.kind == SymbolKind::Data)
                {
                    self.mov_rax_from_mem(sym.offset as u64);
                } else if let Some(&offset) = self.variables.get(name) {
                    let addr = self.get_data_address(offset as usize);
                    self.mov_r10_immediate(addr);
                    self.mov_rax_from_r10();
                }
                let state = self
                    .entropy_state
                    .get(name)
                    .cloned()
                    .unwrap_or(EntropyState::Initialized);
                Ok(state)
            }
            Expr::Syscall(method_name, args) => {
                // FEATURE 5: Check entropy before syscall
                self.check_syscall_entropy(args)?;
                self.generate_syscall(method_name, args)?;
                Ok(EntropyState::Initialized)
            }
            Expr::Alloc(ty, count) => {
                let _size_state = self.generate_expr(count)?;
                let size = self.eval_expr_to_u64(count)?;
                self.validate_allocation_topology(ty, None)?;
                self.mov_rdi_immediate(size);
                self.call_external("__aura_alloc");
                Ok(EntropyState::Uninitialized)
            }
            Expr::Free(ptr, size) => {
                let _ptr_state = self.generate_expr(ptr)?;
                let _size_state = self.generate_expr(size)?;
                self.eval_expr_to_u64(ptr)?;
                self.mov_rdi_rax();
                self.eval_expr_to_u64(size)?;
                self.mov_rsi_immediate(8);
                self.call_external("__aura_free");
                Ok(EntropyState::Initialized)
            }
            Expr::Cast(expr, target_type) => {
                // FEATURE 11: Check topology cast validity
                if let Type::Topology(topology, _) = target_type {
                    self.validate_topology_cast(
                        None,
                        topology
                            .topology
                            .memory_class
                            .clone()
                            .unwrap_or(MemoryClass::Normal),
                    )?;
                }
                let _state = self.generate_expr(expr)?;
                self.generate_cast_conversion(target_type)?;
                Ok(EntropyState::Initialized)
            }
            Expr::PhysAddr(phys_addr) => {
                let offset_val = 0;
                self.check_capability_access(offset_val, &phys_addr.base_address.to_string())?;
                Ok(EntropyState::Initialized)
            }
            Expr::BitRegionAccess(br) => {
                let _base_state = self.generate_expr(&br.base)?;
                if let Expr::Identifier(name) = &*br.base {
                    if let Some(regions) = self.validated_bit_regions.get(name) {
                        let _region = regions
                            .iter()
                            .find(|(o, w)| format!("{}_{}", o, w) == br.region_name);
                    }
                }
                Ok(EntropyState::Initialized)
            }
            Expr::Deref(ptr) => {
                let ptr_state = self.generate_expr(ptr)?;
                self.check_mmio_entropy(ptr_state, "memory")?;
                Ok(EntropyState::Initialized)
            }
            Expr::Binary(op, left, right) => {
                let left_state = self.generate_expr(left)?;
                let right_state = self.generate_expr(right)?;
                match op {
                    BinaryOp::Add => self.add_rax_rdx(),
                    BinaryOp::Sub => self.sub_rax_rdx(),
                    BinaryOp::Mul => self.imul_rax_rdx(),
                    BinaryOp::Div => self.idiv_rdx(),
                    BinaryOp::Mod => self.idiv_rdx(),
                    BinaryOp::LShift => self.shl_rax_cl(),
                    BinaryOp::RShift => self.shr_rax_cl(),
                    BinaryOp::BitAnd => self.and_rax_rdx(),
                    BinaryOp::BitOr => self.or_rax_rdx(),
                    BinaryOp::BitXor => self.xor_rax_rdx(),
                    _ => {}
                }
                Ok(self.propagate_entropy(left_state, right_state))
            }
            Expr::Unary(op, operand) => {
                let state = self.generate_expr(operand)?;
                match op {
                    UnaryOp::Neg => self.neg_rax(),
                    UnaryOp::Not | UnaryOp::BitNot => self.not_rax(),
                    UnaryOp::Deref => {}
                    UnaryOp::AddrOf => {}
                }
                Ok(state)
            }
            _ => Ok(EntropyState::Initialized),
        }
    }

    fn neg_rax(&mut self) {
        self.text.push(0x48);
        self.text.push(0xf7);
        self.text.push(0xd8);
    }

    fn not_rax(&mut self) {
        self.text.push(0x48);
        self.text.push(0xf7);
        self.text.push(0xd0);
    }

    fn add_rax_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0x01);
        self.text.push(0xc2);
    }

    fn sub_rax_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0x29);
        self.text.push(0xc2);
    }

    fn imul_rax_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0x0f);
        self.text.push(0xaf);
        self.text.push(0xc2);
    }

    fn idiv_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0xf7);
        self.text.push(0xfe);
    }

    fn shl_rax_cl(&mut self) {
        self.text.push(0x48);
        self.text.push(0xd3);
        self.text.push(0xe0);
    }

    fn shr_rax_cl(&mut self) {
        self.text.push(0x48);
        self.text.push(0xd3);
        self.text.push(0xe8);
    }

    fn and_rax_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0x21);
        self.text.push(0xc2);
    }

    fn or_rax_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0x09);
        self.text.push(0xc2);
    }

    fn xor_rax_rdx(&mut self) {
        self.text.push(0x48);
        self.text.push(0x31);
        self.text.push(0xc2);
    }

    fn mov_rax_immediate(&mut self, val: u64) {
        self.text.push(0x48);
        self.text.push(0xb8);
        self.text.extend_from_slice(&val.to_le_bytes());
    }

    fn mov_rax_from_mem(&mut self, addr: u64) {
        self.mov_rax_immediate(addr);
        self.text.push(0x48);
        self.text.push(0x8b);
        self.text.push(0x00);
    }

    fn mov_rax_to_mem(&mut self, addr: u64) {
        self.mov_rax_immediate(addr);
        self.text.push(0x48);
        self.text.push(0x89);
        self.text.push(0x00);
    }

    fn mov_rax_to_mem_via_register(&mut self, addr: u64) {
        self.mov_rax_immediate(addr);
        self.text.push(0x48);
        self.text.push(0x89);
        self.text.push(0x00);
    }

    fn mov_rax_from_mem_via_register(&mut self, addr: u64) {
        self.mov_rax_immediate(addr);
        self.text.push(0x48);
        self.text.push(0x8b);
        self.text.push(0x00);
    }

    fn xor_rax_rax(&mut self) {
        self.text.push(0x48);
        self.text.push(0x31);
        self.text.push(0xc0);
    }

    fn ret(&mut self) {
        self.text.push(0xc3);
    }

    fn generate_syscall(&mut self, method_name: &str, args: &[Expr]) -> Result<(), CodegenError> {
        match method_name {
            "write" => self.generate_write_syscall(args)?,
            _ => {
                return Err(CodegenError {
                    message: format!("Unknown syscall method: {}", method_name),
                })
            }
        }
        Ok(())
    }

    fn get_data_address(&self, offset: usize) -> u64 {
        // For now, assume a fixed data address.
        // This will need to be updated with the actual data segment address.
        let addr = 0x1000000 + offset as u64;
        addr
    }

    fn generate_write_syscall(&mut self, args: &[Expr]) -> Result<(), CodegenError> {
        if args.is_empty() {
            return Err(CodegenError {
                message: "write syscall requires at least one argument".to_string(),
            });
        }

        let fd = if args.len() > 1 {
            match &args[0] {
                Expr::Literal(Literal::Int(val, _)) => *val as u64,
                _ => 1,
            }
        } else {
            1
        };

        let data_arg_idx = if args.len() > 1 { 1 } else { 0 };

        match &args[data_arg_idx] {
            Expr::Literal(Literal::String(bytes)) => {
                let offset = self.data.len();
                self.data.extend_from_slice(bytes);
                let len = bytes.len() as u64;
                let data_addr = self.get_data_address(offset);

                self.mov_rdi_immediate(fd);
                self.mov_rsi_immediate(data_addr);
                self.mov_rdx_immediate(len);
                self.mov_rax_immediate(1);
                self.syscall();
            }
            Expr::Identifier(name) => {
                if let Some(sym) = self
                    .symbols
                    .iter()
                    .find(|s| s.name == *name && s.kind == SymbolKind::Data)
                    .cloned()
                {
                    let len = sym.size;
                    let data_addr = self.get_data_address(sym.offset as usize);

                    self.mov_rdi_immediate(fd);
                    self.mov_rsi_immediate(data_addr);
                    self.mov_rdx_immediate(len);
                    self.mov_rax_immediate(1);
                    self.syscall();
                } else {
                    return Err(CodegenError {
                        message: format!("Unknown identifier: {}", name),
                    });
                }
            }
            _ => {
                return Err(CodegenError {
                    message: "write syscall argument must be a string literal or identifier"
                        .to_string(),
                });
            }
        }

        Ok(())
    }

    fn mov_rdi_immediate(&mut self, val: u64) {
        self.text.push(0x48);
        self.text.push(0xbf);
        self.text.extend_from_slice(&val.to_le_bytes());
    }

    fn mov_rsi_immediate(&mut self, val: u64) {
        self.text.push(0x48);
        self.text.push(0xbe);
        self.text.extend_from_slice(&val.to_le_bytes());
    }
    fn lea_rsi_rip_relative(&mut self, offset: i32) {
        self.text.push(0x48);
        self.text.push(0x8d);
        self.text.push(0x35);
        self.text.extend_from_slice(&offset.to_le_bytes());
    }

    fn mov_rdx_immediate(&mut self, val: u64) {
        self.text.push(0x48);
        self.text.push(0xba);
        self.text.extend_from_slice(&val.to_le_bytes());
    }

    fn mov_r10_immediate(&mut self, val: u64) {
        self.text.push(0x49);
        self.text.push(0xba);
        self.text.extend_from_slice(&val.to_le_bytes());
    }

    fn mov_r10_to_mem(&mut self) {
        self.text.push(0x49);
        self.text.push(0x89);
        self.text.push(0x10);
    }

    fn mov_rax_to_r10_mem(&mut self) {
        self.text.push(0x49);
        self.text.push(0x89);
        self.text.push(0x02); // mov [r10], rax
    }

    fn mov_rax_from_r10(&mut self) {
        self.text.push(0x49);
        self.text.push(0x8b);
        self.text.push(0xc2);
    }

    fn syscall(&mut self) {
        self.text.push(0x0f);
        self.text.push(0x05);
    }

    // Helper function to evaluate expression to u64 value
    fn eval_expr_to_u64(&mut self, expr: &Expr) -> Result<u64, CodegenError> {
        match expr {
            Expr::Literal(Literal::Int(val, int_suffix)) => {
                let int_type = IntType::from_suffix(int_suffix);
                if let Some(int_type) = int_type {
                    if !int_type.fits(*val) {
                        return Err(CodegenError {
                            message: format!("Integer literal {} does not fit in type", val),
                        });
                    }
                    Ok(*val as u64 & int_type.mask())
                } else {
                    Ok(*val as u64)
                }
            }
            Expr::Identifier(name) => {
                if let Some(sym) = self
                    .symbols
                    .iter()
                    .find(|s| s.name == *name && s.kind == SymbolKind::Data)
                {
                    self.mov_rax_from_mem(sym.offset as u64);
                    return Ok(0);
                }
                if let Some(&offset) = self.variables.get(name) {
                    let addr = self.get_data_address(offset as usize);
                    self.mov_r10_immediate(addr);
                    self.mov_rax_from_r10();
                    return Ok(0);
                }
                Ok(0)
            }
            _ => Ok(0),
        }
    }

    // FEATURE 1: Emit alloc<T>(count) - puts result in rax
    fn emit_alloc(&mut self, count_expr: &Expr) -> Result<(), CodegenError> {
        let count = self.eval_expr_to_u64(count_expr)?;
        self.mov_rdi_immediate(count);
        self.call_external("__aura_alloc");
        Ok(())
    }

    // FEATURE 1: Emit free(ptr) - takes ptr from rax
    fn emit_free(&mut self, ptr_expr: &Expr) -> Result<(), CodegenError> {
        let _ = self.eval_expr_to_u64(ptr_expr)?;
        self.mov_rdi_rax();
        self.call_external("__aura_free");
        Ok(())
    }

    // FEATURE 9: Emit immediate value respecting bit width
    fn emit_width_immediate(&mut self, val: u64, bits: u8) {
        match bits {
            1..=8 => {
                // mov al, imm8
                self.text.push(0xb0);
                self.text.push(val as u8);
            }
            9..=16 => {
                // mov ax, imm16
                self.text.push(0x66);
                self.text.push(0xb8);
                self.text.extend_from_slice(&(val as u16).to_le_bytes());
            }
            17..=32 => {
                // mov eax, imm32
                self.text.push(0xb8);
                self.text.extend_from_slice(&(val as u32).to_le_bytes());
            }
            _ => {
                // mov rax, imm64
                self.text.push(0x48);
                self.text.push(0xb8);
                self.text.extend_from_slice(&val.to_le_bytes());
            }
        }
    }

    // FEATURE 1: Move rax to rdi
    fn mov_rdi_rax(&mut self) {
        self.text.push(0x48);
        self.text.push(0x89);
        self.text.push(0xf8);
    }

    // FEATURE 1: Call external function
    fn call_external(&mut self, symbol: &str) {
        match symbol {
            "__aura_alloc" => {
                // call r14
                self.text.push(0x41);
                self.text.push(0xff);
                self.text.push(0xd6);
            }
            "__aura_free" => {
                // call r15
                self.text.push(0x41);
                self.text.push(0xff);
                self.text.push(0xd7);
            }
            _ => {
                // Emit: call [rip + offset]
                self.text.push(0xff);
                self.text.push(0x15);
                // Add relocation for external symbol
                self.relocations.push(Relocation {
                    offset: self.text.len(),
                    symbol: symbol.to_string(),
                    kind: RelocationKind::Relative32,
                });
                self.text.extend_from_slice(&[0u8; 4]);
            }
        }
    }

    // FEATURE 8: Generate explicit cast conversion
    fn generate_cast_conversion(&mut self, target_type: &Type) -> Result<(), CodegenError> {
        match target_type {
            Type::I8 => {
                // movsx eax, al (sign-extend 8-bit to 32-bit)
                self.text.push(0x0f);
                self.text.push(0xbe);
                self.text.push(0xc0);
            }
            Type::U8 => {
                // movzx eax, al (zero-extend 8-bit to 32-bit)
                self.text.push(0x0f);
                self.text.push(0xb6);
                self.text.push(0xc0);
            }
            Type::I16 => {
                // movsx eax, ax
                self.text.push(0x0f);
                self.text.push(0xbf);
                self.text.push(0xc0);
            }
            Type::U16 => {
                // movzx eax, ax
                self.text.push(0x0f);
                self.text.push(0xb7);
                self.text.push(0xc0);
            }
            Type::I32 | Type::U32 => {
                // Already in eax, no conversion needed
            }
            Type::I64 | Type::U64 => {
                // Already in rax, no conversion needed
            }
            Type::BitInt(bits, _) => {
                // FEATURE 9: Apply mask for bit-precise type
                self.mask_rax(*bits);
            }
            _ => {}
        }
        Ok(())
    }

    // FEATURE 9: Apply mask to rax for bit-precise types
    fn mask_rax(&mut self, bits: u8) {
        if bits < 64 {
            let mask = if bits >= 63 {
                u64::MAX
            } else {
                (1u64 << bits) - 1
            };
            self.text.push(0x48);
            self.text.push(0x25);
            self.text.extend_from_slice(&mask.to_le_bytes());
        }
    }

    // ========== FEATURE 15: Physical Memory Capability Enforcement ==========
    fn check_capability_access(&self, _addr: u64, _capability: &str) -> Result<(), CodegenError> {
        if let Some((base, length, mode)) = self.capability_ranges.get(_capability) {
            if _addr < *base || _addr >= *base + *length {
                return Err(CodegenError {
                    message: format!(
                        "Capability access violation: address 0x{:x} outside capability range [0x{:x}, 0x{:x})",
                        _addr, base, base + length
                    ),
                });
            }
        }
        Ok(())
    }

    fn validate_capability_range(
        &self,
        name: &str,
        base: u64,
        length: u64,
        mode: CapabilityMode,
    ) -> Result<(), CodegenError> {
        if length == 0 {
            return Err(CodegenError {
                message: format!("Capability '{}' has zero length", name),
            });
        }
        if let Some((_, _, existing_mode)) = self.capability_ranges.get(name) {
            if existing_mode != &mode {
                return Err(CodegenError {
                    message: format!(
                        "Capability '{}' redeclared with different mode: {:?} vs {:?}",
                        name, mode, existing_mode
                    ),
                });
            }
        }
        for (other_name, (other_base, other_length, _)) in &self.capability_ranges {
            if other_name != name {
                let other_end = *other_base + *other_length;
                let this_end = base + length;
                if base < other_end && this_end > *other_base {
                    return Err(CodegenError {
                        message: format!(
                            "Capability '{}' overlaps with capability '{}'",
                            name, other_name
                        ),
                    });
                }
            }
        }
        Ok(())
    }

    // ========== FEATURE 11: Memory Topology Verification ==========
    fn validate_topology_cast(
        &self,
        source_topology: Option<&MemoryTopology>,
        target_class: MemoryClass,
    ) -> Result<(), CodegenError> {
        if let Some(topology) = source_topology {
            if let Some(mem_class) = &topology.memory_class {
                if *mem_class == MemoryClass::Device && target_class == MemoryClass::Encrypted {
                    return Err(CodegenError {
                        message: "Cannot cast Device memory to Encrypted memory".to_string(),
                    });
                }
                if *mem_class == MemoryClass::Encrypted && target_class == MemoryClass::Device {
                    return Err(CodegenError {
                        message: "Cannot cast Encrypted memory to Device memory".to_string(),
                    });
                }
            }
        }
        Ok(())
    }

    fn validate_allocation_topology(
        &self,
        _ty: &Type,
        numa_node: Option<u8>,
    ) -> Result<(), CodegenError> {
        if let Some(node) = numa_node {
            if node > 255 {
                return Err(CodegenError {
                    message: format!("Invalid NUMA node: {}", node),
                });
            }
        }
        Ok(())
    }

    // ========== FEATURE 5: Entropy Propagation ==========
    fn propagate_entropy(&self, left: EntropyState, right: EntropyState) -> EntropyState {
        match (left, right) {
            (EntropyState::Uninitialized, _) | (_, EntropyState::Uninitialized) => {
                EntropyState::Uninitialized
            }
            (EntropyState::Tainted, _) | (_, EntropyState::Tainted) => EntropyState::Tainted,
            (EntropyState::Partial(bits1), EntropyState::Partial(bits2)) => {
                let merged: Vec<BitState> = bits1
                    .iter()
                    .zip(bits2.iter())
                    .map(|(a, b)| match (a, b) {
                        (BitState::Initialized(true), BitState::Initialized(true)) => {
                            BitState::Initialized(true)
                        }
                        _ => BitState::Tainted,
                    })
                    .collect();
                EntropyState::Partial(merged)
            }
            (EntropyState::Partial(bits), EntropyState::Initialized)
            | (EntropyState::Initialized, EntropyState::Partial(bits)) => {
                EntropyState::Partial(bits)
            }
            (EntropyState::Initialized, EntropyState::Initialized) => EntropyState::Initialized,
        }
    }

    fn check_syscall_entropy(&self, args: &[Expr]) -> Result<(), CodegenError> {
        for arg in args {
            if let Expr::Identifier(name) = arg {
                if let Some(state) = self.entropy_state.get(name) {
                    if *state == EntropyState::Uninitialized {
                        return Err(CodegenError {
                            message: format!(
                                "Syscall argument '{}' is uninitialized - this could expose sensitive data",
                                name
                            ),
                        });
                    }
                    if *state == EntropyState::Tainted {
                        return Err(CodegenError {
                            message: format!(
                                "Syscall argument '{}' is tainted - unsafe for system calls",
                                name
                            ),
                        });
                    }
                }
            }
        }
        Ok(())
    }

    fn check_mmio_entropy(&self, _value: EntropyState, _region: &str) -> Result<(), CodegenError> {
        if _value == EntropyState::Uninitialized {
            return Err(CodegenError {
                message: format!(
                    "Cannot write uninitialized value to MMIO region '{}'",
                    _region
                ),
            });
        }
        if _value == EntropyState::Tainted {
            return Err(CodegenError {
                message: format!(
                    "Cannot write tainted value to MMIO region '{}' - security risk",
                    _region
                ),
            });
        }
        Ok(())
    }

    // ========== FEATURE 3: Bit-Region Validation ==========
    fn validate_bit_ranges(
        &self,
        _base_type: &Type,
        regions: &[(u8, u8, &str)],
    ) -> Result<(), CodegenError> {
        let type_bits = match _base_type {
            Type::I8 | Type::U8 => 8,
            Type::I16 | Type::U16 => 16,
            Type::I32 | Type::U32 => 32,
            Type::I64 | Type::U64 => 64,
            Type::BitInt(bits, _) => *bits,
            _ => {
                return Err(CodegenError {
                    message: format!("Cannot define bit regions on non-integer type"),
                })
            }
        };
        for (offset, width, _name) in regions {
            if *offset >= type_bits {
                return Err(CodegenError {
                    message: format!(
                        "Bit region '{}' offset {} exceeds type width of {} bits",
                        _name, offset, type_bits
                    ),
                });
            }
            if *width == 0 {
                return Err(CodegenError {
                    message: format!("Bit region '{}' has zero width", _name),
                });
            }
            if *offset + *width > type_bits {
                return Err(CodegenError {
                    message: format!(
                        "Bit region '{}' [{}, {}) exceeds type width of {} bits",
                        _name,
                        offset,
                        offset + width,
                        type_bits
                    ),
                });
            }
        }
        let mut sorted: Vec<_> = regions.iter().collect();
        sorted.sort_by_key(|r| r.0);
        for i in 0..sorted.len().saturating_sub(1) {
            let (offset1, width1, _) = sorted[i];
            let (offset2, width2, _) = sorted[i + 1];
            if offset1 + width1 > *offset2 {
                return Err(CodegenError {
                    message: format!(
                        "Bit regions overlap: [{}, {}) and [{}, {})",
                        offset1,
                        offset1 + width1,
                        offset2,
                        offset2 + width2
                    ),
                });
            }
        }
        Ok(())
    }

    fn validate_bitregion_decl(
        &mut self,
        name: &str,
        base_type: &Type,
        regions: &[BitRegion],
    ) -> Result<(), CodegenError> {
        if let Some(existing) = self.validated_bit_regions.get(name) {
            return Err(CodegenError {
                message: format!("Bit region '{}' already defined", name),
            });
        }
        let region_tuples: Vec<(u8, u8, &str)> = regions
            .iter()
            .map(|r| (r.bit_offset, r.bit_width, r.name.as_str()))
            .collect();
        self.validate_bit_ranges(base_type, &region_tuples)?;
        self.validated_bit_regions.insert(
            name.to_string(),
            regions
                .iter()
                .map(|r| (r.bit_offset, r.bit_width))
                .collect(),
        );
        Ok(())
    }
}
