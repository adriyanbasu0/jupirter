#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(Struct),
    Union(Union),
    Enum(Enum),
    Const(ConstDecl),
    Var(VarDecl),
    CapabilityDecl(CapabilityDecl),
    TopologyDecl(TopologyDecl),
    BitRegionDecl(BitRegionDecl),
    EntropyDecl(EntropyDecl),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Box<Type>,
    pub body: Vec<Stmt>,
    pub attrs: Vec<FunctionAttribute>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionAttribute {
    Noreturn,
    CCall,
    StdCall,
    Inline,
    Entry(Option<String>),
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<StructField>,
    pub topology: Option<MemoryTopology>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: String,
    pub variants: Vec<UnionVariant>,
}

#[derive(Debug, Clone)]
pub struct UnionVariant {
    pub name: String,
    pub ty: Box<Type>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<i64>,
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
    pub name: String,
    pub ty: Option<Box<Type>>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub ty: Option<Box<Type>>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct CapabilityDecl {
    pub name: String,
    pub base_address: u64,
    pub length: u64,
    pub mode: CapabilityMode,
    pub element_type: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CapabilityMode {
    Read,
    Write,
    ReadWrite,
    Execute,
}

#[derive(Debug, Clone)]
pub struct TopologyDecl {
    pub name: String,
    pub topology: MemoryTopology,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemoryTopology {
    pub numa_node: Option<u8>,
    pub cache_level: Option<CacheLevel>,
    pub memory_class: Option<MemoryClass>,
    pub coherency: Option<CoherencyType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CacheLevel {
    L1,
    L2,
    L3,
    L4,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MemoryClass {
    Device,
    DMA,
    DMAcoherent,
    Framebuffer,
    Encrypted,
    Normal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CoherencyType {
    Local,
    RemoteValid,
    Invalidated,
}

#[derive(Debug, Clone)]
pub struct BitRegionDecl {
    pub name: String,
    pub base_type: Box<Type>,
    pub regions: Vec<BitRegion>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BitRegion {
    pub name: String,
    pub bit_offset: u8,
    pub bit_width: u8,
    pub access: BitAccess,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitAccess {
    ReadOnly,
    WriteOnly,
    ReadWrite,
}

#[derive(Debug, Clone)]
pub struct EntropyDecl {
    pub name: String,
    pub ty: Box<Type>,
    pub initial_entropy: EntropyState,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EntropyState {
    Uninitialized,
    Initialized,
    Tainted,
    Partial(Vec<BitState>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BitState {
    Initialized(bool),
    Uninitialized,
    Tainted,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(LetStmt),
    Const(ConstStmt),
    Expr(Expr),
    Return(Option<Expr>),
    Break,
    Continue,
    Block(Vec<Stmt>),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Asm(AsmStmt),
    Defer(Box<Stmt>),
    EntropyAssert(EntropyAssertStmt),
}

#[derive(Debug, Clone)]
pub struct EntropyAssertStmt {
    pub expr: Box<Expr>,
    pub expected: EntropyState,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: String,
    pub ty: Option<Box<Type>>,
    pub value: Box<Expr>,
    pub is_const: bool,
}

#[derive(Debug, Clone)]
pub struct ConstStmt {
    pub name: String,
    pub ty: Option<Box<Type>>,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub then_branch: Vec<Stmt>,
    pub else_branch: Option<Vec<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Box<Stmt>,
    pub condition: Box<Expr>,
    pub update: Box<Stmt>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct AsmStmt {
    pub template: String,
    pub inputs: Vec<AsmOperand>,
    pub outputs: Vec<AsmOperand>,
    pub clobbers: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct AsmOperand {
    pub constraint: String,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Syscall(String, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Field(Box<Expr>, String),
    PtrField(Box<Expr>, String),
    Cast(Box<Expr>, Type),
    Sizeof(Type),
    Alignof(Type),
    Offsetof(Type, String),
    Assign(Box<Expr>, Box<Expr>),
    AddrOf(Box<Expr>),
    Deref(Box<Expr>),
    Block(Vec<Stmt>, Option<Box<Expr>>),
    If(Box<IfExpr>),
    Alloc(Box<Type>, Box<Expr>),
    Free(Box<Expr>, Box<Expr>),
    PhysAddr(PhysAddrExpr),
    EntropyCheck(EntropyCheckExpr),
    BitRegionAccess(BitRegionAccessExpr),
    TopologyCast(TopologyCastExpr),
}

#[derive(Debug, Clone)]
pub struct PhysAddrExpr {
    pub base_address: u64,
    pub length: u64,
    pub mode: CapabilityMode,
    pub element_type: Option<Box<Type>>,
}

#[derive(Debug, Clone)]
pub struct EntropyCheckExpr {
    pub expr: Box<Expr>,
    pub expected: EntropyState,
}

#[derive(Debug, Clone)]
pub struct BitRegionAccessExpr {
    pub base: Box<Expr>,
    pub region_name: String,
    pub is_write: bool,
}

#[derive(Debug, Clone)]
pub struct TopologyCastExpr {
    pub expr: Box<Expr>,
    pub target_topology: MemoryTopology,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
    BitNot,
    Deref,
    AddrOf,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LShift,
    RShift,
    BitAnd,
    BitOr,
    BitXor,
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64, IntSuffix),
    Float(f64, FloatSuffix),
    Bool(bool),
    String(Vec<u8>),
    Char(u8),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum IntSuffix {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Usize,
    Isize,
    None,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FloatSuffix {
    F32,
    F64,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Usize,
    Isize,
    BitInt(u8, bool),
    Ptr(Box<Type>),
    MutPtr(Box<Type>),
    ConstPtr(Box<Type>),
    PhysAddr(PhysAddrType),
    Array(usize, Box<Type>),
    Func(Vec<Type>, Box<Type>),
    Entropy(EntropyType),
    BitRegion(BitRegionType),
    Topology(TopologyType, Box<Type>),
    Named(String),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PhysAddrType {
    pub base_address: u64,
    pub length: u64,
    pub mode: CapabilityMode,
    pub element_type: Option<Box<Type>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EntropyType {
    pub base_type: Box<Type>,
    pub state: EntropyState,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BitRegionType {
    pub base_type: Box<Type>,
    pub regions: Vec<BitRegion>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TopologyType {
    pub topology: MemoryTopology,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Bool => 1,
            Type::I8 | Type::U8 => 1,
            Type::I16 | Type::U16 => 2,
            Type::I32 | Type::U32 | Type::F32 => 4,
            Type::I64 | Type::U64 | Type::F64 | Type::Usize | Type::Isize => 8,
            Type::BitInt(bits, _) => {
                if *bits <= 8 {
                    1
                } else if *bits <= 16 {
                    2
                } else if *bits <= 32 {
                    4
                } else if *bits <= 64 {
                    8
                } else if *bits <= 128 {
                    16
                } else {
                    (*bits as usize + 7) / 8
                }
            }
            Type::Ptr(_) | Type::MutPtr(_) | Type::ConstPtr(_) => 8,
            Type::PhysAddr(p) => {
                if let Some(elem) = &p.element_type {
                    let elem_size = elem.size();
                    let count = p.length / elem_size as u64;
                    count as usize * elem_size
                } else {
                    8 // Base pointer size
                }
            }
            Type::Array(n, t) => *n * t.size(),
            Type::Func(_, _) => 8,
            Type::Entropy(e) => e.base_type.size(),
            Type::BitRegion(b) => b.base_type.size(),
            Type::Topology(_, t) => t.size(),
            Type::Named(_) => 0,
            Type::Error => 0,
        }
    }

    pub fn align(&self) -> usize {
        match self {
            Type::Void => 1,
            Type::Bool => 1,
            Type::I8 | Type::U8 => 1,
            Type::I16 | Type::U16 => 2,
            Type::I32 | Type::U32 | Type::F32 => 4,
            Type::I64 | Type::U64 | Type::F64 | Type::Usize | Type::Isize => 8,
            Type::BitInt(bits, _) => {
                if *bits <= 8 {
                    1
                } else if *bits <= 16 {
                    2
                } else if *bits <= 32 {
                    4
                } else if *bits <= 64 {
                    8
                } else if *bits <= 128 {
                    8
                } else {
                    16
                }
            }
            Type::Ptr(_) | Type::MutPtr(_) | Type::ConstPtr(_) => 8,
            Type::PhysAddr(p) => {
                if let Some(elem) = &p.element_type {
                    elem.align()
                } else {
                    8
                }
            }
            Type::Array(_, t) => t.align(),
            Type::Func(_, _) => 1,
            Type::Entropy(e) => e.base_type.align(),
            Type::BitRegion(b) => b.base_type.align(),
            Type::Topology(_, t) => t.align(),
            Type::Named(_) => 1,
            Type::Error => 1,
        }
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::I8
                | Type::I16
                | Type::I32
                | Type::I64
                | Type::U8
                | Type::U16
                | Type::U32
                | Type::U64
                | Type::Usize
                | Type::Isize
                | Type::BitInt(_, _)
        )
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Ptr(_) | Type::MutPtr(_) | Type::ConstPtr(_))
    }

    pub fn is_physical_capability(&self) -> bool {
        matches!(self, Type::PhysAddr(_))
    }

    pub fn is_entropy_type(&self) -> bool {
        matches!(self, Type::Entropy(_))
    }

    pub fn is_bit_region_type(&self) -> bool {
        matches!(self, Type::BitRegion(_))
    }
}
