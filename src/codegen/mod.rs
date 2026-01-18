use crate::ast::*;
use std::fmt;
use std::fs::File;
use std::io::Write;

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
}

pub fn generate(typed_ast: &Program) -> Result<AuraObject, CodegenError> {
    let mut codegen = CodeGenerator::new();

    for item in &typed_ast.items {
        if let Item::Function(f) = item {
            for attr in &f.attrs {
                if let FunctionAttribute::Entry(Some(entry_name)) = attr {
                    codegen.entry_point_name = Some(entry_name.clone());
                } else if let FunctionAttribute::Entry(None) = attr {
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

    fn generate_var_item(&mut self, _v: &VarDecl) -> Result<(), CodegenError> {
        self.bss_size += 8;
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
        match &*l.value {
            Expr::Literal(Literal::Int(val, _)) => {
                self.mov_rax_immediate(*val as u64);
            }
            Expr::Identifier(name) => {
                if let Some(sym) = self
                    .symbols
                    .iter()
                    .find(|s| s.name == *name && s.kind == SymbolKind::Data)
                {
                    self.mov_rax_from_mem(sym.offset as usize);
                }
            }
            _ => {}
        }
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
                    self.mov_rax_from_mem(sym.offset as usize);
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

    fn generate_expr(&mut self, expr: &Expr) -> Result<u64, CodegenError> {
        match expr {
            Expr::Literal(Literal::Int(val, _)) => Ok(*val as u64),
            Expr::Identifier(name) => {
                if let Some(sym) = self
                    .symbols
                    .iter()
                    .find(|s| s.name == *name && s.kind == SymbolKind::Data)
                {
                    self.mov_rax_from_mem(sym.offset as usize);
                    return Ok(0);
                }
                Ok(0)
            }
            Expr::Syscall(method_name, args) => {
                self.generate_syscall(method_name, args)?;
                Ok(0)
            }
            _ => Ok(0),
        }
    }

    fn mov_rax_immediate(&mut self, val: u64) {
        self.text.push(0x48);
        self.text.push(0xb8);
        self.text.extend_from_slice(&val.to_le_bytes());
    }

    fn mov_rax_from_mem(&mut self, offset: usize) {
        self.text.push(0x48);
        self.text.push(0xa1);
        self.text.extend_from_slice(&offset.to_le_bytes());
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
}
impl CodeGenerator {
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

    fn syscall(&mut self) {
        self.text.push(0x0f);
        self.text.push(0x05);
    }

    fn get_data_address(&self, offset: usize) -> u64 {
        0x1000000 + offset as u64
    }
}

pub fn write_aura_binary(object: &AuraObject, path: &std::path::Path) -> std::io::Result<()> {
    let mut file = File::create(path)?;

    let header = AuraBinaryHeader {
        magic: *b"AURA",
        version: 1,
        flags: 0,
        reserved: 0,
        entry_point: object.entry_point,
        stack_size: 4096,
        text_offset: std::mem::size_of::<AuraBinaryHeader>() as u64,
        text_size: object.text.len() as u64,
        data_offset: (std::mem::size_of::<AuraBinaryHeader>() + align_to(object.text.len(), 16))
            as u64,
        data_size: object.data.len() as u64,
        bss_size: object.bss_size as u64,
        reloc_count: object.relocations.len() as u64,
        symbol_count: object.symbols.len() as u64,
    };

    file.write_all(&header.as_bytes())?;
    file.write_all(&object.text)?;

    let text_pad = align_to(object.text.len(), 16) - object.text.len();
    if text_pad > 0 {
        file.write_all(&vec![0u8; text_pad])?;
    }

    file.write_all(&object.data)?;

    let data_pad = align_to(object.data.len(), 16) - object.data.len();
    if data_pad > 0 {
        file.write_all(&vec![0u8; data_pad])?;
    }

    for reloc in &object.relocations {
        file.write_all(&reloc.as_bytes())?;
    }

    for sym in &object.symbols {
        file.write_all(&sym.as_bytes())?;
    }

    Ok(())
}

fn align_to(size: usize, align: usize) -> usize {
    if align == 0 {
        size
    } else {
        ((size + align - 1) / align) * align
    }
}

#[repr(C)]
struct AuraBinaryHeader {
    magic: [u8; 4],
    version: u8,
    flags: u8,
    reserved: u16,
    entry_point: u64,
    stack_size: u64,
    text_offset: u64,
    text_size: u64,
    data_offset: u64,
    data_size: u64,
    bss_size: u64,
    reloc_count: u64,
    symbol_count: u64,
}

impl AuraBinaryHeader {
    fn as_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.magic);
        bytes.push(self.version);
        bytes.push(self.flags);
        bytes.extend_from_slice(&self.reserved.to_le_bytes());
        bytes.extend_from_slice(&self.entry_point.to_le_bytes());
        bytes.extend_from_slice(&self.stack_size.to_le_bytes());
        bytes.extend_from_slice(&self.text_offset.to_le_bytes());
        bytes.extend_from_slice(&self.text_size.to_le_bytes());
        bytes.extend_from_slice(&self.data_offset.to_le_bytes());
        bytes.extend_from_slice(&self.data_size.to_le_bytes());
        bytes.extend_from_slice(&self.bss_size.to_le_bytes());
        bytes.extend_from_slice(&self.reloc_count.to_le_bytes());
        bytes.extend_from_slice(&self.symbol_count.to_le_bytes());
        bytes
    }
}

impl Relocation {
    fn as_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.offset.to_le_bytes());
        let sym_len = (self.symbol.len() as u64).to_le_bytes();
        bytes.extend_from_slice(&sym_len);
        bytes.extend_from_slice(self.symbol.as_bytes());
        bytes.push(0);
        bytes.push(self.kind.clone() as u8);
        bytes
    }
}

impl Symbol {
    fn as_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        let name_len = (self.name.len() as u64).to_le_bytes();
        bytes.extend_from_slice(&name_len);
        bytes.extend_from_slice(self.name.as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(&self.offset.to_le_bytes());
        bytes.extend_from_slice(&self.size.to_le_bytes());
        bytes.push(self.kind.clone() as u8);
        bytes
    }
}

pub struct AuraBinary;

impl AuraBinary {
    pub fn dump(data: &[u8]) -> std::io::Result<()> {
        if data.len() < std::mem::size_of::<AuraBinaryHeader>() {
            eprintln!("File too small for header");
            return Ok(());
        }

        let header = AuraBinaryHeader::from_bytes(data);

        println!("=== Aura Binary Dump ===");
        println!(
            "Magic: {}",
            std::str::from_utf8(&header.magic).unwrap_or("INVALID")
        );
        println!("Version: {}", header.version);
        println!("Entry Point: 0x{:016x}", header.entry_point);
        println!("Stack Size: {}", header.stack_size);
        println!(
            "Text Offset: {}, Size: {}",
            header.text_offset, header.text_size
        );
        println!(
            "Data Offset: {}, Size: {}",
            header.data_offset, header.data_size
        );
        println!("BSS Size: {}", header.bss_size);
        println!("Relocations: {}", header.reloc_count);
        println!("Symbols: {}", header.symbol_count);

        let text_start = header.text_offset as usize;
        let text_end = text_start + header.text_size as usize;
        if text_end <= data.len() && header.text_size > 0 {
            println!("\n=== Text Section ({} bytes) ===", header.text_size);
            Self::print_hex(&data[text_start..text_end]);
        }

        let data_start = header.data_offset as usize;
        let data_end = data_start + header.data_size as usize;
        if data_end <= data.len() && header.data_size > 0 {
            println!("\n=== Data Section ({} bytes) ===", header.data_size);
            Self::print_hex(&data[data_start..data_end]);
        }

        Ok(())
    }

    fn print_hex(data: &[u8]) {
        for (i, chunk) in data.chunks(16).enumerate() {
            let offset = i * 16;
            let hex: Vec<String> = chunk.iter().map(|b| format!("{:02x}", b)).collect();
            println!("{:08x}: {:<48}", offset, hex.join(" "));
        }
    }
}

impl AuraBinaryHeader {
    fn from_bytes(data: &[u8]) -> Self {
        let mut pos = 0;
        let mut magic = [0u8; 4];
        magic.copy_from_slice(&data[0..4]);
        pos += 4;

        let version = data[pos];
        pos += 1;

        let flags = data[pos];
        pos += 1;

        let mut reserved = [0u8; 2];
        reserved.copy_from_slice(&data[pos..pos + 2]);
        pos += 2;

        let mut entry_point = [0u8; 8];
        entry_point.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut stack_size = [0u8; 8];
        stack_size.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut text_offset = [0u8; 8];
        text_offset.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut text_size = [0u8; 8];
        text_size.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut data_offset = [0u8; 8];
        data_offset.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut data_size = [0u8; 8];
        data_size.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut bss_size = [0u8; 8];
        bss_size.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut reloc_count = [0u8; 8];
        reloc_count.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut symbol_count = [0u8; 8];
        symbol_count.copy_from_slice(&data[pos..pos + 8]);

        AuraBinaryHeader {
            magic,
            version,
            flags,
            reserved: u16::from_le_bytes(reserved),
            entry_point: u64::from_le_bytes(entry_point),
            stack_size: u64::from_le_bytes(stack_size),
            text_offset: u64::from_le_bytes(text_offset),
            text_size: u64::from_le_bytes(text_size),
            data_offset: u64::from_le_bytes(data_offset),
            data_size: u64::from_le_bytes(data_size),
            bss_size: u64::from_le_bytes(bss_size),
            reloc_count: u64::from_le_bytes(reloc_count),
            symbol_count: u64::from_le_bytes(symbol_count),
        }
    }
}
