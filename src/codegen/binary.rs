use std::fs::File;
use std::io::Write;

use super::{Relocation, Symbol};

pub fn write_aura_binary(
    object: &super::AuraObject,
    path: &std::path::Path,
) -> std::io::Result<()> {
    let mut file = File::create(path)?;

    let header_size = std::mem::size_of::<AuraBinaryHeader>();
    let text_offset = header_size as u64;
    let aligned_text_size = align_to(object.text.len(), 16);
    let data_offset = text_offset + aligned_text_size as u64;
    let aligned_data_size = align_to(object.data.len(), 16);

    let reloc_offset = data_offset + aligned_data_size as u64;
    let symbol_offset = reloc_offset + object.relocations.len() as u64 * 280;
    let cap_offset = symbol_offset + object.symbols.len() as u64 * 280;
    let topo_offset = cap_offset + object.capability_sections.len() as u64 * 280;
    let br_offset = topo_offset + object.topology_sections.len() as u64 * 280;

    let header = AuraBinaryHeader {
        magic: *b"AURA",
        version: 2,
        flags: 0,
        reserved: 0,
        entry_point: object.entry_point,
        stack_size: 4096,
        text_offset,
        text_size: object.text.len() as u64,
        data_offset,
        data_size: object.data.len() as u64,
        bss_size: object.bss_size as u64,
        reloc_count: object.relocations.len() as u64,
        symbol_count: object.symbols.len() as u64,
        capability_count: object.capability_sections.len() as u64,
        topology_count: object.topology_sections.len() as u64,
        bitregion_count: object.bit_region_sections.len() as u64,
    };

    file.write_all(&header.as_bytes())?;

    let mut written = header_size;
    file.write_all(&object.text)?;
    written += object.text.len();

    if aligned_text_size > object.text.len() {
        file.write_all(&vec![0u8; aligned_text_size - object.text.len()])?;
        written += aligned_text_size - object.text.len();
    }

    file.write_all(&object.data)?;
    written += object.data.len();

    if aligned_data_size > object.data.len() {
        file.write_all(&vec![0u8; aligned_data_size - object.data.len()])?;
        written += aligned_data_size - object.data.len();
    }

    for reloc in &object.relocations {
        let mut bytes = reloc.as_bytes();
        bytes.resize(280, 0);
        file.write_all(&bytes)?;
    }

    for sym in &object.symbols {
        let mut bytes = sym.as_bytes();
        bytes.resize(280, 0);
        file.write_all(&bytes)?;
    }

    for cap in &object.capability_sections {
        let mut bytes = cap.as_bytes();
        bytes.resize(280, 0);
        file.write_all(&bytes)?;
    }

    for topo in &object.topology_sections {
        let mut bytes = topo.as_bytes();
        bytes.resize(280, 0);
        file.write_all(&bytes)?;
    }

    for br in &object.bit_region_sections {
        let mut bytes = br.as_bytes();
        bytes.resize(280, 0);
        file.write_all(&bytes)?;
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
    capability_count: u64,
    topology_count: u64,
    bitregion_count: u64,
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
        bytes.extend_from_slice(&self.capability_count.to_le_bytes());
        bytes.extend_from_slice(&self.topology_count.to_le_bytes());
        bytes.extend_from_slice(&self.bitregion_count.to_le_bytes());
        bytes
    }
}

impl Relocation {
    pub fn as_bytes(&self) -> Vec<u8> {
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
    pub fn as_bytes(&self) -> Vec<u8> {
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

#[derive(Debug, Clone)]
pub struct CapabilitySection {
    pub name: String,
    pub base_address: u64,
    pub length: u64,
    pub mode: u8,
    pub element_size: u32,
    pub element_count: u64,
}

impl CapabilitySection {
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        let name_len = (self.name.len() as u64).to_le_bytes();
        bytes.extend_from_slice(&name_len);
        bytes.extend_from_slice(self.name.as_bytes());
        bytes.push(0);
        bytes.extend_from_slice(&self.base_address.to_le_bytes());
        bytes.extend_from_slice(&self.length.to_le_bytes());
        bytes.push(self.mode);
        bytes.extend_from_slice(&self.element_size.to_le_bytes());
        bytes.extend_from_slice(&self.element_count.to_le_bytes());
        bytes
    }
}

#[derive(Debug, Clone)]
pub struct TopologySection {
    pub name: String,
    pub numa_node: u8,
    pub cache_level: u8,
    pub memory_class: u8,
}

impl TopologySection {
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        let name_len = (self.name.len() as u64).to_le_bytes();
        bytes.extend_from_slice(&name_len);
        bytes.extend_from_slice(self.name.as_bytes());
        bytes.push(0);
        bytes.push(self.numa_node);
        bytes.push(self.cache_level);
        bytes.push(self.memory_class);
        bytes
    }
}

#[derive(Debug, Clone)]
pub struct BitRegionSection {
    pub name: String,
    pub base_type: String,
    pub regions: Vec<BitRegionInfo>,
}

#[derive(Debug, Clone)]
pub struct BitRegionInfo {
    pub name: String,
    pub bit_offset: u8,
    pub bit_width: u8,
    pub access: u8,
}

impl BitRegionSection {
    pub fn as_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        let name_len = (self.name.len() as u64).to_le_bytes();
        bytes.extend_from_slice(&name_len);
        bytes.extend_from_slice(self.name.as_bytes());
        bytes.push(0);

        let base_type_len = (self.base_type.len() as u64).to_le_bytes();
        bytes.extend_from_slice(&base_type_len);
        bytes.extend_from_slice(self.base_type.as_bytes());
        bytes.push(0);

        bytes.extend_from_slice(&(self.regions.len() as u64).to_le_bytes());

        for region in &self.regions {
            let region_name_len = (region.name.len() as u64).to_le_bytes();
            bytes.extend_from_slice(&region_name_len);
            bytes.extend_from_slice(region.name.as_bytes());
            bytes.push(0);
            bytes.push(region.bit_offset);
            bytes.push(region.bit_width);
            bytes.push(region.access);
        }

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
        println!("Capabilities: {}", header.capability_count);
        println!("Topology Sections: {}", header.topology_count);
        println!("Bit Region Sections: {}", header.bitregion_count);

        let text_start = header.text_offset as usize;
        let text_end = text_start + header.text_size as usize;
        if text_end <= data.len() && header.text_size > 0 {
            println!("\n=== Text Section ({} bytes) ===", header.text_size);
            Self::print_hex(&data[text_start..text_end]);
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
        pos += 8;

        let mut capability_count = [0u8; 8];
        capability_count.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut topology_count = [0u8; 8];
        topology_count.copy_from_slice(&data[pos..pos + 8]);
        pos += 8;

        let mut bitregion_count = [0u8; 8];
        bitregion_count.copy_from_slice(&data[pos..pos + 8]);

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
            capability_count: u64::from_le_bytes(capability_count),
            topology_count: u64::from_le_bytes(topology_count),
            bitregion_count: u64::from_le_bytes(bitregion_count),
        }
    }
}
