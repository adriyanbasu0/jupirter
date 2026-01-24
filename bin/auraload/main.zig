const std = @import("std");
const posix = std.posix;
const mem = std.mem;

const AURA_VERSION = 2;

const AuraHeaderV2 = extern struct {
    magic: [4]u8,
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
};

extern fn trampoline(entry: *anyopaque, stack: *anyopaque, alloc_fn: *const fn (usize) callconv(.c) ?*anyopaque, free_fn: *const fn (*anyopaque, usize) callconv(.c) void) void;

fn trampoline_alloc(size: usize) callconv(.c) ?*anyopaque {
    const result = posix.mmap(null, size, posix.PROT.READ | posix.PROT.WRITE, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0) catch null;
    if (result) |r| return r.ptr;
    return null;
}

fn trampoline_free(ptr: *anyopaque, size: usize) callconv(.c) void {
    const slice = @as([*]align(4096) u8, @ptrCast(@alignCast(ptr)))[0..size];
    posix.munmap(slice);
}

fn alignUp(ptr: usize, alignment: usize) usize {
    return (ptr + alignment - 1) & ~(alignment - 1);
}

fn read_u64(fd: posix.fd_t, offset: usize) !u64 {
    var val: u64 = 0;
    const amt = try posix.pread(fd, mem.asBytes(&val), offset);
    if (amt != @sizeOf(u64)) return error.ReadFailed;
    return val;
}

fn read_u8(fd: posix.fd_t, offset: usize) !u8 {
    var val: u8 = 0;
    const amt = try posix.pread(fd, mem.asBytes(&val), offset);
    if (amt != @sizeOf(u8)) return error.ReadFailed;
    return val;
}

fn read_bytes(fd: posix.fd_t, buf: []u8, offset: usize) !void {
    const amt = try posix.pread(fd, buf, offset);
    if (amt != buf.len) return error.ReadFailed;
}

const OutStream = struct {
    file: std.fs.File,
    buffer: []u8,
    pos: usize = 0,

    fn flush(self: *OutStream) !void {
        if (self.pos > 0) {
            try self.file.writeAll(self.buffer[0..self.pos]);
            self.pos = 0;
        }
    }

    fn write(self: *OutStream, bytes: []const u8) !void {
        if (bytes.len + self.pos > self.buffer.len) {
            try self.flush();
            if (bytes.len > self.buffer.len) {
                try self.file.writeAll(bytes);
                return;
            }
        }
        @memcpy(self.buffer[self.pos..][0..bytes.len], bytes);
        self.pos += bytes.len;
    }

    fn print(self: *OutStream, comptime fmt: []const u8, args: anytype) !void {
        const msg = std.fmt.allocPrint(std.heap.page_allocator, fmt, args) catch return error.OutOfMemory;
        defer std.heap.page_allocator.free(msg);
        try self.write(msg);
    }
};

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <program.aura>\n", .{args[0]});
        return error.InvalidArgs;
    }

    const filename = args[1];
    const fd = try posix.open(filename, .{}, 0o644);
    defer posix.close(fd);

    var header: AuraHeaderV2 = undefined;
    const amt = try posix.pread(fd, mem.asBytes(&header), 0);
    if (amt != @sizeOf(AuraHeaderV2)) {
        std.debug.print("Error: Cannot read header\n", .{});
        return error.ReadFailed;
    }

    if (!mem.eql(u8, &header.magic, "AURA")) {
        std.debug.print("Error: Invalid magic number\n", .{});
        return error.InvalidMagic;
    }

    if (header.version == 1) {
        std.debug.print("Error: Version 1 binaries not supported by this loader\n", .{});
        return error.UnsupportedVersion;
    }

    if (header.version != AURA_VERSION) {
        std.debug.print("Error: Unsupported version: {d} (expected {d})\n", .{ header.version, AURA_VERSION });
        return error.UnsupportedVersion;
    }

    var out_buf: [8192]u8 = undefined;
    var out = OutStream{ .file = std.fs.File.stdout(), .buffer = &out_buf };

    try out.write("=== Aura Binary Loader ===\n");
    try out.print("Version: {d} | Entry: 0x{x} | Stack: {d} bytes\n", .{ header.version, header.entry_point, header.stack_size });
    try out.print("Text: {d} bytes | Data: {d} bytes | BSS: {d} bytes\n", .{ header.text_size, header.data_size, header.bss_size });
    try out.flush();

    const text_offset = @sizeOf(AuraHeaderV2);
    const data_offset = text_offset + alignUp(header.text_size, 16);
    const data_size_aligned = alignUp(header.data_size, 16);

    const reloc_offset = data_offset + data_size_aligned;
    const symbol_offset = reloc_offset + header.reloc_count * 280;
    const capability_offset = symbol_offset + header.symbol_count * 280;
    const topology_offset = capability_offset + header.capability_count * 280;
    const bitregion_offset = topology_offset + header.topology_count * 280;

    try out.write("\n=== Physical Memory Capability Bounds ===\n");
    try out.flush();

    var i: u64 = 0;
    while (i < header.capability_count) : (i += 1) {
        const base_offset = capability_offset + (i * 280);
        const name_len = try read_u64(fd, base_offset);
        const clamped_name_len = if (name_len > 256) 256 else name_len;
        var name: [256]u8 = undefined;
        try read_bytes(fd, name[0..clamped_name_len], base_offset + 8);
        name[clamped_name_len] = 0;

        const base_address = try read_u64(fd, base_offset + 8 + clamped_name_len + 1);
        const length = try read_u64(fd, base_offset + 8 + clamped_name_len + 1 + 8);
        const mode = try read_u8(fd, base_offset + 8 + clamped_name_len + 1 + 8 + 8);

        const end_address = base_address + length - 1;
        try out.print("  [CAPABILITY] {s}\n", .{name[0..clamped_name_len]});
        try out.print("    Address Bounds: 0x{x:016} - 0x{x:016}\n", .{ base_address, end_address });
        try out.print("    Size: {d} bytes (0x{x})\n", .{ length, length });
        const mode_str = [_][]const u8{ "read-only", "write-only", "read-write", "execute-only" };
        try out.print("    Access Mode: {s}\n", .{mode_str[mode % 4]});
        try out.flush();
    }

    if (header.capability_count == 0) {
        try out.write("  (none defined)\n");
        try out.flush();
    }

    try out.write("\n=== Memory Topology Regions ===\n");
    try out.flush();

    i = 0;
    while (i < header.topology_count) : (i += 1) {
        const base_offset = topology_offset + (i * 280);
        const name_len = try read_u64(fd, base_offset);
        const clamped_name_len = if (name_len > 256) 256 else name_len;
        var name: [256]u8 = undefined;
        try read_bytes(fd, name[0..clamped_name_len], base_offset + 8);
        name[clamped_name_len] = 0;

        const numa_node = try read_u8(fd, base_offset + 8 + clamped_name_len + 1);
        const cache_level = try read_u8(fd, base_offset + 8 + clamped_name_len + 1 + 1);
        const memory_class = try read_u8(fd, base_offset + 8 + clamped_name_len + 1 + 1 + 1);

        try out.print("  [TOPOLOGY] {s}\n", .{name[0..clamped_name_len]});
        const display_numa_node = if (numa_node == 0xFF) 255 else numa_node;
        try out.print("    NUMA Node: {d}\n", .{display_numa_node});
        try out.print("    Cache Level: L{d}\n", .{cache_level});
        const mem_class = [_][]const u8{ "Normal", "Device", "DMA", "DMA-coherent", "Framebuffer", "Encrypted" };
        if (memory_class < 6) {
            try out.print("    Memory Class: {s}\n", .{mem_class[memory_class]});
        } else {
            try out.write("    Memory Class: Unknown\n");
        }
        try out.flush();
    }

    if (header.topology_count == 0) {
        try out.write("  (none defined)\n");
        try out.flush();
    }

    try out.write("\n=== Bit-Region Type Definitions ===\n");
    try out.flush();

    i = 0;
    while (i < header.bitregion_count) : (i += 1) {
        const base_offset = bitregion_offset + (i * 280);
        const name_len = try read_u64(fd, base_offset);
        const clamped_name_len = if (name_len > 256) 256 else name_len;
        var name: [256]u8 = undefined;
        try read_bytes(fd, name[0..clamped_name_len], base_offset + 8);
        name[clamped_name_len] = 0;

        const base_type_len = try read_u64(fd, base_offset + 8 + clamped_name_len + 1);
        const clamped_base_type_len = if (base_type_len > 64) 64 else base_type_len;
        var base_type: [64]u8 = undefined;
        try read_bytes(fd, base_type[0..clamped_base_type_len], base_offset + 8 + clamped_name_len + 1 + 8);
        base_type[clamped_base_type_len] = 0;

        const region_count = try read_u64(fd, base_offset + 8 + clamped_name_len + 1 + 8 + clamped_base_type_len + 1);

        try out.print("  [BITREGION] {s} : {s}\n", .{ name[0..clamped_name_len], base_type[0..clamped_base_type_len] });
        try out.print("    Regions Defined: {d}\n", .{region_count});
        try out.flush();

        var region_offset = base_offset + 8 + clamped_name_len + 1 + 8 + clamped_base_type_len + 1 + 8;
        var j: u64 = 0;
        while (j < region_count) : (j += 1) {
            const region_name_len = try read_u64(fd, region_offset);
            const clamped_region_name_len = if (region_name_len > 256) 256 else region_name_len;
            var region_name: [256]u8 = undefined;
            try read_bytes(fd, region_name[0..clamped_region_name_len], region_offset + 8);
            region_name[clamped_region_name_len] = 0;

            const bit_offset = try read_u8(fd, region_offset + 8 + clamped_region_name_len + 1);
            const bit_width = try read_u8(fd, region_offset + 8 + clamped_region_name_len + 1 + 1);
            const access = try read_u8(fd, region_offset + 8 + clamped_region_name_len + 1 + 1 + 1);

            const access_str = [_][]const u8{ "read-only", "write-only", "read-write" };
            try out.print("      {s}: bits[{d}..{d}] {s}\n", .{ region_name[0..clamped_region_name_len], bit_offset, bit_offset + bit_width - 1, access_str[access % 3] });

            region_offset += 8 + clamped_region_name_len + 1 + 1 + 1 + 1;
        }
        try out.flush();
    }

    if (header.bitregion_count == 0) {
        try out.write("  (none defined)\n");
        try out.flush();
    }

    const page_size = std.os.linux.getauxval(std.elf.AT_PAGESZ);
    const total_size = header.text_size + header.data_size + page_size;

    const code_slice = posix.mmap(null, total_size, posix.PROT.READ | posix.PROT.WRITE | posix.PROT.EXEC, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0) catch {
        std.debug.print("Error: Cannot allocate memory\n", .{});
        return error.MmapFailed;
    };

    @memset(code_slice, 0);

    const aligned_text = alignUp(@intFromPtr(code_slice.ptr), page_size);
    const aligned_data = aligned_text + header.text_size;

    const text_buf = @as([*]u8, @ptrFromInt(aligned_text))[0..header.text_size];
    const text_amt = try posix.pread(fd, text_buf, text_offset);
    if (text_amt != header.text_size) {
        std.debug.print("Error: Cannot read text section\n", .{});
        posix.munmap(code_slice);
        return error.ReadFailed;
    }

    if (header.data_size > 0) {
        const data_buf = @as([*]u8, @ptrFromInt(aligned_data))[0..header.data_size];
        const data_amt = try posix.pread(fd, data_buf, data_offset);
        if (data_amt != header.data_size) {
            std.debug.print("Error: Cannot read data section\n", .{});
            posix.munmap(code_slice);
            return error.ReadFailed;
        }
    }

    var data_addr: ?[]align(4096) u8 = null;
    if (header.data_size > 0) {
        data_addr = posix.mmap(null, header.data_size + page_size, posix.PROT.READ | posix.PROT.WRITE, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0) catch {
            std.debug.print("Error: Cannot map data section\n", .{});
            posix.munmap(code_slice);
            return error.MmapFailed;
        };

        @memcpy(data_addr.?[0..header.data_size], @as([*]u8, @ptrFromInt(aligned_data))[0..header.data_size]);
    }

    const stack_addr = posix.mmap(null, header.stack_size + page_size, posix.PROT.READ | posix.PROT.WRITE, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0) catch {
        std.debug.print("Error: Cannot allocate stack\n", .{});
        posix.munmap(code_slice);
        if (header.data_size > 0) {
            posix.munmap(data_addr.?);
        }
        return error.MmapFailed;
    };

    const aligned_stack = alignUp(@intFromPtr(stack_addr.ptr), 16) + header.stack_size - 8;

    posix.close(fd);

    const entry_point = aligned_text + header.entry_point;
    try out.write("\n=== Executing Program ===\n");
    try out.flush();

    trampoline(@as(*anyopaque, @ptrFromInt(entry_point)), @as(*anyopaque, @ptrFromInt(aligned_stack)), &trampoline_alloc, &trampoline_free);
}
