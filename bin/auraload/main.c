#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>

#define AURA_MAGIC 0x41555241
#define AURA_VERSION 2

#pragma pack(push, 1)
typedef struct {
    uint8_t magic[4];
    uint8_t version;
    uint8_t flags;
    uint16_t reserved;
    uint64_t entry_point;
    uint64_t stack_size;
    uint64_t text_offset;
    uint64_t text_size;
    uint64_t data_offset;
    uint64_t data_size;
    uint64_t bss_size;
    uint64_t reloc_count;
    uint64_t symbol_count;
    uint64_t capability_count;
    uint64_t topology_count;
    uint64_t bitregion_count;
} AuraHeaderV2;
#pragma pack(pop)

extern void trampoline(void *entry, void *stack, void* alloc_ptr, void* free_ptr);

void* trampoline_alloc(size_t size) {
    void* ptr = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (ptr == MAP_FAILED) {
        return NULL;
    }
    return ptr;
}

void trampoline_free(void* ptr, size_t size) {
    munmap(ptr, size);
}

static void *align_up(void *ptr, size_t alignment) {
    uintptr_t addr = (uintptr_t)ptr;
    addr = (addr + alignment - 1) & ~(alignment - 1);
    return (void *)addr;
}

static uint64_t read_u64(int fd, size_t offset) {
    uint64_t val;
    pread(fd, &val, sizeof(val), offset);
    return val;
}

static uint8_t read_u8(int fd, size_t offset) {
    uint8_t val;
    pread(fd, &val, sizeof(val), offset);
    return val;
}

static void read_bytes(int fd, void *buf, size_t len, size_t offset) {
    pread(fd, buf, len, offset);
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <program.aura>\n", argv[0]);
        return 1;
    }

    const char *filename = argv[1];
    int fd = open(filename, O_RDONLY);
    if (fd < 0) {
        fprintf(stderr, "Error: Cannot open file: %s\n", filename);
        return 1;
    }

    off_t file_size = lseek(fd, 0, SEEK_END);
    if (file_size < 0) {
        fprintf(stderr, "Error: Cannot get file size\n");
        close(fd);
        return 1;
    }

    AuraHeaderV2 header;
    if (pread(fd, &header, sizeof(header), 0) != sizeof(header)) {
        fprintf(stderr, "Error: Cannot read header\n");
        close(fd);
        return 1;
    }

    if (header.magic[0] != 'A' || header.magic[1] != 'U' ||
        header.magic[2] != 'R' || header.magic[3] != 'A') {
        fprintf(stderr, "Error: Invalid magic number\n");
        close(fd);
        return 1;
    }

    if (header.version == 1) {
        fprintf(stderr, "Error: Version 1 binaries not supported by this loader\n");
        close(fd);
        return 1;
    }

    if (header.version != AURA_VERSION) {
        fprintf(stderr, "Error: Unsupported version: %u (expected %u)\n", header.version, AURA_VERSION);
        close(fd);
        return 1;
    }

    printf("=== Aura Binary Loader ===\n");
    printf("Version: %u | Entry: 0x%lx | Stack: %lu bytes\n",
           header.version, header.entry_point, header.stack_size);
    printf("Text: %lu bytes | Data: %lu bytes | BSS: %lu bytes\n",
           header.text_size, header.data_size, header.bss_size);
    fflush(stdout);

    size_t text_offset = sizeof(AuraHeaderV2);
    size_t data_offset = text_offset + ((header.text_size + 15) & ~15);
    size_t data_size_aligned = (header.data_size + 15) & ~15;

    size_t reloc_offset = data_offset + data_size_aligned;
    size_t symbol_offset = reloc_offset + header.reloc_count * 280;
    size_t capability_offset = symbol_offset + header.symbol_count * 280;
    size_t topology_offset = capability_offset + header.capability_count * 280;
    size_t bitregion_offset = topology_offset + header.topology_count * 280;

    printf("\n=== Physical Memory Capability Bounds ===\n");
    fflush(stdout);

    for (uint64_t i = 0; i < header.capability_count; i++) {
        uint64_t name_len = read_u64(fd, capability_offset + (i * 280));
        char name[257];
        if (name_len > 256) name_len = 256;
        read_bytes(fd, name, name_len, capability_offset + (i * 280) + 8);
        name[name_len] = '\0';

        uint64_t base_address = read_u64(fd, capability_offset + (i * 280) + 8 + name_len + 1);
        uint64_t length = read_u64(fd, capability_offset + (i * 280) + 8 + name_len + 1 + 8);
        uint8_t mode = read_u8(fd, capability_offset + (i * 280) + 8 + name_len + 1 + 8 + 8);

        uint64_t end_address = base_address + length - 1;
        printf("  [CAPABILITY] %s\n", name);
        printf("    Address Bounds: 0x%016lx - 0x%016lx\n", base_address, end_address);
        printf("    Size: %lu bytes (0x%lx)\n", length, length);
        const char *mode_str[] = {"read-only", "write-only", "read-write", "execute-only"};
        printf("    Access Mode: %s\n", mode_str[mode % 4]);
        fflush(stdout);
    }

    if (header.capability_count == 0) {
        printf("  (none defined)\n");
        fflush(stdout);
    }

    printf("\n=== Memory Topology Regions ===\n");
    fflush(stdout);

    for (uint64_t i = 0; i < header.topology_count; i++) {
        uint64_t name_len = read_u64(fd, topology_offset + (i * 280));
        char name[257];
        if (name_len > 256) name_len = 256;
        read_bytes(fd, name, name_len, topology_offset + (i * 280) + 8);
        name[name_len] = '\0';

        uint8_t numa_node = read_u8(fd, topology_offset + (i * 280) + 8 + name_len + 1);
        uint8_t cache_level = read_u8(fd, topology_offset + (i * 280) + 8 + name_len + 1 + 1);
        uint8_t memory_class = read_u8(fd, topology_offset + (i * 280) + 8 + name_len + 1 + 1 + 1);

        printf("  [TOPOLOGY] %s\n", name);
        printf("    NUMA Node: %u\n", numa_node == 0xFF ? 255 : numa_node);
        printf("    Cache Level: L%u\n", cache_level);
        const char *mem_class[] = {"Normal", "Device", "DMA", "DMA-coherent", "Framebuffer", "Encrypted"};
        if (memory_class < 6) {
            printf("    Memory Class: %s\n", mem_class[memory_class]);
        } else {
            printf("    Memory Class: Unknown\n");
        }
        fflush(stdout);
    }

    if (header.topology_count == 0) {
        printf("  (none defined)\n");
        fflush(stdout);
    }

    printf("\n=== Bit-Region Type Definitions ===\n");
    fflush(stdout);

    for (uint64_t i = 0; i < header.bitregion_count; i++) {
        uint64_t name_len = read_u64(fd, bitregion_offset + (i * 280));
        char name[257];
        if (name_len > 256) name_len = 256;
        read_bytes(fd, name, name_len, bitregion_offset + (i * 280) + 8);
        name[name_len] = '\0';

        uint64_t base_type_len = read_u64(fd, bitregion_offset + (i * 280) + 8 + name_len + 1);
        char base_type[65];
        if (base_type_len > 64) base_type_len = 64;
        read_bytes(fd, base_type, base_type_len, bitregion_offset + (i * 280) + 8 + name_len + 1 + 8);
        base_type[base_type_len] = '\0';

        uint64_t region_count = read_u64(fd, bitregion_offset + (i * 280) + 8 + name_len + 1 + 8 + base_type_len + 1);

        printf("  [BITREGION] %s : %s\n", name, base_type);
        printf("    Regions Defined: %lu\n", region_count);
        fflush(stdout);

        size_t region_offset = bitregion_offset + (i * 280) + 8 + name_len + 1 + 8 + base_type_len + 1 + 8;
        for (uint64_t j = 0; j < region_count; j++) {
            uint64_t region_name_len = read_u64(fd, region_offset);
            char region_name[257];
            if (region_name_len > 256) region_name_len = 256;
            read_bytes(fd, region_name, region_name_len, region_offset + 8);
            region_name[region_name_len] = '\0';

            uint8_t bit_offset = read_u8(fd, region_offset + 8 + region_name_len + 1);
            uint8_t bit_width = read_u8(fd, region_offset + 8 + region_name_len + 1 + 1);
            uint8_t access = read_u8(fd, region_offset + 8 + region_name_len + 1 + 1 + 1);

            const char *access_str[] = {"read-only", "write-only", "read-write"};
            printf("      %s: bits[%u..%u] %s\n", region_name, bit_offset, bit_offset + bit_width - 1, access_str[access % 3]);

            region_offset += 8 + region_name_len + 1 + 1 + 1 + 1;
        }
        fflush(stdout);
    }

    if (header.bitregion_count == 0) {
        printf("  (none defined)\n");
        fflush(stdout);
    }

    size_t page_size = sysconf(_SC_PAGESIZE);
    size_t total_size = header.text_size + header.data_size + page_size;

    void *code_addr = mmap(NULL, total_size,
                           PROT_READ | PROT_WRITE | PROT_EXEC,
                           MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (code_addr == MAP_FAILED) {
        fprintf(stderr, "Error: Cannot allocate memory\n");
        close(fd);
        return 1;
    }

    memset(code_addr, 0, total_size);

    void *aligned_text = align_up(code_addr, page_size);
    void *aligned_data = (void *)((uintptr_t)aligned_text + header.text_size);

    if (pread(fd, aligned_text, header.text_size, text_offset)
        != (ssize_t)header.text_size) {
        fprintf(stderr, "Error: Cannot read text section\n");
        munmap(code_addr, total_size);
        close(fd);
        return 1;
    }

    if (header.data_size > 0) {
        if (pread(fd, aligned_data, header.data_size, data_offset)
            != (ssize_t)header.data_size) {
            fprintf(stderr, "Error: Cannot read data section\n");
            munmap(code_addr, total_size);
            close(fd);
            return 1;
        }
    }

    uint64_t data_base_addr = 0x1000000;
    void *data_addr = mmap((void *)data_base_addr, header.data_size + page_size,
                            PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0);
    if (data_addr != (void *)data_base_addr && header.data_size > 0) {
        fprintf(stderr, "Error: Cannot map data at fixed address\n");
        munmap(code_addr, total_size);
        close(fd);
        return 1;
    }

    if (header.data_size > 0) {
        memcpy(data_addr, aligned_data, header.data_size);
    }

    void *stack_addr = mmap(NULL, header.stack_size + page_size,
                            PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stack_addr == MAP_FAILED) {
        fprintf(stderr, "Error: Cannot allocate stack\n");
        munmap(code_addr, total_size);
        if (header.data_size > 0) {
            munmap(data_addr, header.data_size + page_size);
        }
        close(fd);
        return 1;
    }

    void *aligned_stack = (void *)((uintptr_t)align_up(stack_addr, 16) + header.stack_size - 8);

    close(fd);

    void *entry_point = (void *)((uintptr_t)aligned_text + header.entry_point);
    printf("\n=== Executing Program ===\n");
    fflush(stdout);
    trampoline(entry_point, aligned_stack, &trampoline_alloc, &trampoline_free);

    return 0;
}
