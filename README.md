# Aura Programming Language

A low-level systems programming language combining the philosophy of Zig, Carbon, and Go.

## Philosophy

Aura is designed for:

- **OS Kernels** - Full control over memory and execution
- **Bootloaders** - No runtime dependencies
- **Bare-metal programming** - Predictable behavior
- **High-performance applications** - Zero-cost abstractions

### Core Principles

1. **Explicit Everything** - No hidden behavior
2. **No Runtime** - Direct hardware access
3. **No GC** - Manual memory management
4. **ABI-Aware** - Full control over calling conventions
5. **Predictable** - Every operation has defined semantics

## Mutability Rules (Inverted)

Aura intentionally inverts common mutability semantics:

```aura
const counter: i32 = 0     # Mutable value, immutable binding
counter += 1               # ✅ Valid

var max_limit: i32 = 10    # Immutable value, mutable binding
max_limit += 1             # ❌ Compile error
```

**Rationale**: `const` means "this binding exists forever" (but the value can change). `var` means "this value is fixed" (but the binding can be reassigned).

## Architecture

```
Aura Source (.aura)
    ↓
Rust Compiler Frontend (lexer → parser → typecheck)
    ↓
Machine Code Generator (x86_64)
    ↓
Zig Binary Builder (or Rust fallback)
    ↓
Custom Aura Binary (.aura)
    ↓
C Runtime Loader (auraload)
    ↓
Assembly Trampoline
    ↓
Program Execution
```

## Building

```bash
# Full build
./build.sh

# Or step by step:
cargo build --release
gcc -static -O2 -o bin/auraload bin/auraload/main.c
gcc -c -o lib/trampoline/trampoline.o lib/trampoline/trampoline.S
```

## Usage

```bash
# Compile
cargo run --release -- build tests/hello.aura

# Run
./bin/auraload tests/hello.aura

# Type check only
cargo run --release -- check tests/hello.aura

# Dump binary info
cargo run --release -- dump tests/hello.aura
```

## Language Features

### Types


| Type                      | Size        | Description       |
| ------------------------- | ----------- | ----------------- |
| `i8`, `i16`, `i32`, `i64` | 1/2/4/8     | Signed integers   |
| `u8`, `u16`, `u32`, `u64` | 1/2/4/8     | Unsigned integers |
| `f32`, `f64`              | 4/8         | Floating point    |
| `bool`                    | 1           | Boolean           |
| `*T`                      | 8           | Pointer           |
| `[N]T`                    | N*sizeof(T) | Fixed array       |

### Functions

```aura
fn add(a: i32, b: i32) i32 {
    return a + b
}

noreturn fn exit(code: i32) -> noreturn {
    asm("mov $$60, %rax" :: "D"(code));
    asm("syscall");
}
```

### Inline Assembly

```aura
asm("syscall"
    : "=a"(result)
    : "a"(syscall_nr), "D"(a1), "S"(a2), "d"(a3)
    : "rcx", "r11", "memory");
```

### Structs

```aura
struct Point {
    x: f64,
    y: f64,
}
```

## Binary Format (.aura)

```
Offset  Size  Field
0       4     Magic (0x41555241 "AURA")
4       1     Version
8       8     Entry Point RVA
16      8     Stack Size
24      8     Text Offset
32      8     Text Size
40      8     Data Offset
48      8     Data Size
56      8     BSS Size
64      N     Text Section (code)
N       M     Data Section (initialized data)
```

## ABI Requirements (x86_64 System V)

- Stack aligned to 16 bytes at function call
- Red zone: 128 bytes below %rsp
- Arguments: %rdi, %rsi, %rdx, %rcx, %r8, %r9
- Return: %rax (integer), %xmm0 (float)
- Callee-saved: %rbx, %r12, %r13, %r14, %r15, %rbp

## Non-Goals (Explicitly Forbidden)

- ❌ Garbage collection
- ❌ Hidden runtime
- ❌ Implicit heap allocation
- ❌ Exceptions
- ❌ Reflection
- ❌ JIT compilation
- ❌ ELF output
- ❌ Dynamic linking

## Long-Term Vision

- [ ]  Self-hosting compiler
- [ ]  Cross-architecture backends (RISC-V, ARM64)
- [ ]  WebAssembly target
- [ ]  Kernel-mode Aura
- [ ]  Formal verification

## License

MIT
