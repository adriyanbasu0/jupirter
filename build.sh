#!/bin/bash
set -e

echo "=== Aura Language Compiler Build ==="

cd /home/adriyan/behead

# Build Rust compiler
echo "Building Rust compiler..."
cargo build --release

# Build Zig loader
echo "Building Zig loader (auraload)..."
zig build-exe -O ReleaseFast bin/auraload/main.zig lib/trampoline/trampoline.o -femit-bin=bin/auraload/auraload

# Build trampoline if not already built
if [ ! -f "lib/trampoline/trampoline.o" ]; then
    echo "Building assembly trampoline..."
    gcc -c -o lib/trampoline/trampoline.o lib/trampoline/trampoline.S
fi

echo ""
echo "=== Build Complete ==="
echo ""
echo "Binaries:"
echo "  Rust compiler:   target/release/aura"
echo "  Zig runtime:     bin/auraload/auraload"
echo "  Trampoline:      lib/trampoline/trampoline.o"
echo ""
echo "Running example program..."
echo ""

# Run the example program and capture exit code
./target/release/aura build -o out.aura tests/kernel_features.aura
EXIT_CODE=$?
echo ""
echo "Build exit code: $EXIT_CODE"

if [ $EXIT_CODE -eq 0 ]; then
    set +e
    ./bin/auraload/auraload out.aura
    EXIT_CODE=$?
    set -e
    echo ""
    echo "Program exit code: $EXIT_CODE"
fi
