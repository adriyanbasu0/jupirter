.intel_syntax noprefix
.code64

.section .text
.global trampoline

# Trampoline: Transfers control to Aura entry point
# Arguments:
#   rdi = entry point address
#   rsi = stack pointer
#   rdx = alloc function pointer
#   rcx = free function pointer
#
# This trampoline:
# 1. Switches to the new stack
# 2. Aligns stack to 16 bytes (System V ABI requires %rsp % 16 == 8 at entry)
# 3. Clears registers
# 4. Jumps to entry point
# 5. On return, exits with the value in RAX

trampoline:
    # Save entry point in r12 (callee-saved)
    mov r12, rdi

    # Save alloc/free ptrs in r14/r15 (callee-saved)
    mov r14, rdx
    mov r15, rcx

    # Set up new stack
    mov rsp, rsi

    # System V AMD64 ABI requires stack to be 16-byte aligned
    and rsp, -16

    # Clear registers (except r10, r12, r14, r15 which have entry point, alloc, free, and var access)
    xor eax, eax        # Return value register
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx
    xor esi, esi
    xor edi, edi
    xor ebp, ebp
    xor r8, r8
    xor r9, r9
    xor r11, r11
    xor r13, r13

    # Clear RFLAGS (except fixed bits)
    pushfq
    pop rax
    and rax, 0x3F7FD7   # Clear user-settable bits
    push rax
    popfq

    # Jump to entry point
    jmp r12

    # Return from user program - RAX contains the exit code
    # Use syscall to exit with that code
    mov rdi, rax        # exit code
    mov rax, 60         # __NR_exit
    syscall

    # Never reached
    ret
