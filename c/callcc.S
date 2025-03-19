// Implementation for SCM_CALLCC_ONESHOT, and bits for SCM_CALLCC if required.

	.globl cur_link
	.globl ccresthunk
	.globl rcimmix_alloc
	.globl need_more_frames
	.globl argcnt
	.globl ccresthunk_oneshot
	.globl SCM_CALLCC_ONESHOT
	.text
#if defined(__x86_64__)

	.intel_syntax noprefix
// About need_more_frames:
// Unfortunately on x86, the return registers and first arg registers
// are not equivalent.  In some cases (-coverage), llvm/clang will
// insert code at the start of the function (even for naked
// functions!!).  Since this isn't a *real* function, but really a
// return point, we have to prevent this.  Just a small thunk that could
// otherwise be in C if the return register RAX were RDI instead.

// Oneshot continuations: Basically just longjmp/setjmp, except it
// produces a real tailcall (the continuation is on the stack).
// setjmp requires the caller frame to remain on the stack directly.
// TODO: possibly pre-allocate the continuation space, to simplify the
// asm.

	
need_more_frames:
	mov rsi, rax // Why we need a thunk: return value needs to move to second arg
	mov rdi, cur_link[rip] // move global (or TODO thread-local) link to first arg
	add rdi, 2 // make it look like a closure
	jmp ccresthunk // Jump to return routine

	// Requires returns_twice, preserve_none in LLVM
SCM_CALLCC_ONESHOT:
	// Preserve the two registers that preserve_none doesn't: rsp, rbp
	mov r14, r12 // First arg in preservenonecc is r12
	mov r12, rsp
	mov r13, rbp
	mov rdi, 32// size of ccsave_oneshot 4*8 = 32
	push rbp // align stack
	call rcimmix_alloc
	pop rbp
	mov QWORD PTR [rax], 0x12 // CLOSURE_TAG
	lea rdi, [rip + ccresthunk_oneshot]
	mov QWORD PTR [rax+8], rdi
	mov [rax+16], r12
	mov [rax+24], r13
	lea rsi, [rax + 2] // Add PTR_TAG and set as arg 2
	mov rdi, r14 // set thunk closure as arg1
	mov QWORD PTR [rip + argcnt], 2 // Set argument count
	jmp [rdi+6] // Get thunk pointer & call

	// RDI is closure, RSI is return value.
ccresthunk_oneshot:
	mov rax, rsi // Move return in to position.
	mov rbp, [rdi+22] // Restore callee-saved
	mov rsp, [rdi+14]
	ret
#elif defined(__aarch64__)
SCM_CALLCC_ONESHOT:
	mov x19, x20 // First arg reg is x20.
	mov x20, SP
	mov X21, FP
	mov x0, 32 // Make a call, allocate 32-byte closure.
	stp x29, x30, [sp, -16]!
	bl rcimmix_alloc
	ldp x29, x30, [sp], 16
	mov x1, #0x12 // setup closure tag, pointer, SP,FP saves.
	str x1, [x0]
	adrp x1, ccresthunk_oneshot
	add x1, x1, :lo12:ccresthunk_oneshot
	str x1, [x0, #8]
	str x20, [x0, #16]
	str x21, [x0, #24]
	add x1, x0, #2 // Tag closure, move to secondcall arg.
	mov x0, x19 // Move thunk to x0, fetch ptr.
	ldr x2, [x0, #6] 
	// Set argcnt to 2.
	adrp x3, :got:argcnt
	mov x4, #2
	ldr x3, [x3, :got_lo12:argcnt]
	str x4, [x3]
	// Finally, tail-call the thunk ptr.
	br x2
	
	// x0 is closure, x1 is return value
ccresthunk_oneshot:
	ldr FP, [x0, #22]
	ldr x2, [x0, #14] // Can't  ldr SP directly.
	mov SP, x2
	mov x0, x1
	ret
#endif
	.section  .note.GNU-stack, "", @progbits
