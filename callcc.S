// Unfortunately on x86, the return registers and first arg registers
// are not equivalent.  In some cases (-coverage), llvm/clang will
// insert code at the start of the function (even for naked
// functions!!).  Since this isn't a *real* function, but really a
// return point, we have to prevent this.  Just a small thunk that could
// otherwise be in C if the return register RAX were RDI instead.
#if defined(__x86_64__)

	.globl cur_link
	.globl ccresthunk
	.globl need_more_frames
	.intel_syntax noprefix
	.text
need_more_frames:
	mov rsi, rax // Why we need a thunk: return value needs to move to second arg
	mov rdi, cur_link[rip] // move global (or TODO thread-local) link to first arg
	add rdi, 2 // make it look like a closure
	jmp ccresthunk // Jump to return routine

#endif
	.section  .note.GNU-stack, "", @progbits
