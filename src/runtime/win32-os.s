	.file	"win32-os.c"
	.text
	.p2align 4,,15
	.def	save_lisp_tls;	.scl	3;	.type	32;	.endef
	.seh_proc	save_lisp_tls
save_lisp_tls:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	addq	$8, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.def	restore_lisp_tls;	.scl	3;	.type	32;	.endef
	.seh_proc	restore_lisp_tls
restore_lisp_tls:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	addq	$8, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	fiber_funeral_function
	.def	fiber_funeral_function;	.scl	2;	.type	32;	.endef
	.seh_proc	fiber_funeral_function
fiber_funeral_function:
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$88, %rsp
	.seh_stackalloc	88
	.seh_endprologue
	movq	__imp_GetQueuedCompletionStatus(%rip), %rsi
	movq	%rcx, %rbx
	jmp	.L4
	.p2align 4,,10
.L6:
	movq	48(%rsp), %rcx
	testq	%rcx, %rcx
	je	.L5
	call	pthread_np_switch_to_fiber
.L4:
	movl	$-1, 32(%rsp)
	leaq	64(%rsp), %r9
	leaq	48(%rsp), %r8
	leaq	76(%rsp), %rdx
	movq	%rbx, %rcx
	call	*%rsi
	testl	%eax, %eax
	jne	.L6
.L5:
	movq	%rbx, %rcx
	call	*__imp_CloseHandle(%rip)
	xorl	%eax, %eax
	addq	$88, %rsp
	popq	%rbx
	popq	%rsi
	ret
	.seh_endproc
	.section .rdata,"dr"
.LC0:
	.ascii "Now should know DLL: %s\12\0"
	.align 8
.LC1:
	.ascii "DLL detection: %u, base %p: %s\12\0"
	.text
	.p2align 4,,15
	.def	os_get_build_time_shared_libraries.constprop.7;	.scl	3;	.type	32;	.endef
	.seh_proc	os_get_build_time_shared_libraries.constprop.7
os_get_build_time_shared_libraries.constprop.7:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$72, %rsp
	.seh_stackalloc	72
	.seh_setframe	%rbp, 128
	.seh_endprologue
	testq	%rcx, %rcx
	movq	%rcx, %r12
	movq	%rdx, %r13
	je	.L21
.L9:
	movslq	60(%r12), %rax
	subq	$144, %rsp
	xorl	%edi, %edi
	leaq	48(%rsp), %rbx
	addq	%r12, %rax
	cmpl	$17744, (%rax)
	je	.L22
.L10:
	movl	%edi, %eax
	leaq	-56(%rbp), %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
	.p2align 4,,10
.L22:
	mov	144(%rax), %esi
	movq	__imp_GetModuleHandleA(%rip), %r14
	addq	%r12, %rsi
	.p2align 4,,10
.L11:
	movl	16(%rsi), %eax
	testl	%eax, %eax
	je	.L10
	movl	dyndebug_runtime_link(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L23
.L12:
	mov	12(%rsi), %ecx
	addq	%r12, %rcx
	call	*%r14
	testq	%rax, %rax
	movq	%rax, %r15
	je	.L13
	testl	%edi, %edi
	je	.L14
	cmpq	(%rbx), %rax
	je	.L13
	leal	-1(%rdi), %edx
	xorl	%eax, %eax
	salq	$3, %rdx
	jmp	.L15
	.p2align 4,,10
.L16:
	addq	$8, %rax
	cmpq	(%rbx,%rax), %r15
	je	.L13
.L15:
	cmpq	%rdx, %rax
	jne	.L16
.L14:
	mov	%edi, %eax
	testq	%r13, %r13
	movq	%r15, (%rbx,%rax,8)
	je	.L17
	movq	%r15, 0(%r13,%rax,8)
.L17:
	movl	dyndebug_runtime_link(%rip), %edx
	testl	%edx, %edx
	jne	.L24
.L18:
	addl	$1, %edi
.L13:
	addq	$20, %rsi
	cmpl	$14, %edi
	jbe	.L11
	jmp	.L10
	.p2align 4,,10
.L23:
	mov	12(%rsi), %eax
	movq	dyndebug_output(%rip), %rcx
	leaq	.LC0(%rip), %rdx
	leaq	(%r12,%rax), %r8
	call	fprintf
	jmp	.L12
	.p2align 4,,10
.L24:
	mov	12(%rsi), %eax
	addq	%r12, %rax
	movq	%rax, -72(%rbp)
	call	*__imp___iob_func(%rip)
	leaq	96(%rax), %rcx
	movq	-72(%rbp), %rax
	leaq	.LC1(%rip), %rdx
	movq	%r15, %r9
	movl	%edi, %r8d
	movq	%rax, 32(%rsp)
	call	fprintf
	jmp	.L18
.L21:
	xorl	%ecx, %ecx
	call	*__imp_GetModuleHandleA(%rip)
	movq	%rax, %r12
	.p2align 4,,3
	jmp	.L9
	.seh_endproc
	.section .rdata,"dr"
.LC2:
	.ascii "<FormatMessage failed>\0"
.LC3:
	.ascii "win32-os.c\0"
	.align 8
.LC4:
	.ascii "Expression unexpectedly false: %s:%d\12 ... %s\12     ===> returned #X%p, \12     (in thread %p) ... Win32 thinks:\12     ===> code %u, message => %s\12 ... CRT thinks:\12     ===> code %u, message => %s\12\0"
	.text
	.p2align 4,,15
	.def	win_aver.part.3.constprop.8;	.scl	3;	.type	32;	.endef
	.seh_proc	win_aver.part.3.constprop.8
win_aver.part.3.constprop.8:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$136, %rsp
	.seh_stackalloc	136
	.seh_endprologue
	leaq	.LC2(%rip), %rax
	movl	%r9d, %r12d
	movq	%rdx, %r13
	movl	%r8d, %r15d
	movq	%rcx, %rsi
	movq	%rax, 120(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbp
	movl	%eax, %ebx
	call	*%rbp
	movl	(%rax), %edi
	call	*%rbp
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	120(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	testl	%r12d, %r12d
	movl	%eax, %r14d
	jne	.L26
	movl	dyndebug_survive_aver(%rip), %r8d
	testl	%r8d, %r8d
	je	.L27
.L26:
	movq	120(%rsp), %rdx
	mov	specials(%rip), %r12d
	movq	%rdx, 104(%rsp)
	call	pthread_self
	movq	104(%rsp), %rdx
	movq	%rbp, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%edi, 72(%rsp)
	movl	%ebx, 56(%rsp)
	movl	%r15d, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	1432(%rax,%r12,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	%rsi, 40(%rsp)
	movq	%r13, 32(%rsp)
	movq	%rax, 48(%rsp)
	call	fprintf
	testl	%r14d, %r14d
	je	.L30
	movq	120(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
.L30:
	addq	$136, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L27:
	mov	specials(%rip), %r12d
	movq	120(%rsp), %r14
	call	pthread_self
	movq	%rbp, 72(%rsp)
	movl	%edi, 64(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	%r14, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC4(%rip), %rcx
	movq	1432(%rax,%r12,8), %rax
	movq	%r13, %r9
	movl	%r15d, %r8d
	movq	%rsi, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.p2align 4,,15
	.globl	fff_foreign_callback
	.def	fff_foreign_callback;	.scl	2;	.type	32;	.endef
	.seh_proc	fff_foreign_callback
fff_foreign_callback:
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$40, %rsp
	.seh_stackalloc	40
	.seh_endprologue
	mov	specials(%rip), %esi
	movq	%rcx, %rbx
	call	pthread_self
	movq	%rax, %rdi
	movq	1432(%rax,%rsi,8), %rax
	movq	240(%rax), %rdx
	xorl	%ebp, %ebp
	movq	(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L32
	movq	$0, (%rdx)
	movq	248(%rax), %rbp
	movq	$0, 248(%rax)
.L32:
	movq	537923376, %rcx
	movq	16(%rbx), %r9
	movq	8(%rbx), %r8
	movq	(%rbx), %rdx
	testq	%rcx, %rcx
	je	.L33
	shrq	$3, %rcx
	movq	(%rax,%rcx,8), %rcx
	cmpq	$98, %rcx
	je	.L33
.L34:
	call	funcall3
	mov	specials(%rip), %eax
	testq	%rsi, %rsi
	movq	1432(%rdi,%rax,8), %rax
	je	.L35
	movq	240(%rax), %rdx
	movq	%rsi, (%rdx)
	movq	%rbp, 248(%rax)
.L35:
	movl	$1, 24(%rbx)
	addq	$40, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	ret
	.p2align 4,,10
.L33:
	movq	537923336, %rcx
	jmp	.L34
	.seh_endproc
	.section .rdata,"dr"
.LC5:
	.ascii "lock\0"
.LC6:
	.ascii "Trying to %s dead mutex %p\12\0"
.LC7:
	.ascii "unlock\0"
	.text
	.p2align 4,,15
	.def	tty_read_line_server;	.scl	3;	.type	32;	.endef
	.seh_proc	tty_read_line_server
tty_read_line_server:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$72, %rsp
	.seh_stackalloc	72
	.seh_endprologue
	movq	32776+ttyinput(%rip), %rax
	leaq	DEAD_MUTEX(%rip), %rbx
	cmpq	%rbx, %rax
	je	.L55
.L38:
	cmpq	$-1, %rax
	je	.L56
	leaq	64(%rax), %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L42:
	cmpq	$0, 32896+ttyinput(%rip)
	movq	__imp_LeaveCriticalSection(%rip), %r13
	je	.L40
	movl	32904+ttyinput(%rip), %r10d
	leaq	60(%rsp), %rdi
	leaq	ttyinput(%rip), %rbp
	movq	__imp_ReadConsoleW(%rip), %r12
	testl	%r10d, %r10d
	jne	.L49
	.p2align 4,,10
.L53:
	leaq	32776+ttyinput(%rip), %rdx
	leaq	32832+ttyinput(%rip), %rcx
	call	pthread_cond_wait
	movl	32904+ttyinput(%rip), %r9d
	testl	%r9d, %r9d
	je	.L53
.L49:
	movq	32776+ttyinput(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.L57
.L44:
	addq	$64, %rcx
	call	*%r13
	movl	32772+ttyinput(%rip), %eax
	movl	$16384, %r8d
	movq	32896+ttyinput(%rip), %rcx
	movq	$0, 32(%rsp)
	movq	%rdi, %r9
	mov	%eax, %edx
	subl	%eax, %r8d
	leaq	0(%rbp,%rdx,2), %rdx
	call	*%r12
	movq	32776+ttyinput(%rip), %rcx
	movl	%eax, %esi
	cmpq	%rbx, %rcx
	je	.L58
.L45:
	cmpq	$-1, %rcx
	je	.L59
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L47:
	testl	%esi, %esi
	jne	.L60
.L48:
	cmpq	$0, 32896+ttyinput(%rip)
	movl	$0, 32904+ttyinput(%rip)
	jne	.L53
.L40:
	movq	32776+ttyinput(%rip), %rax
	cmpq	%rbx, %rax
	je	.L61
.L50:
	leaq	64(%rax), %rcx
	call	*%r13
	xorl	%eax, %eax
	addq	$72, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L60:
	leaq	32784+ttyinput(%rip), %rcx
	movl	60(%rsp), %eax
	addl	%eax, 32772+ttyinput(%rip)
	call	pthread_cond_broadcast
	jmp	.L48
.L59:
	leaq	32776+ttyinput(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L47
.L57:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L44
.L58:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L45
.L56:
	leaq	32776+ttyinput(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L42
.L61:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rax
	jmp	.L50
.L55:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rax
	jmp	.L38
	.seh_endproc
	.p2align 4,,15
	.def	tty_read_line_client;	.scl	3;	.type	32;	.endef
	.seh_proc	tty_read_line_client
tty_read_line_client:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$88, %rsp
	.seh_stackalloc	88
	.seh_endprologue
	xorl	%ebx, %ebx
	movslq	%r8d, %r8
	movq	%rcx, %r12
	movq	%rdx, %rsi
	shrq	%r8
	testl	%r8d, %r8d
	je	.L63
	movq	32776+ttyinput(%rip), %rcx
	cmpl	$16384, %r8d
	movl	$16384, %ebp
	leaq	DEAD_MUTEX(%rip), %rdi
	cmovle	%r8d, %ebp
	addl	%ebp, %ebp
	cmpq	%rdi, %rcx
	je	.L93
.L64:
	cmpq	$-1, %rcx
	je	.L94
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L66:
	movl	32888+ttyinput(%rip), %ebx
	testl	%ebx, %ebx
	je	.L95
.L67:
	mov	32768+ttyinput(%rip), %eax
	movl	32772+ttyinput(%rip), %edx
	xorl	%r12d, %r12d
	movslq	%ebp, %r14
	leaq	ttyinput(%rip), %r13
	jmp	.L90
	.p2align 4,,10
.L70:
	movl	32904+ttyinput(%rip), %r11d
	testl	%r11d, %r11d
	jne	.L72
	leaq	32832+ttyinput(%rip), %rcx
	movl	$1, 32904+ttyinput(%rip)
	call	pthread_cond_broadcast
.L72:
	leaq	32776+ttyinput(%rip), %rdx
	leaq	32784+ttyinput(%rip), %rcx
	call	pthread_cond_wait
	cmpq	$0, ptr_CancelIoEx(%rip)
	movq	32896+ttyinput(%rip), %rbx
	je	.L91
	mov	specials(%rip), %r15d
	call	pthread_self
	movq	1432(%rax,%r15,8), %rdx
	movq	%rbx, %rax
	lock cmpxchgq	%r12, 264(%rdx)
.L91:
	mov	32768+ttyinput(%rip), %eax
	movl	32772+ttyinput(%rip), %edx
.L90:
	cmpl	%edx, %eax
	jne	.L96
	cmpq	$0, ptr_CancelIoEx(%rip)
	movq	32896+ttyinput(%rip), %r15
	je	.L70
	mov	specials(%rip), %eax
	movq	%rax, 72(%rsp)
	call	pthread_self
	movq	%rax, %rbx
	movq	72(%rsp), %rax
	movq	1432(%rbx,%rax,8), %rdx
	movq	%r12, %rax
	lock cmpxchgq	%r15, 264(%rdx)
	je	.L70
	mov	specials(%rip), %eax
	movq	1432(%rbx,%rax,8), %rax
	movq	144(%rax), %rcx
	call	*__imp_ResetEvent(%rip)
	mov	specials(%rip), %eax
	movq	1432(%rbx,%rax,8), %rax
	movl	$-1, %ebx
	movq	$0, 264(%rax)
	movl	$0, 32904+ttyinput(%rip)
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
.L71:
	movq	32776+ttyinput(%rip), %rcx
	cmpq	%rdi, %rcx
	je	.L97
.L84:
	addq	$64, %rcx
	call	*__imp_LeaveCriticalSection(%rip)
.L63:
	movl	%ebx, %eax
	addq	$88, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L95:
	movq	__imp_GetCurrentProcess(%rip), %r13
	call	*%r13
	movq	%rax, %rbx
	call	*%r13
	movl	$2, 48(%rsp)
	movq	%rax, %rcx
	movl	$0, 40(%rsp)
	movl	$0, 32(%rsp)
	leaq	32896+ttyinput(%rip), %r9
	movq	%rbx, %r8
	movq	%r12, %rdx
	call	*__imp_DuplicateHandle(%rip)
	testl	%eax, %eax
	jne	.L98
.L92:
	call	*__imp__errno(%rip)
	movl	$-1, %ebx
	movl	$5, (%rax)
	jmp	.L71
	.p2align 4,,10
.L96:
	subl	%eax, %edx
	leal	(%rdx,%rdx), %ebx
	cmpl	%ebp, %ebx
	cmovg	%ebp, %ebx
	cmpl	$0, %ebx
	je	.L76
	jle	.L71
	leaq	0(%r13,%rax,2), %rdx
	movq	%r14, %r8
	movq	%rsi, %rcx
	call	memcpy
	movl	32768+ttyinput(%rip), %eax
	movslq	%ebx, %r9
	movl	32772+ttyinput(%rip), %edx
	shrq	%r9
	addl	%r9d, %eax
	cmpl	%edx, %eax
	movl	%eax, 32768+ttyinput(%rip)
	je	.L99
.L78:
	xorl	%r10d, %r10d
	testq	%r9, %r9
	je	.L79
	xorl	%r8d, %r8d
	xorl	%ecx, %ecx
	jmp	.L82
	.p2align 4,,10
.L80:
	movl	%ecx, %r11d
	addl	$1, %ecx
	subl	%r10d, %r11d
	movw	%r8w, (%rsi,%r11,2)
	mov	%ecx, %r8d
	cmpq	%r8, %r9
	jbe	.L100
.L82:
	movzwl	(%rsi,%r8,2), %r8d
	cmpw	$13, %r8w
	jne	.L80
	addl	$1, %ecx
	addl	$1, %r10d
	mov	%ecx, %r8d
	cmpq	%r8, %r9
	ja	.L82
.L100:
	addl	%r10d, %r10d
.L79:
	subl	%r10d, %ebx
	je	.L90
	jmp	.L71
	.p2align 4,,10
.L98:
	leaq	32784+ttyinput(%rip), %rcx
	xorl	%edx, %edx
	call	pthread_cond_init
	leaq	32832+ttyinput(%rip), %rcx
	xorl	%edx, %edx
	call	pthread_cond_init
	leaq	tty_read_line_server(%rip), %r8
	leaq	32880+ttyinput(%rip), %rcx
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	call	pthread_create
	movl	$1, 32888+ttyinput(%rip)
	jmp	.L67
	.p2align 4,,10
.L76:
	movl	$0, 32772+ttyinput(%rip)
	movl	$0, 32768+ttyinput(%rip)
	jmp	.L92
.L97:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L84
.L94:
	leaq	32776+ttyinput(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L66
.L99:
	movl	$0, 32772+ttyinput(%rip)
	movl	$0, 32768+ttyinput(%rip)
	xorl	%edx, %edx
	xorl	%eax, %eax
	jmp	.L78
.L93:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L64
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC8:
	.ascii "PostQueuedCompletionStatus(dead_fiber_queue,0, (ULONG_PTR)companion_pthread,NULL)\0"
	.text
	.p2align 4,,15
	.def	fiber_is_dead;	.scl	3;	.type	32;	.endef
	.seh_proc	fiber_is_dead
fiber_is_dead:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	movq	%rcx, %r8
	movq	dead_fiber_queue(%rip), %rcx
	call	*__imp_PostQueuedCompletionStatus(%rip)
	movl	dyndebug_skip_averlax(%rip), %r13d
	testl	%r13d, %r13d
	jne	.L101
	testl	%eax, %eax
	je	.L105
.L101:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L105:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r12d
	movl	%eax, %ebp
	testl	%r12d, %r12d
	je	.L103
	mov	specials(%rip), %r12d
	movq	104(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1130, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC8(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L101
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L101
.L103:
	mov	specials(%rip), %ebp
	movq	104(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC8(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1130, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
.LC9:
	.ascii "[0x%p] %s\12\0"
	.text
	.p2align 4,,15
	.globl	odprint
	.def	odprint;	.scl	2;	.type	32;	.endef
	.seh_proc	odprint
odprint:
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$1064, %rsp
	.seh_stackalloc	1064
	.seh_endprologue
	movq	%rcx, %rsi
	call	*__imp_GetLastError(%rip)
	movl	%eax, %ebx
	call	pthread_self
	leaq	.LC9(%rip), %rdx
	leaq	32(%rsp), %rcx
	movq	%rax, %r8
	movq	%rsi, %r9
	call	sprintf
	leaq	32(%rsp), %rcx
	call	*__imp_OutputDebugStringA(%rip)
	movl	%ebx, %ecx
	call	*__imp_SetLastError(%rip)
	nop
	addq	$1064, %rsp
	popq	%rbx
	popq	%rsi
	ret
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC10:
	.ascii "[0x%p] [0x%04lx] [x%p] %s, %s, %s, %s \0"
	.align 8
.LC11:
	.ascii "[0x%p] [0x%04lx] (arch_os_get_current_thread() is NULL) \0"
	.text
	.p2align 4,,15
	.globl	odprintf_
	.def	odprintf_;	.scl	2;	.type	32;	.endef
	.seh_proc	odprintf_
odprintf_:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$1144, %rsp
	.seh_stackalloc	1144
	.seh_endprologue
	movq	%rdx, 1224(%rsp)
	movq	%r8, 1232(%rsp)
	movq	%rcx, %rdi
	movq	%r9, 1240(%rsp)
	call	*__imp_GetLastError(%rip)
	mov	specials(%rip), %ebx
	movl	%eax, %esi
	call	pthread_self
	movq	%rax, %rbp
	movq	1432(%rax,%rbx,8), %r12
	testq	%r12, %r12
	je	.L108
	movl	$537920783, %ecx
	leaq	96(%rsp), %rbx
	call	t_nil_s
	movl	$537920399, %ecx
	movq	%rax, %r15
	call	t_nil_s
	movl	$537920591, %ecx
	movq	%rax, %r14
	call	t_nil_s
	movl	$537920719, %ecx
	movq	%rax, %r13
	call	t_nil_s
	movq	%rax, 88(%rsp)
	call	*__imp_GetCurrentThreadId(%rip)
	movq	88(%rsp), %rdx
	movl	%eax, %r9d
	movq	%rbp, %r8
	movq	%rbx, %rcx
	movq	%r15, 64(%rsp)
	movq	%r14, 56(%rsp)
	movq	%r13, 48(%rsp)
	movq	%r12, 32(%rsp)
	movq	%rdx, 40(%rsp)
	leaq	.LC10(%rip), %rdx
	call	sprintf
.L109:
	movq	%rbx, %rcx
.L110:
	movl	(%rcx), %edx
	addq	$4, %rcx
	leal	-16843009(%rdx), %eax
	notl	%edx
	andl	%edx, %eax
	andl	$-2139062144, %eax
	je	.L110
	movl	%eax, %edx
	leaq	1224(%rsp), %r8
	shrl	$16, %edx
	testl	$32896, %eax
	cmove	%edx, %eax
	leaq	2(%rcx), %rdx
	movq	%r8, 1128(%rsp)
	cmove	%rdx, %rcx
	addb	%al, %al
	movq	%rdi, %rdx
	sbbq	$3, %rcx
	subq	%rbx, %rcx
	movslq	%ecx, %rcx
	addq	%rbx, %rcx
	call	vsprintf
	movq	%rbx, %rdx
.L112:
	movl	(%rdx), %ecx
	addq	$4, %rdx
	leal	-16843009(%rcx), %eax
	notl	%ecx
	andl	%ecx, %eax
	andl	$-2139062144, %eax
	je	.L112
	movl	%eax, %ecx
	movl	dyndebug_to_odstring(%rip), %r15d
	shrl	$16, %ecx
	testl	$32896, %eax
	cmove	%ecx, %eax
	leaq	2(%rdx), %rcx
	cmove	%rcx, %rdx
	addb	%al, %al
	sbbq	$3, %rdx
	subl	%ebx, %edx
	movslq	%edx, %rax
	addl	$1, %edx
	testl	%r15d, %r15d
	movslq	%edx, %rdx
	movb	$10, 96(%rsp,%rax)
	movb	$0, 96(%rsp,%rdx)
	jne	.L122
	movl	dyndebug_to_filestream(%rip), %r14d
	testl	%r14d, %r14d
	jne	.L123
.L115:
	movl	%esi, %ecx
	call	*__imp_SetLastError(%rip)
	nop
	addq	$1144, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L122:
	movq	%rbx, %rcx
	call	*__imp_OutputDebugStringA(%rip)
	movl	dyndebug_to_filestream(%rip), %r14d
	testl	%r14d, %r14d
	je	.L115
.L123:
	movq	loglock.75747(%rip), %rcx
	leaq	DEAD_MUTEX(%rip), %rdi
	cmpq	%rdi, %rcx
	je	.L124
.L116:
	cmpq	$-1, %rcx
	je	.L125
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L118:
	movq	dyndebug_output(%rip), %rdx
	movq	%rbx, %rcx
	call	fputs
	movq	loglock.75747(%rip), %rcx
	cmpq	%rdi, %rcx
	je	.L126
.L119:
	addq	$64, %rcx
	call	*__imp_LeaveCriticalSection(%rip)
	jmp	.L115
	.p2align 4,,10
.L108:
	leaq	96(%rsp), %rbx
	call	*__imp_GetCurrentThreadId(%rip)
	leaq	.LC11(%rip), %rdx
	movl	%eax, %r9d
	movq	%rbp, %r8
	movq	%rbx, %rcx
	call	sprintf
	jmp	.L109
	.p2align 4,,10
.L126:
	leaq	loglock.75747(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	loglock.75747(%rip), %rcx
	jmp	.L119
	.p2align 4,,10
.L125:
	leaq	loglock.75747(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L118
	.p2align 4,,10
.L124:
	leaq	loglock.75747(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	loglock.75747(%rip), %rcx
	jmp	.L116
	.seh_endproc
	.p2align 4,,15
	.globl	block_deferrables_and_return_mask
	.def	block_deferrables_and_return_mask;	.scl	2;	.type	32;	.endef
	.seh_proc	block_deferrables_and_return_mask
block_deferrables_and_return_mask:
	subq	$56, %rsp
	.seh_stackalloc	56
	.seh_endprologue
	xorl	%ecx, %ecx
	leaq	44(%rsp), %rdx
	call	block_deferrable_signals
	movl	44(%rsp), %eax
	addq	$56, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	apply_sigmask
	.def	apply_sigmask;	.scl	2;	.type	32;	.endef
	.seh_proc	apply_sigmask
apply_sigmask:
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$32, %rsp
	.seh_stackalloc	32
	.seh_endprologue
	movl	%ecx, %ebx
	call	pthread_self
	movl	%ebx, 40(%rax)
	addq	$32, %rsp
	popq	%rbx
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	UL_GetCurrentTeb
	.def	UL_GetCurrentTeb;	.scl	2;	.type	32;	.endef
	.seh_proc	UL_GetCurrentTeb
UL_GetCurrentTeb:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
/APP
 # 1594 "/home/anton/sdks/orgbin/cross-amd64-win64-20110206/bin/../lib/gcc/x86_64-w64-mingw32/4.6.0/../../../../x86_64-w64-mingw32/include/winnt.h" 1
	movq	%gs:48,%rax
 # 0 "" 2
/NO_APP
	addq	$8, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	UL_GetCurrentFrame
	.def	UL_GetCurrentFrame;	.scl	2;	.type	32;	.endef
	.seh_proc	UL_GetCurrentFrame
UL_GetCurrentFrame:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	.seh_setframe	%rbp, 0
	.seh_endprologue
	movq	%rbp, %rax
	popq	%rbp
	ret
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC12:
	.ascii "VirtualAlloc(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj), MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)\0"
	.text
	.p2align 4,,15
	.globl	alloc_gc_page
	.def	alloc_gc_page;	.scl	2;	.type	32;	.endef
	.seh_proc	alloc_gc_page
alloc_gc_page:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	movl	$8, %edx
	movl	$4, %r9d
	movl	$12288, %r8d
	movl	$553648128, %ecx
	call	*__imp_VirtualAlloc(%rip)
	movl	dyndebug_skip_averlax(%rip), %edx
	testl	%edx, %edx
	jne	.L131
	testq	%rax, %rax
	je	.L135
.L131:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L135:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %ebp
	movl	dyndebug_survive_aver(%rip), %eax
	testl	%eax, %eax
	je	.L133
	mov	specials(%rip), %r12d
	movq	104(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$455, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC12(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L131
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L131
.L133:
	mov	specials(%rip), %ebp
	movq	104(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC12(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$455, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC13:
	.ascii "VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj), PAGE_READWRITE, &oldProt)\0"
	.text
	.p2align 4,,15
	.globl	map_gc_page
	.def	map_gc_page;	.scl	2;	.type	32;	.endef
	.seh_proc	map_gc_page
map_gc_page:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	movl	$4, %r8d
	movl	$8, %edx
	movl	$553648128, %ecx
	leaq	108(%rsp), %r9
	call	*__imp_VirtualProtect(%rip)
	movl	dyndebug_skip_averlax(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L136
	testl	%eax, %eax
	je	.L140
.L136:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L140:
	leaq	.LC2(%rip), %rax
	movq	%rax, 96(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	96(%rsp), %rax
	xorl	%edx, %edx
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	$1033, %r9d
	movl	%ebx, %r8d
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %ecx
	movl	%eax, %ebp
	testl	%ecx, %ecx
	je	.L138
	mov	specials(%rip), %r12d
	movq	96(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$483, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC13(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L136
	movq	96(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L136
.L138:
	mov	specials(%rip), %ebp
	movq	96(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC13(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$483, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC14:
	.ascii "VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj), PAGE_READONLY, &oldProt)\0"
	.text
	.p2align 4,,15
	.globl	map_gc_page_readonly
	.def	map_gc_page_readonly;	.scl	2;	.type	32;	.endef
	.seh_proc	map_gc_page_readonly
map_gc_page_readonly:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	movl	$2, %r8d
	movl	$8, %edx
	movl	$553648128, %ecx
	leaq	108(%rsp), %r9
	call	*__imp_VirtualProtect(%rip)
	movl	dyndebug_skip_averlax(%rip), %r10d
	testl	%r10d, %r10d
	jne	.L141
	testl	%eax, %eax
	je	.L145
.L141:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L145:
	leaq	.LC2(%rip), %rax
	movq	%rax, 96(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	96(%rsp), %rax
	movl	$1033, %r9d
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r9d
	movl	%eax, %ebp
	testl	%r9d, %r9d
	je	.L143
	mov	specials(%rip), %r12d
	movq	96(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$531, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC14(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L141
	movq	96(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L141
.L143:
	mov	specials(%rip), %ebp
	movq	96(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC14(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$531, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC15:
	.ascii "VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj), PAGE_NOACCESS, &oldProt)\0"
	.text
	.p2align 4,,15
	.globl	unmap_gc_page
	.def	unmap_gc_page;	.scl	2;	.type	32;	.endef
	.seh_proc	unmap_gc_page
unmap_gc_page:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	movl	$1, %r8d
	movl	$8, %edx
	movl	$553648128, %ecx
	leaq	108(%rsp), %r9
	call	*__imp_VirtualProtect(%rip)
	movl	dyndebug_skip_averlax(%rip), %ebx
	testl	%ebx, %ebx
	jne	.L146
	testl	%eax, %eax
	je	.L150
.L146:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L150:
	leaq	.LC2(%rip), %rax
	movq	%rax, 96(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	96(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r11d
	movl	%eax, %ebp
	testl	%r11d, %r11d
	je	.L148
	mov	specials(%rip), %r12d
	movq	96(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$538, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC15(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L146
	movq	96(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L146
.L148:
	mov	specials(%rip), %ebp
	movq	96(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC15(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$538, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.p2align 4,,15
	.globl	do_nothing
	.def	do_nothing;	.scl	2;	.type	32;	.endef
	.seh_proc	do_nothing
do_nothing:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	addq	$8, %rsp
	ret
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC16:
	.ascii "Thread %p has CSP [%p] %p => %p\0"
	.text
	.p2align 4,,15
	.globl	os_get_csp
	.def	os_get_csp;	.scl	2;	.type	32;	.endef
	.seh_proc	os_get_csp
os_get_csp:
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$48, %rsp
	.seh_stackalloc	48
	.seh_endprologue
	movl	dyndebug_safepoints(%rip), %eax
	movq	%rcx, %rbx
	testl	%eax, %eax
	je	.L153
	movq	240(%rcx), %r8
	movq	72(%rcx), %rax
	movq	%rcx, %rdx
	leaq	.LC16(%rip), %rcx
	movq	%rax, 32(%rsp)
	movq	(%r8), %r9
	call	odprintf_
.L153:
	movq	240(%rbx), %rax
	movq	(%rax), %rax
	addq	$48, %rsp
	popq	%rbx
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	os_get_build_time_shared_libraries
	.def	os_get_build_time_shared_libraries;	.scl	2;	.type	32;	.endef
	.seh_proc	os_get_build_time_shared_libraries
os_get_build_time_shared_libraries:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$72, %rsp
	.seh_stackalloc	72
	.seh_setframe	%rbp, 128
	.seh_endprologue
	testq	%rdx, %rdx
	movl	%ecx, %r13d
	movq	%r8, %r15
	movq	%r9, %r14
	movq	%rdx, %r12
	je	.L172
.L155:
	movslq	60(%r12), %rdx
	mov	%r13d, %eax
	leaq	30(,%rax,8), %rax
	shrq	$4, %rax
	addq	%r12, %rdx
	salq	$4, %rax
	call	___chkstk_ms
	xorl	%edi, %edi
	subq	%rax, %rsp
	cmpl	$17744, (%rdx)
	leaq	48(%rsp), %rbx
	je	.L173
.L156:
	movl	%edi, %eax
	leaq	-56(%rbp), %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	ret
	.p2align 4,,10
.L173:
	testl	%r13d, %r13d
	mov	144(%rdx), %eax
	je	.L156
	leaq	(%r12,%rax), %rsi
	.p2align 4,,10
.L171:
	movl	16(%rsi), %eax
	testl	%eax, %eax
	je	.L156
	movl	dyndebug_runtime_link(%rip), %eax
	testl	%eax, %eax
	jne	.L174
.L157:
	mov	12(%rsi), %ecx
	addq	%r12, %rcx
	call	*__imp_GetModuleHandleA(%rip)
	testq	%rax, %rax
	movq	%rax, %r9
	je	.L158
	testl	%edi, %edi
	je	.L159
	cmpq	(%rbx), %rax
	je	.L158
	leal	-1(%rdi), %edx
	xorl	%eax, %eax
	salq	$3, %rdx
	jmp	.L160
	.p2align 4,,10
.L161:
	addq	$8, %rax
	cmpq	(%rbx,%rax), %r9
	je	.L158
.L160:
	cmpq	%rdx, %rax
	jne	.L161
.L159:
	mov	%edi, %eax
	testq	%r15, %r15
	movq	%r9, (%rbx,%rax,8)
	je	.L162
	movq	%r9, (%r15,%rax,8)
.L162:
	testq	%r14, %r14
	je	.L163
	mov	12(%rsi), %edx
	addq	%r12, %rdx
	movq	%rdx, (%r14,%rax,8)
.L163:
	movl	dyndebug_runtime_link(%rip), %eax
	testl	%eax, %eax
	jne	.L175
.L164:
	addl	$1, %edi
.L158:
	addq	$20, %rsi
	cmpl	%r13d, %edi
	jb	.L171
	jmp	.L156
	.p2align 4,,10
.L174:
	mov	12(%rsi), %eax
	movq	dyndebug_output(%rip), %rcx
	leaq	.LC0(%rip), %rdx
	leaq	(%r12,%rax), %r8
	call	fprintf
	jmp	.L157
	.p2align 4,,10
.L175:
	mov	12(%rsi), %eax
	movq	%r9, -80(%rbp)
	addq	%r12, %rax
	movq	%rax, -72(%rbp)
	call	*__imp___iob_func(%rip)
	movq	-80(%rbp), %r9
	leaq	96(%rax), %rcx
	movq	-72(%rbp), %rax
	leaq	.LC1(%rip), %rdx
	movl	%edi, %r8d
	movq	%rax, 32(%rsp)
	call	fprintf
	jmp	.L164
.L172:
	xorl	%ecx, %ecx
	call	*__imp_GetModuleHandleA(%rip)
	movq	%rax, %r12
	.p2align 4,,3
	jmp	.L155
	.seh_endproc
	.p2align 4,,15
	.globl	os_preinit
	.def	os_preinit;	.scl	2;	.type	32;	.endef
	.seh_proc	os_preinit
os_preinit:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	addq	$8, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	fiber_announce_factory
	.def	fiber_announce_factory;	.scl	2;	.type	32;	.endef
	.seh_proc	fiber_announce_factory
fiber_announce_factory:
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$32, %rsp
	.seh_stackalloc	32
	.seh_endprologue
	leaq	DEAD_MUTEX(%rip), %rbx
	movq	%rcx, %rsi
	movq	fiber_factory_lock(%rip), %rcx
	movq	%rdx, %rdi
	cmpq	%rbx, %rcx
	je	.L182
.L178:
	cmpq	$-1, %rcx
	je	.L183
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L180:
	leaq	fiber_factory_condition(%rip), %rcx
	movq	%rdi, fiber_factory_callback(%rip)
	movq	%rsi, fiber_factory_fiber(%rip)
	call	pthread_cond_broadcast
	movq	fiber_factory_lock(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.L184
.L181:
	movq	__imp_LeaveCriticalSection(%rip), %rax
	addq	$64, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	rex.W jmp *%rax
	.p2align 4,,10
.L183:
	leaq	fiber_factory_lock(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L180
	.p2align 4,,10
.L184:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rcx
	jmp	.L181
	.p2align 4,,10
.L182:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rcx
	jmp	.L178
	.seh_endproc
	.p2align 4,,15
	.globl	fiber_deinit_runtime
	.def	fiber_deinit_runtime;	.scl	2;	.type	32;	.endef
	.seh_proc	fiber_deinit_runtime
fiber_deinit_runtime:
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$112, %rsp
	.seh_stackalloc	112
	.seh_endprologue
	movq	fiber_factory_lock(%rip), %rcx
	leaq	DEAD_MUTEX(%rip), %rbx
	cmpq	%rbx, %rcx
	je	.L200
.L186:
	cmpq	$-1, %rcx
	je	.L201
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
	movq	fiber_factory_fiber(%rip), %rsi
	testq	%rsi, %rsi
	je	.L202
.L189:
	leaq	fiber_factory_condition(%rip), %rcx
	movq	$0, fiber_factory_fiber(%rip)
	call	pthread_cond_signal
	movq	fiber_factory_lock(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.L203
.L192:
	addq	$64, %rcx
	call	*__imp_LeaveCriticalSection(%rip)
	xorl	%r9d, %r9d
	movq	%rsi, %r8
	xorl	%edx, %edx
	movq	dead_fiber_queue(%rip), %rcx
	movq	__imp_PostQueuedCompletionStatus(%rip), %rbx
	call	*%rbx
	movl	dyndebug_skip_averlax(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L193
	testl	%eax, %eax
	je	.L204
.L193:
	xorl	%edx, %edx
	xorl	%r9d, %r9d
	xorl	%r8d, %r8d
	movq	dead_fiber_queue(%rip), %rcx
	call	*%rbx
	movl	dyndebug_skip_averlax(%rip), %edx
	testl	%edx, %edx
	jne	.L196
	testl	%eax, %eax
	je	.L205
.L196:
	movq	fiber_funeral_service(%rip), %rcx
	xorl	%edx, %edx
	call	pthread_join
	nop
	addq	$112, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
.L204:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbp
	movl	%eax, %esi
	call	*%rbp
	movl	(%rax), %edi
	call	*%rbp
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	$1033, %r9d
	movl	%esi, %r8d
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %ecx
	movl	%eax, %r12d
	testl	%ecx, %ecx
	je	.L194
	mov	specials(%rip), %r13d
	movq	104(%rsp), %r14
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%esi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1130, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC8(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r12d, %r12d
	je	.L193
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L193
	.p2align 4,,10
.L205:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %ebp
	movl	dyndebug_survive_aver(%rip), %eax
	testl	%eax, %eax
	je	.L197
	mov	specials(%rip), %r12d
	movq	104(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1130, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC8(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L196
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L196
	.p2align 4,,10
.L201:
	leaq	fiber_factory_lock(%rip), %rcx
	call	pthread_mutex_lock
	movq	fiber_factory_fiber(%rip), %rsi
	testq	%rsi, %rsi
	jne	.L189
	.p2align 4,,10
.L202:
	movq	fiber_factory_lock(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.L206
.L190:
	addq	$64, %rcx
	call	*__imp_LeaveCriticalSection(%rip)
	nop
	addq	$112, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
.L203:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rcx
	jmp	.L192
	.p2align 4,,10
.L200:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rcx
	jmp	.L186
	.p2align 4,,10
.L206:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rcx
	jmp	.L190
.L197:
	mov	specials(%rip), %ebp
	movq	104(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	movq	1432(%rax,%rbp,8), %rax
.L199:
	leaq	.LC8(%rip), %r9
	leaq	.LC3(%rip), %rdx
	leaq	.LC4(%rip), %rcx
	movl	$1130, %r8d
	movq	%rax, 40(%rsp)
	movq	$0, 32(%rsp)
	call	lose
.L194:
	mov	specials(%rip), %ebx
	movq	104(%rsp), %r12
	call	pthread_self
	movq	%rbp, 72(%rsp)
	movl	%edi, 64(%rsp)
	movq	%r12, 56(%rsp)
	movl	%esi, 48(%rsp)
	movq	1432(%rax,%rbx,8), %rax
	jmp	.L199
	.seh_endproc
	.p2align 4,,15
	.globl	fff_generic_callback
	.def	fff_generic_callback;	.scl	2;	.type	32;	.endef
	.seh_proc	fff_generic_callback
fff_generic_callback:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$152, %rsp
	.seh_stackalloc	152
	.seh_endprologue
	movq	%rcx, %r12
	movq	%rdx, %rbp
	movq	%r8, %rdi
	call	pthread_np_notice_thread
	mov	specials(%rip), %esi
	call	pthread_self
	movq	%rax, %rbx
	movq	1432(%rax,%rsi,8), %rax
	testq	%rax, %rax
	je	.L208
	movq	240(%rax), %rdx
	xorl	%r13d, %r13d
	movq	(%rdx), %rsi
	testq	%rsi, %rsi
	je	.L209
	movq	$0, (%rdx)
	movq	248(%rax), %r13
	movq	$0, 248(%rax)
.L209:
	movq	537923376, %rdx
	testq	%rdx, %rdx
	jne	.L231
.L210:
	movq	537923336, %rcx
.L211:
	movq	%rdi, %r9
	movq	%rbp, %r8
	movq	%r12, %rdx
	call	funcall3
	mov	specials(%rip), %eax
	testq	%rsi, %rsi
	movq	1432(%rbx,%rax,8), %rax
	je	.L207
	movq	240(%rax), %rdx
	movq	%rsi, (%rdx)
	movq	%r13, 248(%rax)
.L207:
	addq	$152, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L231:
	shrq	$3, %rdx
	movq	(%rax,%rdx,8), %rcx
	cmpq	$98, %rcx
	jne	.L211
	jmp	.L210
	.p2align 4,,10
.L208:
	call	pthread_np_notice_thread
	mov	lisp_fiber_key(%rip), %eax
	addq	$178, %rax
	movq	8(%rbx,%rax,8), %rcx
	testq	%rcx, %rcx
	movq	%rcx, 128(%rsp)
	je	.L232
	cmpq	$0, fiber_factory_fiber(%rip)
	je	.L233
.L221:
	leaq	96(%rsp), %r8
	leaq	fff_foreign_callback(%rip), %rdx
	movq	%r12, 96(%rsp)
	movq	%rbp, 104(%rsp)
	movq	%rdi, 112(%rsp)
	movl	$0, 120(%rsp)
	call	pthread_np_run_in_fiber
	testl	%eax, %eax
	jne	.L234
.L223:
	movl	120(%rsp), %r11d
	testl	%r11d, %r11d
	je	.L235
	cmpq	$0, fiber_factory_fiber(%rip)
	jne	.L207
	mov	lisp_fiber_key(%rip), %eax
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	movq	128(%rsp), %r8
	movq	dead_fiber_queue(%rip), %rcx
	movq	$0, 1432(%rbx,%rax,8)
	call	*__imp_PostQueuedCompletionStatus(%rip)
	movl	dyndebug_skip_averlax(%rip), %r10d
	testl	%r10d, %r10d
	jne	.L207
	testl	%eax, %eax
	jne	.L207
	leaq	.LC2(%rip), %rax
	movq	%rax, 136(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbp
	movl	%eax, %esi
	call	*%rbp
	movl	(%rax), %edi
	call	*%rbp
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	136(%rsp), %rax
	movl	$1033, %r9d
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	%esi, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r9d
	movl	%eax, %r12d
	testl	%r9d, %r9d
	je	.L225
	movq	136(%rsp), %rax
	movq	%rbp, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%edi, 72(%rsp)
	movl	%esi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	dyndebug_output(%rip), %rcx
	movl	$1130, %r9d
	movq	%rax, 64(%rsp)
	mov	specials(%rip), %eax
	movq	1432(%rbx,%rax,8), %rax
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC8(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r12d, %r12d
	je	.L207
	movq	136(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L207
	.p2align 4,,10
.L234:
	mov	lisp_fiber_key(%rip), %eax
	movl	$-1, %ecx
	movq	$0, 1432(%rbx,%rax,8)
	call	*__imp_Sleep(%rip)
	jmp	.L223
	.p2align 4,,10
.L232:
	movq	fiber_factory_lock(%rip), %rax
	leaq	DEAD_MUTEX(%rip), %rsi
	cmpq	%rsi, %rax
	je	.L236
.L214:
	cmpq	$-1, %rax
	je	.L237
	leaq	64(%rax), %rcx
	call	*__imp_EnterCriticalSection(%rip)
	jmp	.L230
	.p2align 4,,10
.L228:
	leaq	fiber_factory_lock(%rip), %rdx
	leaq	fiber_factory_condition(%rip), %rcx
	call	pthread_cond_wait
.L230:
	movq	fiber_factory_fiber(%rip), %rcx
	testq	%rcx, %rcx
	je	.L228
	movq	fiber_factory_callback(%rip), %rdx
	leaq	128(%rsp), %r8
	call	pthread_np_run_in_fiber
	movq	fiber_factory_lock(%rip), %rax
	cmpq	%rsi, %rax
	je	.L238
.L219:
	leaq	64(%rax), %rcx
	call	*__imp_LeaveCriticalSection(%rip)
	mov	lisp_fiber_key(%rip), %eax
	movq	128(%rsp), %rcx
	movq	%rcx, 1432(%rbx,%rax,8)
.L220:
	testq	%rcx, %rcx
	jne	.L221
	movl	$1, %ecx
	call	*__imp_ExitThread(%rip)
	.p2align 4,,10
.L233:
	movl	$-1, %ecx
	movq	$0, 8(%rbx,%rax,8)
	call	*__imp_Sleep(%rip)
	movq	128(%rsp), %rcx
	jmp	.L220
.L237:
	leaq	fiber_factory_lock(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L230
.L238:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rax
	jmp	.L219
.L236:
	leaq	fiber_factory_lock(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movb	$5, %cl
	call	pthread_np_lose
	movq	fiber_factory_lock(%rip), %rax
	jmp	.L214
.L235:
	mov	lisp_fiber_key(%rip), %eax
	movl	$1, %ecx
	movq	$0, 1432(%rbx,%rax,8)
	call	*__imp_ExitThread(%rip)
.L225:
	movq	136(%rsp), %rax
	movq	%rbp, 72(%rsp)
	leaq	.LC8(%rip), %r9
	movl	%edi, 64(%rsp)
	movl	%esi, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	leaq	.LC4(%rip), %rcx
	movl	$1130, %r8d
	movq	%rax, 56(%rsp)
	mov	specials(%rip), %eax
	movq	1432(%rbx,%rax,8), %rax
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
.LC17:
	.ascii "SBCL_DYNDEBUG__LAZY_FPU\0"
	.align 8
.LC18:
	.ascii "SBCL_DYNDEBUG__LAZY_FPU_CAREFUL\0"
.LC19:
	.ascii "SBCL_DYNDEBUG__SKIP_AVERLAX\0"
.LC20:
	.ascii "SBCL_DYNDEBUG__SURVIVE_AVER\0"
.LC21:
	.ascii "SBCL_DYNDEBUG__RUNTIME_LINK\0"
.LC22:
	.ascii "SBCL_DYNDEBUG__SAFEPOINTS\0"
.LC23:
	.ascii "SBCL_DYNDEBUG__PAGEFAULTS\0"
.LC24:
	.ascii "SBCL_DYNDEBUG__IO\0"
.LC25:
	.ascii "SBCL_DYNDEBUG__SEH\0"
.LC26:
	.ascii "kernel32\0"
.LC27:
	.ascii "GetWriteWatch\0"
.LC28:
	.ascii "ResetWriteWatch\0"
.LC29:
	.ascii "CancelIoEx\0"
.LC30:
	.ascii "CancelSynchronousIo\0"
	.text
	.p2align 4,,15
	.globl	os_init
	.def	os_init;	.scl	2;	.type	32;	.endef
	.seh_proc	os_init
os_init:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	.seh_endprologue
	movl	$480, %ecx
	call	*__imp__set_sbh_threshold(%rip)
	leaq	96(%rsp), %rcx
	call	*__imp_GetSystemInfo(%rip)
	cmpl	$32768, 100(%rsp)
	movl	$32768, %eax
	cmovae	100(%rsp), %eax
	mov	%eax, %eax
	movq	%rax, os_vm_page_size(%rip)
	movl	128(%rsp), %eax
	movl	%eax, os_number_of_processors(%rip)
	call	*__imp___iob_func(%rip)
	addq	$96, %rax
	movq	__imp_GetEnvironmentVariableA(%rip), %rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC17(%rip), %rcx
	movq	%rax, dyndebug_output(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC18(%rip), %rcx
	movl	%eax, dyndebug_lazy_fpu(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC19(%rip), %rcx
	movl	%eax, dyndebug_lazy_fpu_careful(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC20(%rip), %rcx
	movl	%eax, dyndebug_skip_averlax(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC21(%rip), %rcx
	movl	%eax, dyndebug_survive_aver(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC22(%rip), %rcx
	movl	%eax, dyndebug_runtime_link(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC23(%rip), %rcx
	movl	%eax, dyndebug_safepoints(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC24(%rip), %rcx
	movl	%eax, dyndebug_pagefaults(%rip)
	call	*%rbx
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	leaq	.LC25(%rip), %rcx
	movl	%eax, dyndebug_io(%rip)
	call	*%rbx
	movl	%eax, dyndebug_seh(%rip)
/APP
 # 405 "win32-os.c" 1
	mov %gs:0,%rax
 # 0 "" 2
/NO_APP
	leaq	save_lisp_tls_key(%rip), %rcx
	xorl	%edx, %edx
	movq	%rax, base_seh_frame(%rip)
	call	pthread_key_create
	leaq	fiber_is_dead(%rip), %rdx
	leaq	lisp_fiber_key(%rip), %rcx
	call	pthread_key_create
	movl	$524319, %edx
	movl	$-17, %ecx
	call	*__imp__controlfp(%rip)
	leaq	save_lisp_tls(%rip), %rax
	leaq	fiber_factory_condition(%rip), %rcx
	xorl	%edx, %edx
	movq	%rax, pthread_save_context_hook(%rip)
	leaq	restore_lisp_tls(%rip), %rax
	movq	%rax, pthread_restore_context_hook(%rip)
	call	pthread_cond_init
	leaq	fiber_factory_lock(%rip), %rcx
	xorl	%edx, %edx
	call	pthread_mutex_init
	movl	$4, %r9d
	movl	$12288, %r8d
	movl	$8, %edx
	movl	$553648128, %ecx
	call	*__imp_VirtualAlloc(%rip)
	movl	dyndebug_skip_averlax(%rip), %r13d
	testl	%r13d, %r13d
	jne	.L240
	testq	%rax, %rax
	je	.L247
.L240:
	xorl	%r8d, %r8d
	xorl	%edx, %edx
	movl	$1, %r9d
	movq	$-1, %rcx
	call	*__imp_CreateIoCompletionPort(%rip)
	leaq	fiber_funeral_function(%rip), %r8
	leaq	fiber_funeral_service(%rip), %rcx
	xorl	%edx, %edx
	movq	%rax, %r9
	movq	%rax, dead_fiber_queue(%rip)
	call	pthread_create
	leaq	.LC26(%rip), %rcx
	call	*__imp_GetModuleHandleA(%rip)
	testq	%rax, %rax
	movq	%rax, %rbx
	je	.L243
	movq	__imp_GetProcAddress(%rip), %rsi
	leaq	.LC27(%rip), %rdx
	movq	%rax, %rcx
	call	*%rsi
	leaq	.LC28(%rip), %rdx
	movq	%rbx, %rcx
	movq	%rax, ptr_GetWriteWatch(%rip)
	call	*%rsi
	leaq	.LC29(%rip), %rdx
	movq	%rbx, %rcx
	movq	%rax, ptr_ResetWriteWatch(%rip)
	call	*%rsi
	leaq	.LC30(%rip), %rdx
	movq	%rax, ptr_CancelIoEx(%rip)
	movq	%rbx, %rcx
	call	*%rsi
	movq	%rax, ptr_CancelSynchronousIo(%rip)
.L243:
	xorl	%eax, %eax
	cmpq	$0, ptr_GetWriteWatch(%rip)
	je	.L244
	cmpq	$1, ptr_ResetWriteWatch(%rip)
	sbbl	%eax, %eax
	notl	%eax
	andl	$2097152, %eax
.L244:
	movl	%eax, mwwFlag(%rip)
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L247:
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r12d
	movl	%eax, %ebp
	testl	%r12d, %r12d
	je	.L241
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$455, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC12(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L240
	movq	152(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L240
.L241:
	mov	specials(%rip), %ebp
	movq	152(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC12(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$455, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC31:
	.ascii "!ptr_GetWriteWatch(0, ww_start,ww_size, writeAddrs,&nAddrs,&granularity)\0"
	.align 8
.LC32:
	.ascii "Write watch caught %u minipages in %u macropages\12\0"
	.text
	.p2align 4,,15
	.globl	os_commit_wp_violation_data
	.def	os_commit_wp_violation_data;	.scl	2;	.type	32;	.endef
	.seh_proc	os_commit_wp_violation_data
os_commit_wp_violation_data:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$152, %rsp
	.seh_stackalloc	152
	.seh_endprologue
	movabsq	$68719476736, %rax
	movl	%ecx, %edi
	movq	core_mmap_end(%rip), %rcx
	testq	%rcx, %rcx
	cmove	%rax, %rcx
	call	find_page_index
	movq	page_table_pages(%rip), %r9
	movslq	%eax, %rdx
	cmpq	%r9, %rdx
	jge	.L248
	movq	%rdx, %rcx
	movl	$-1, %ebx
	subq	%rdx, %r9
	salq	$4, %rcx
	xorl	%edx, %edx
	addq	page_table(%rip), %rcx
	movl	%ebx, %esi
	jmp	.L252
	.p2align 4,,10
.L251:
	addq	$1, %rdx
	addq	$16, %rcx
	cmpq	%r9, %rdx
	je	.L268
.L252:
	testb	$1, 10(%rcx)
	leal	(%rdx,%rax), %r8d
	je	.L251
	cmpl	$-1, %esi
	movl	%r8d, %ebx
	jne	.L251
	addq	$1, %rdx
	addq	$16, %rcx
	movl	%r8d, %esi
	cmpq	%r9, %rdx
	jne	.L252
.L268:
	cmpl	$-1, %esi
	je	.L248
	movslq	%esi, %rsi
	movq	%rsi, %rcx
	call	page_address
	movslq	%ebx, %rcx
	movq	%rax, %r12
	call	page_address
	movq	%rsi, %rcx
	movq	%rax, %rbx
	call	page_address
	movl	mwwFlag(%rip), %edx
	testl	%edx, %edx
	je	.L248
	testl	%edi, %edi
	je	.L248
	movq	%rbx, %r8
	leaq	140(%rsp), %rbp
	leaq	112(%rsp), %rdi
	subq	%rax, %r8
	xorl	%ebx, %ebx
	xorl	%esi, %esi
	addq	$32768, %r8
.L261:
	xorl	%ecx, %ecx
	movq	$512, 112(%rsp)
	movq	%rbp, 40(%rsp)
	movq	%rdi, 32(%rsp)
	leaq	writeAddrs.75962(%rip), %r9
	movq	%r12, %rdx
	call	*ptr_GetWriteWatch(%rip)
	movl	dyndebug_skip_averlax(%rip), %r15d
	testl	%r15d, %r15d
	jne	.L253
	testl	%eax, %eax
	jne	.L269
.L253:
	movq	112(%rsp), %r15
	testq	%r15, %r15
	je	.L258
	movq	$0, 104(%rsp)
	xorl	%r12d, %r12d
	movq	$-1, %r14
	jmp	.L257
	.p2align 4,,10
.L256:
	addq	os_vm_page_size(%rip), %r13
	movq	%r13, 104(%rsp)
.L255:
	addq	$1, %r12
	cmpq	%r15, %r12
	je	.L270
.L257:
	leaq	writeAddrs.75962(%rip), %rax
	movq	(%rax,%r12,8), %r13
	movq	%r13, %rcx
	call	find_page_index
	cmpq	%rax, %r14
	je	.L255
	movq	%r13, %rcx
	call	find_page_index
	movslq	%eax, %r14
	movq	%r14, %rax
	salq	$4, %rax
	addq	page_table(%rip), %rax
	testb	$1, 10(%rax)
	je	.L256
	movq	%r13, %rcx
	addq	$1, %rbx
	call	gencgc_handle_wp_violation
	jmp	.L256
.L258:
	movl	dyndebug_pagefaults(%rip), %r14d
	testl	%r14d, %r14d
	je	.L248
	leaq	.LC32(%rip), %rcx
	movq	%rbx, %r8
	movl	%esi, %edx
	call	odprintf_
	nop
.L248:
	addq	$152, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
.L270:
	movq	112(%rsp), %rax
	addl	%eax, %esi
	cmpq	$512, %rax
	jne	.L258
	movq	104(%rsp), %rcx
	call	find_page_index
	movq	page_table_pages(%rip), %r9
	movslq	%eax, %rdx
	cmpq	%rdx, %r9
	jle	.L258
	movq	%rdx, %rcx
	movl	$-1, %r13d
	subq	%rdx, %r9
	salq	$4, %rcx
	xorl	%edx, %edx
	addq	page_table(%rip), %rcx
	movl	%r13d, %r14d
	jmp	.L260
	.p2align 4,,10
.L259:
	addq	$1, %rdx
	addq	$16, %rcx
	cmpq	%r9, %rdx
	je	.L271
.L260:
	testb	$1, 10(%rcx)
	leal	(%rdx,%rax), %r8d
	je	.L259
	cmpl	$-1, %r14d
	movl	%r8d, %r13d
	jne	.L259
	addq	$1, %rdx
	addq	$16, %rcx
	movl	%r8d, %r14d
	cmpq	%r9, %rdx
	jne	.L260
.L271:
	cmpl	$-1, %r14d
	je	.L258
	movslq	%r14d, %r14
	movq	%r14, %rcx
	call	page_address
	movslq	%r13d, %rcx
	movq	%rax, %r12
	call	page_address
	movq	%r14, %rcx
	movq	%rax, %r13
	call	page_address
	subq	%rax, %r13
	leaq	32768(%r13), %r8
	jmp	.L261
.L269:
	leaq	.LC2(%rip), %rax
	movq	%rax, 128(%rsp)
	call	*__imp_GetLastError(%rip)
	movl	%eax, %r12d
	call	*__imp__errno(%rip)
	movl	(%rax), %r14d
	call	*__imp__errno(%rip)
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r13
	leaq	128(%rsp), %rax
	movl	$1033, %r9d
	movl	%r12d, %r8d
	xorl	%edx, %edx
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %r15d
	mov	specials(%rip), %eax
	movq	128(%rsp), %rdx
	movq	%rdx, 96(%rsp)
	movq	%rax, 104(%rsp)
	call	pthread_self
	movq	96(%rsp), %rdx
	movq	%r13, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r14d, 72(%rsp)
	movl	%r12d, 56(%rsp)
	movl	$1523, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	104(%rsp), %rdx
	movq	1432(%rax,%rdx,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC31(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r15d, %r15d
	je	.L253
	movq	128(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L253
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC33:
	.ascii "VirtualAlloc(addr, len, MEM_RESERVE|MEM_COMMIT, PAGE_EXECUTE_READWRITE)\0"
	.align 8
.LC34:
	.ascii "VirtualQuery(addr, &mem_info, sizeof mem_info)\0"
	.align 8
.LC35:
	.ascii "validation of reserved space too short.\12\0"
	.align 8
.LC36:
	.ascii "VirtualAlloc(addr, len, (mem_info.State == MEM_RESERVE)? MEM_COMMIT: MEM_RESERVE|memWatch, PAGE_EXECUTE_READWRITE) ||(memWatch=0,mwwFlag=0,VirtualAlloc(addr, len, (mem_info.State == MEM_RESERVE)? MEM_COMMIT: MEM_RESERVE, PAGE_EXECUTE_READWRITE))\0"
	.text
	.p2align 4,,15
	.globl	os_validate
	.def	os_validate;	.scl	2;	.type	32;	.endef
	.seh_proc	os_validate
os_validate:
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$160, %rsp
	.seh_stackalloc	160
	.seh_endprologue
	movl	mwwFlag(%rip), %ebp
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	movq	%rdx, %rsi
	je	.L314
	movq	core_mmap_end(%rip), %rcx
	testq	%rcx, %rcx
	je	.L283
	leaq	(%rbx,%rdx), %r12
	cmpq	%r12, %rcx
	jae	.L277
	cmpq	%rbx, %rcx
	ja	.L315
.L277:
	testq	%rcx, %rcx
	movabsq	$68719476735, %rax
	setne	%dl
	cmpq	%rax, %r12
	jbe	.L309
	testb	%dl, %dl
	jne	.L278
.L309:
	movabsq	$68719476735, %rax
	cmpq	%rax, %rbx
	seta	%r13b
.L280:
	testb	%r13b, %r13b
	je	.L283
	testb	%dl, %dl
	je	.L283
	cmpq	%rbx, %rcx
	movq	%rbx, %rdi
	ja	.L274
.L283:
	movq	%rbx, %rcx
	movl	$48, %r8d
	leaq	96(%rsp), %rdx
	call	*__imp_VirtualQuery(%rip)
	movl	dyndebug_skip_averlax(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L316
	testq	%rax, %rax
	je	.L317
.L284:
	movl	128(%rsp), %eax
	cmpl	$8192, %eax
	je	.L318
.L285:
	movabsq	$-68719476736, %rcx
	movabsq	$5242879999, %rdx
	xorl	%r8d, %r8d
	addq	%rbx, %rcx
	movl	$64, %r9d
	movq	__imp_VirtualAlloc(%rip), %rdi
	cmpq	%rdx, %rcx
	movq	%rsi, %rdx
	movq	%rbx, %rcx
	cmovbe	%ebp, %r8d
	movl	$4096, %ebp
	orl	$8192, %r8d
	cmpl	$8192, %eax
	cmove	%ebp, %r8d
	call	*%rdi
	testq	%rax, %rax
	movl	$1, %edx
	je	.L319
.L289:
	movl	dyndebug_skip_averlax(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L291
	testl	%edx, %edx
	movq	%rbx, %rdi
	jne	.L274
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1787, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC36(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	jne	.L320
.L308:
	xorl	%edi, %edi
	.p2align 4,,10
.L274:
	movq	%rdi, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
.L278:
	cmpq	%rcx, %r12
	jb	.L281
	cmpq	%rax, %rbx
	movl	$1, %edx
	seta	%r13b
	jmp	.L280
	.p2align 4,,10
.L281:
	cmpq	%rax, %rbx
	seta	%r13b
	ja	.L321
.L282:
	movabsq	$-68719476736, %rdx
	movabsq	$68719476736, %rcx
	addq	%r12, %rdx
	call	os_validate
	testq	%rax, %rax
	je	.L308
	movq	core_mmap_end(%rip), %rcx
	movabsq	$68719476736, %rsi
	subq	%rbx, %rsi
	testq	%rcx, %rcx
	setne	%dl
	jmp	.L280
	.p2align 4,,10
.L321:
	cmpq	%rbx, %rcx
	movq	%rbx, %rdi
	ja	.L274
	jmp	.L282
	.p2align 4,,10
.L316:
	testq	%rax, %rax
	jne	.L284
	xorl	%edi, %edi
	.p2align 4,,6
	jmp	.L274
	.p2align 4,,10
.L291:
	testl	%edx, %edx
	je	.L308
	movq	%rbx, %rdi
	.p2align 4,,4
	jmp	.L274
	.p2align 4,,10
.L315:
	movq	%r12, %rdx
	subq	%rcx, %rdx
	call	os_validate
	testq	%rax, %rax
	je	.L308
	movq	core_mmap_end(%rip), %rcx
	movq	%rcx, %rax
	subq	%r12, %rax
	addq	%rax, %rsi
	leaq	(%rbx,%rsi), %r12
	jmp	.L277
	.p2align 4,,10
.L317:
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movl	%edi, 72(%rsp)
	movq	%rbp, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1750, %r9d
	movq	$0, 40(%rsp)
	xorl	%edi, %edi
	movq	%rax, 48(%rsp)
	leaq	.LC34(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	je	.L274
.L312:
	movq	152(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L274
	.p2align 4,,10
.L318:
	cmpq	%rsi, 120(%rsp)
	jae	.L322
	movq	__imp___iob_func(%rip), %rdi
	call	*%rdi
	leaq	.LC35(%rip), %rcx
	leaq	96(%rax), %r9
	movl	$40, %r8d
	movl	$1, %edx
	call	fwrite
	call	*%rdi
	leaq	96(%rax), %rcx
	call	fflush
	movl	128(%rsp), %eax
	jmp	.L285
	.p2align 4,,10
.L320:
	movq	152(%rsp), %rcx
	xorl	%edi, %edi
	call	*__imp_LocalFree(%rip)
	jmp	.L274
	.p2align 4,,10
.L319:
	cmpl	$8192, 128(%rsp)
	movl	$8192, %r8d
	movq	%rsi, %rdx
	movl	$0, mwwFlag(%rip)
	movl	$64, %r9d
	movq	%rbx, %rcx
	cmove	%ebp, %r8d
	call	*%rdi
	xorl	%edx, %edx
	testq	%rax, %rax
	setne	%dl
	jmp	.L289
	.p2align 4,,10
.L314:
	movl	$64, %r9d
	xorl	%ecx, %ecx
	movl	$12288, %r8d
	call	*__imp_VirtualAlloc(%rip)
	movl	dyndebug_skip_averlax(%rip), %r9d
	movq	%rax, %rdi
	testl	%r9d, %r9d
	jne	.L274
	testq	%rax, %rax
	jne	.L274
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %ebp
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r13d
	movq	152(%rsp), %r14
	movl	%eax, %esi
	call	pthread_self
	movq	%r12, 80(%rsp)
	movl	%ebp, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1733, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC33(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	je	.L274
	jmp	.L312
	.p2align 4,,10
.L322:
	movl	mwwFlag(%rip), %r8d
	movl	$64, %r9d
	movq	%rsi, %rdx
	movq	%rbx, %rcx
	movq	%rbx, %rdi
	orl	$8192, %r8d
	call	*__imp_VirtualAlloc(%rip)
	jmp	.L274
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC37:
	.ascii "VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE)\0"
	.text
	.p2align 4,,15
	.globl	os_validate_recommit
	.def	os_validate_recommit;	.scl	2;	.type	32;	.endef
	.seh_proc	os_validate_recommit
os_validate_recommit:
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$112, %rsp
	.seh_stackalloc	112
	.seh_endprologue
	movq	%rcx, %rbx
	movq	core_mmap_end(%rip), %rcx
	movq	%rdx, %rsi
	testq	%rcx, %rcx
	je	.L324
	leaq	(%rbx,%rdx), %rdi
	cmpq	%rdi, %rcx
	jae	.L326
	cmpq	%rbx, %rcx
	ja	.L340
.L326:
	testq	%rcx, %rcx
	movabsq	$68719476735, %rax
	setne	%dl
	cmpq	%rax, %rdi
	jbe	.L337
	testb	%dl, %dl
	jne	.L328
.L337:
	movabsq	$68719476735, %rax
	cmpq	%rax, %rbx
	seta	%bpl
.L330:
	testb	%bpl, %bpl
	je	.L324
	testb	%dl, %dl
	je	.L324
	cmpq	%rbx, %rcx
	ja	.L327
.L324:
	movq	%rbx, %rcx
	movl	$64, %r9d
	movl	$4096, %r8d
	movq	%rsi, %rdx
	call	*__imp_VirtualAlloc(%rip)
	movl	dyndebug_skip_averlax(%rip), %r10d
	movq	%rax, %rbx
	testl	%r10d, %r10d
	jne	.L327
	testq	%rbx, %rbx
	je	.L341
	.p2align 4,,10
.L327:
	movq	%rbx, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
.L328:
	cmpq	%rcx, %rdi
	jb	.L331
	cmpq	%rax, %rbx
	movl	$1, %edx
	seta	%bpl
	jmp	.L330
	.p2align 4,,10
.L331:
	cmpq	%rax, %rbx
	seta	%bpl
	ja	.L342
.L332:
	movabsq	$-68719476736, %rdx
	movabsq	$68719476736, %rcx
	addq	%rdi, %rdx
	call	os_validate_recommit
	testq	%rax, %rax
	je	.L336
	movq	core_mmap_end(%rip), %rcx
	movabsq	$68719476736, %rsi
	subq	%rbx, %rsi
	testq	%rcx, %rcx
	setne	%dl
	jmp	.L330
	.p2align 4,,10
.L342:
	cmpq	%rcx, %rbx
	jb	.L327
	jmp	.L332
	.p2align 4,,10
.L340:
	movq	%rdi, %rdx
	subq	%rcx, %rdx
	call	os_validate_recommit
	testq	%rax, %rax
	je	.L336
	movq	core_mmap_end(%rip), %rcx
	movq	%rcx, %rax
	subq	%rdi, %rax
	addq	%rax, %rsi
	leaq	(%rbx,%rsi), %rdi
	jmp	.L326
	.p2align 4,,10
.L341:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %esi
	call	*%rdi
	movl	(%rax), %ebp
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%esi, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r13d
	movq	104(%rsp), %r14
	movl	%eax, %edi
	call	pthread_self
	movq	%r12, 80(%rsp)
	movl	%ebp, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%esi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1801, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC37(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%edi, %edi
	je	.L327
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L327
.L336:
	xorl	%ebx, %ebx
	jmp	.L327
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC38:
	.ascii "Got library %u/%u with base %p\12\0"
	.align 8
.LC39:
	.ascii "Core requests linking [0x%p] <= [%s]: => [0x%p]\12\0"
	.align 8
.LC40:
	.ascii "Core requests linking [0x%p] <= [%s]: failed\12\0"
	.text
	.p2align 4,,15
	.globl	os_link_runtime
	.def	os_link_runtime;	.scl	2;	.type	32;	.endef
	.seh_proc	os_link_runtime
os_link_runtime:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$200, %rsp
	.seh_stackalloc	200
	.seh_endprologue
	xorl	%ecx, %ecx
	leaq	64(%rsp), %rdi
	call	*__imp_GetModuleHandleW(%rip)
	movl	$16, %ecx
	movq	%rax, %r8
	xorl	%eax, %eax
	rep stosq
	leaq	72(%rsp), %rdx
	movq	%r8, %rcx
	movq	%r8, 64(%rsp)
	call	os_get_build_time_shared_libraries.constprop.7
	leal	1(%rax), %edi
	movl	dyndebug_runtime_link(%rip), %eax
	testl	%eax, %eax
	je	.L344
	testl	%edi, %edi
	je	.L344
	xorl	%ebx, %ebx
	.p2align 4,,10
.L345:
	movq	64(%rsp,%rbx,8), %rax
	movq	dyndebug_output(%rip), %rcx
	leaq	.LC38(%rip), %rdx
	movl	%ebx, %r8d
	movl	%edi, %r9d
	addq	$1, %rbx
	movq	%rax, 32(%rsp)
	call	fprintf
	cmpl	%ebx, %edi
	ja	.L345
.L344:
	movq	537921096, %rax
	cmpq	$537919511, %rax
	je	.L343
	movq	__imp_GetProcAddress(%rip), %r12
	movl	$538968064, %r13d
	.p2align 4,,10
.L357:
	movq	os_vm_page_size(%rip), %rdx
	leaq	-7(%rax), %r14
	movq	-7(%rax), %rbx
	leaq	-1(%rdx), %rax
	testq	%rax, %r13
	je	.L361
.L347:
	xorl	%r15d, %r15d
	cmpq	$537919511, 1(%rbx)
	movq	-7(%rbx), %rbp
	setne	%r15b
	addq	$1, %rbp
	testl	%edi, %edi
	je	.L348
	leaq	64(%rsp), %r8
	xorl	%ebx, %ebx
	jmp	.L353
	.p2align 4,,10
.L349:
	addl	$1, %ebx
	addq	$8, %r8
	cmpl	%ebx, %edi
	je	.L348
.L353:
	movq	(%r8), %rcx
	movq	%rbp, %rdx
	movq	%r8, 56(%rsp)
	call	*%r12
	testq	%rax, %rax
	movq	%rax, %rsi
	movq	56(%rsp), %r8
	je	.L349
	movl	dyndebug_runtime_link(%rip), %eax
	testl	%eax, %eax
	jne	.L362
.L350:
	testl	%r15d, %r15d
	movq	%rsi, %rdx
	movq	%r13, %rcx
	je	.L351
	call	arch_write_linkage_table_ref
.L352:
	cmpl	%edi, %ebx
	je	.L348
.L354:
	movq	8(%r14), %rax
	addq	$16, %r13
	cmpq	$537919511, %rax
	jne	.L357
.L343:
	addq	$200, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L361:
	movq	%r13, %rcx
	call	os_validate_recommit
	jmp	.L347
	.p2align 4,,10
.L348:
	movl	dyndebug_runtime_link(%rip), %r11d
	testl	%r11d, %r11d
	jne	.L363
.L355:
	testl	%r15d, %r15d
	je	.L356
	movq	undefined_alien_address(%rip), %rdx
	movq	%r13, %rcx
	call	arch_write_linkage_table_ref
	jmp	.L354
	.p2align 4,,10
.L351:
	call	arch_write_linkage_table_jmp
	.p2align 4,,8
	jmp	.L352
	.p2align 4,,10
.L362:
	movq	dyndebug_output(%rip), %rcx
	leaq	.LC39(%rip), %rdx
	movq	%rbp, %r9
	movq	%r13, %r8
	movq	%rsi, 32(%rsp)
	call	fprintf
	jmp	.L350
	.p2align 4,,10
.L356:
	leaq	undefined_alien_function(%rip), %rdx
	movq	%r13, %rcx
	call	arch_write_linkage_table_jmp
	jmp	.L354
.L363:
	call	*__imp___iob_func(%rip)
	leaq	.LC40(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%rbp, %r9
	movq	%r13, %r8
	call	fprintf
	jmp	.L355
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC41:
	.ascii "VirtualAlloc(NULL, len, MEM_RESERVE, PAGE_EXECUTE_READWRITE)\0"
	.text
	.p2align 4,,15
	.globl	os_allocate_lazily
	.def	os_allocate_lazily;	.scl	2;	.type	32;	.endef
	.seh_proc	os_allocate_lazily
os_allocate_lazily:
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$112, %rsp
	.seh_stackalloc	112
	.seh_endprologue
	movl	$64, %r9d
	movl	$8192, %r8d
	movq	%rcx, %rdx
	xorl	%ecx, %ecx
	call	*__imp_VirtualAlloc(%rip)
	movq	%rax, %rbx
	movl	dyndebug_skip_averlax(%rip), %eax
	testl	%eax, %eax
	jne	.L365
	testq	%rbx, %rbx
	je	.L366
.L365:
	movq	%rbx, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
.L366:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %esi
	call	*%rdi
	movl	(%rax), %ebp
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%esi, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r13d
	movq	104(%rsp), %r14
	movl	%eax, %edi
	call	pthread_self
	movq	%r12, 80(%rsp)
	movl	%ebp, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%esi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1810, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC41(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%edi, %edi
	je	.L365
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L365
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC42:
	.ascii "VirtualFree(addr, len, MEM_DECOMMIT)\0"
	.text
	.p2align 4,,15
	.globl	os_invalidate
	.def	os_invalidate;	.scl	2;	.type	32;	.endef
	.seh_proc	os_invalidate
os_invalidate:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$280, %rsp
	.seh_stackalloc	280
	movaps	%xmm6, 112(%rsp)
	.seh_savexmm	%xmm6, 112
	movaps	%xmm7, 128(%rsp)
	.seh_savexmm	%xmm7, 128
	movaps	%xmm8, 144(%rsp)
	.seh_savexmm	%xmm8, 144
	movaps	%xmm9, 160(%rsp)
	.seh_savexmm	%xmm9, 160
	movaps	%xmm10, 176(%rsp)
	.seh_savexmm	%xmm10, 176
	movaps	%xmm11, 192(%rsp)
	.seh_savexmm	%xmm11, 192
	movaps	%xmm12, 208(%rsp)
	.seh_savexmm	%xmm12, 208
	movaps	%xmm13, 224(%rsp)
	.seh_savexmm	%xmm13, 224
	movaps	%xmm14, 240(%rsp)
	.seh_savexmm	%xmm14, 240
	movaps	%xmm15, 256(%rsp)
	.seh_savexmm	%xmm15, 256
	.seh_endprologue
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	core_mmap_end(%rip), %rcx
	testq	%rcx, %rcx
	je	.L368
	leaq	(%rdi,%rdx), %rbx
	cmpq	%rbx, %rcx
	jae	.L370
	cmpq	%rdi, %rcx
	ja	.L379
.L370:
	testq	%rcx, %rcx
	movabsq	$68719476735, %rdx
	setne	%al
	cmpq	%rdx, %rbx
	jbe	.L371
	testb	%al, %al
	je	.L371
	cmpq	%rcx, %rbx
	jb	.L372
	cmpq	%rdx, %rdi
	movl	$1, %eax
	seta	%bpl
.L373:
	testb	%bpl, %bpl
	je	.L368
	testb	%al, %al
	je	.L368
	cmpq	%rdi, %rcx
	ja	.L375
.L368:
	movq	%rsi, %rdx
	movl	$16384, %r8d
	movq	%rdi, %rcx
	call	*__imp_VirtualFree(%rip)
	movl	dyndebug_skip_averlax(%rip), %edx
	testl	%edx, %edx
	jne	.L367
	testl	%eax, %eax
	je	.L380
.L367:
	movaps	112(%rsp), %xmm6
	movaps	128(%rsp), %xmm7
	movaps	144(%rsp), %xmm8
	movaps	160(%rsp), %xmm9
	movaps	176(%rsp), %xmm10
	movaps	192(%rsp), %xmm11
	movaps	208(%rsp), %xmm12
	movaps	224(%rsp), %xmm13
	movaps	240(%rsp), %xmm14
	movaps	256(%rsp), %xmm15
	addq	$280, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L371:
	movabsq	$68719476735, %rdx
	cmpq	%rdx, %rdi
	seta	%bpl
	jmp	.L373
	.p2align 4,,10
.L372:
	cmpq	%rdx, %rdi
	seta	%bpl
	jbe	.L374
	cmpq	%rcx, %rdi
	jae	.L374
.L375:
	call	fast_bzero
	.p2align 4,,5
	jmp	.L367
	.p2align 4,,10
.L374:
	movabsq	$-68719476736, %rdx
	movabsq	$68719476736, %rcx
	movabsq	$68719476736, %rsi
	addq	%rbx, %rdx
	subq	%rdi, %rsi
	call	os_invalidate
	movq	core_mmap_end(%rip), %rcx
	testq	%rcx, %rcx
	setne	%al
	jmp	.L373
	.p2align 4,,10
.L380:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	104(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1836, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC42(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	je	.L367
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L367
	.p2align 4,,10
.L379:
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	call	os_invalidate
	movq	core_mmap_end(%rip), %rcx
	movq	%rcx, %rax
	subq	%rbx, %rax
	addq	%rax, %rsi
	leaq	(%rdi,%rsi), %rbx
	jmp	.L370
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC43:
	.ascii "VirtualFree(addr, 0, MEM_RELEASE)\0"
	.text
	.p2align 4,,15
	.globl	os_invalidate_free
	.def	os_invalidate_free;	.scl	2;	.type	32;	.endef
	.seh_proc	os_invalidate_free
os_invalidate_free:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	xorl	%edx, %edx
	movl	$32768, %r8d
	call	*__imp_VirtualFree(%rip)
	movl	dyndebug_skip_averlax(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L381
	testl	%eax, %eax
	je	.L383
.L381:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L383:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	104(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1843, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC43(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	je	.L381
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L381
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC44:
	.ascii "VirtualQuery(addr, &minfo, sizeof minfo)\0"
.LC45:
	.ascii "minfo.AllocationBase\0"
	.align 8
.LC46:
	.ascii "VirtualFree(minfo.AllocationBase, 0, MEM_RELEASE)\0"
	.text
	.p2align 4,,15
	.globl	os_invalidate_free_by_any_address
	.def	os_invalidate_free_by_any_address;	.scl	2;	.type	32;	.endef
	.seh_proc	os_invalidate_free_by_any_address
os_invalidate_free_by_any_address:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	.seh_endprologue
	movl	$48, %r8d
	leaq	96(%rsp), %rdx
	call	*__imp_VirtualQuery(%rip)
	movl	dyndebug_skip_averlax(%rip), %r10d
	movq	104(%rsp), %rcx
	testl	%r10d, %r10d
	jne	.L386
	testq	%rax, %rax
	je	.L393
	testq	%rcx, %rcx
	je	.L394
.L386:
	movl	$32768, %r8d
	xorl	%edx, %edx
	call	*__imp_VirtualFree(%rip)
	movl	dyndebug_skip_averlax(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L384
	testl	%eax, %eax
	je	.L395
.L384:
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L395:
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1852, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC46(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	je	.L384
	movq	152(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L384
	.p2align 4,,10
.L394:
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1851, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC45(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	jne	.L389
.L392:
	movq	104(%rsp), %rcx
	jmp	.L386
	.p2align 4,,10
.L393:
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1850, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC44(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	jne	.L396
.L388:
	movl	dyndebug_skip_averlax(%rip), %r9d
	movq	104(%rsp), %rcx
	testl	%r9d, %r9d
	jne	.L386
	testq	%rcx, %rcx
	jne	.L386
	jmp	.L394
	.p2align 4,,10
.L396:
	movq	152(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L388
	.p2align 4,,10
.L389:
	movq	152(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L392
	.seh_endproc
	.section .rdata,"dr"
.LC47:
	.ascii "//////<SBCL executable>\0"
	.align 8
.LC48:
	.ascii "gmfnResult>0 && gmfnResult<(MAX_PATH+1)\0"
	.align 8
.LC49:
	.ascii "handle && (handle!=INVALID_HANDLE_VALUE)\0"
	.text
	.p2align 4,,15
	.globl	win32_open_for_mmap
	.def	win32_open_for_mmap;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_open_for_mmap
win32_open_for_mmap:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$664, %rsp
	.seh_stackalloc	664
	.seh_endprologue
	leaq	.LC47(%rip), %rbp
	movq	%rcx, %rbx
	movl	$24, %ecx
	movq	%rbx, %rsi
	movq	%rbp, %rdi
	repe cmpsb
	jne	.L398
	leaq	112(%rsp), %rsi
	xorl	%ecx, %ecx
	movl	$261, %r8d
	movq	%rsi, %rdx
	call	*__imp_GetModuleFileNameW(%rip)
	movl	dyndebug_skip_averlax(%rip), %edx
	testl	%edx, %edx
	jne	.L399
	subl	$1, %eax
	cmpl	$259, %eax
	ja	.L412
.L399:
	movq	%rsi, %rcx
	movq	$0, 48(%rsp)
	movl	$0, 40(%rsp)
	movl	$3, 32(%rsp)
	xorl	%r9d, %r9d
	movl	$1, %r8d
	movl	$1179817, %edx
	call	*__imp_CreateFileW(%rip)
	movq	%rax, %rsi
.L401:
	cmpq	$-1, %rsi
	jne	.L402
	movl	$24, %ecx
	movq	%rbx, %rsi
	movq	%rbp, %rdi
	repe cmpsb
	je	.L403
	movq	$0, 48(%rsp)
	movl	$0, 40(%rsp)
	xorl	%r9d, %r9d
	movl	$3, 32(%rsp)
	movl	$1, %r8d
	movl	$1179817, %edx
	movq	%rbx, %rcx
	call	*__imp_CreateFileA(%rip)
	movq	%rax, %rsi
.L404:
	cmpq	$-1, %rsi
	movq	$-2, %rax
	jne	.L402
.L407:
	movl	dyndebug_skip_averlax(%rip), %ebx
	testl	%ebx, %ebx
	jne	.L408
	cmpq	$-3, %rax
	ja	.L413
.L408:
	movl	$32768, %edx
	movq	%rsi, %rcx
	call	*__imp__open_osfhandle(%rip)
	addq	$664, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L398:
	movq	$0, 48(%rsp)
	movl	$0, 40(%rsp)
	xorl	%r9d, %r9d
	movl	$3, 32(%rsp)
	movl	$1, %r8d
	movl	$1179817, %edx
	movq	%rbx, %rcx
	call	*__imp_CreateFileA(%rip)
	movq	%rax, %rsi
	jmp	.L401
	.p2align 4,,10
.L403:
	leaq	112(%rsp), %rsi
	xorl	%ecx, %ecx
	movl	$261, %r8d
	movq	%rsi, %rdx
	call	*__imp_GetModuleFileNameW(%rip)
	movl	dyndebug_skip_averlax(%rip), %r12d
	testl	%r12d, %r12d
	jne	.L405
	subl	$1, %eax
	cmpl	$259, %eax
	ja	.L414
.L405:
	movq	%rsi, %rcx
	movq	$0, 48(%rsp)
	movl	$0, 40(%rsp)
	movl	$3, 32(%rsp)
	xorl	%r9d, %r9d
	movl	$1, %r8d
	movl	$1179817, %edx
	call	*__imp_CreateFileW(%rip)
	movq	%rax, %rsi
	jmp	.L404
	.p2align 4,,10
.L413:
	leaq	.LC2(%rip), %rax
	movq	%rax, 648(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbp
	movl	%eax, %ebx
	call	*%rbp
	movl	(%rax), %edi
	call	*%rbp
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	648(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r11d
	movl	%eax, %r12d
	testl	%r11d, %r11d
	je	.L409
	mov	specials(%rip), %r13d
	movq	648(%rsp), %r14
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1881, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC49(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r12d, %r12d
	je	.L408
	movq	648(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L408
	.p2align 4,,10
.L412:
	leaq	.LC2(%rip), %rax
	movq	%rax, 648(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %r12
	movl	%eax, %edi
	call	*%r12
	movl	(%rax), %r13d
	call	*%r12
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	648(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%edi, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r15d
	movl	%eax, %r14d
	testl	%r15d, %r15d
	je	.L400
	movq	648(%rsp), %rdx
	mov	specials(%rip), %r15d
	movq	%rdx, 104(%rsp)
	call	pthread_self
	movq	104(%rsp), %rdx
	movq	%r12, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r13d, 72(%rsp)
	movl	%edi, 56(%rsp)
	movl	$1874, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	1432(%rax,%r15,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC48(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r14d, %r14d
	je	.L399
	movq	648(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L399
	.p2align 4,,10
.L414:
	leaq	.LC2(%rip), %rax
	movq	%rax, 648(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbx
	movl	%eax, %edi
	call	*%rbx
	movl	(%rax), %r13d
	call	*%rbx
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	648(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%edi, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %ebp
	movl	%eax, %ebx
	testl	%ebp, %ebp
	je	.L400
	mov	specials(%rip), %ebp
	movq	648(%rsp), %r14
	call	pthread_self
	movq	%r12, 80(%rsp)
	movl	%r13d, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%edi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1874, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC48(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebx, %ebx
	je	.L405
	movq	648(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L405
	.p2align 4,,10
.L402:
	leaq	-1(%rsi), %rax
	jmp	.L407
.L400:
	mov	specials(%rip), %ebx
	movq	648(%rsp), %rsi
	call	pthread_self
	movq	%r12, 72(%rsp)
	movl	%r13d, 64(%rsp)
	leaq	.LC48(%rip), %r9
	movq	%rsi, 56(%rsp)
	movl	%edi, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1874, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L409:
	mov	specials(%rip), %esi
	movq	648(%rsp), %r12
	call	pthread_self
	movq	%rbp, 72(%rsp)
	movl	%edi, 64(%rsp)
	leaq	.LC49(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rsi,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1881, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
.LC50:
	.ascii "fd\0"
.LC51:
	.ascii "rb\0"
	.text
	.p2align 4,,15
	.globl	win32_fopen_runtime
	.def	win32_fopen_runtime;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_fopen_runtime
win32_fopen_runtime:
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$112, %rsp
	.seh_stackalloc	112
	.seh_endprologue
	leaq	.LC47(%rip), %rcx
	call	win32_open_for_mmap
	movl	dyndebug_skip_averlax(%rip), %r8d
	movl	%eax, %ebx
	testl	%r8d, %r8d
	jne	.L416
	testl	%eax, %eax
	js	.L419
.L416:
	leaq	.LC51(%rip), %rdx
	movl	%ebx, %ecx
	call	*__imp__fdopen(%rip)
	addq	$112, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	ret
	.p2align 4,,10
.L419:
	leaq	.LC2(%rip), %rax
	movq	%rax, 104(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbp
	movl	%eax, %esi
	call	*%rbp
	movl	(%rax), %edi
	call	*%rbp
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	104(%rsp), %rax
	xorl	%edx, %edx
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	$1033, %r9d
	movl	%esi, %r8d
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %ecx
	movl	%eax, %r12d
	testl	%ecx, %ecx
	je	.L417
	mov	specials(%rip), %r13d
	movq	104(%rsp), %r14
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%esi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1889, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC50(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r12d, %r12d
	je	.L416
	movq	104(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L416
.L417:
	mov	specials(%rip), %ebx
	movq	104(%rsp), %r12
	call	pthread_self
	movq	%rbp, 72(%rsp)
	movl	%edi, 64(%rsp)
	leaq	.LC50(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%esi, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1889, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC52:
	.ascii "MapViewOfFileEx(mapping, FILE_MAP_COPY |(os_supports_executable_mapping? FILE_MAP_EXECUTE: 0), 0, offset, len, addr)\0"
	.align 8
.LC53:
	.ascii "VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE)|| VirtualAlloc(addr, len, MEM_RESERVE|MEM_COMMIT|mwwFlag, PAGE_EXECUTE_READWRITE)\0"
.LC54:
	.ascii "lseek(fd, offset, SEEK_SET)\0"
.LC55:
	.ascii "count == len\0"
	.text
	.p2align 4,,15
	.globl	os_map
	.def	os_map;	.scl	2;	.type	32;	.endef
	.seh_proc	os_map
os_map:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$136, %rsp
	.seh_stackalloc	136
	.seh_endprologue
	movabsq	$68719476736, %rax
	cmpq	%rax, %r8
	movl	%ecx, %edi
	movl	%edx, %ebp
	movq	%r8, %rbx
	movq	%r9, %rsi
	je	.L446
.L421:
	movq	%rsi, %rdx
	movl	$64, %r9d
	movl	$4096, %r8d
	movq	%rbx, %rcx
	movq	__imp_VirtualAlloc(%rip), %r12
	call	*%r12
	testq	%rax, %rax
	movl	$1, %edx
	je	.L447
.L431:
	movl	dyndebug_skip_averlax(%rip), %eax
	testl	%eax, %eax
	jne	.L432
	testl	%edx, %edx
	je	.L448
.L432:
	xorl	%r8d, %r8d
	movl	%ebp, %edx
	movl	%edi, %ecx
	call	lseek
	movl	dyndebug_skip_averlax(%rip), %ebp
	testl	%ebp, %ebp
	jne	.L435
	testl	%eax, %eax
	js	.L449
.L435:
	movl	%esi, %r8d
	movq	%rbx, %rdx
	movl	%edi, %ecx
	call	read
	cltq
	movl	dyndebug_skip_averlax(%rip), %r10d
	cmpq	%rsi, %rax
	sete	%al
	movzbl	%al, %eax
	subl	$1, %eax
	testl	%r10d, %r10d
	jne	.L430
	cmpl	$-1, %eax
	je	.L450
.L430:
	movq	%rbx, %rax
	addq	$136, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L448:
	leaq	.LC2(%rip), %rax
	movq	%rax, 120(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %r13
	movl	%eax, %r12d
	call	*%r13
	movl	(%rax), %r14d
	call	*%r13
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r13
	leaq	120(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%r12d, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %r15d
	movl	dyndebug_survive_aver(%rip), %eax
	testl	%eax, %eax
	je	.L433
	mov	specials(%rip), %eax
	movq	120(%rsp), %rdx
	movq	%rdx, 96(%rsp)
	movq	%rax, 104(%rsp)
	call	pthread_self
	movq	96(%rsp), %rdx
	movq	%r13, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r14d, 72(%rsp)
	movl	%r12d, 56(%rsp)
	movl	$1960, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	104(%rsp), %rdx
	movq	1432(%rax,%rdx,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC53(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r15d, %r15d
	je	.L432
	movq	120(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L432
	.p2align 4,,10
.L450:
	leaq	.LC2(%rip), %rax
	movq	%rax, 120(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rbp
	movl	%eax, %esi
	call	*%rbp
	movl	(%rax), %edi
	call	*%rbp
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	120(%rsp), %rax
	movl	$1033, %r9d
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	%esi, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r9d
	movl	%eax, %r12d
	testl	%r9d, %r9d
	je	.L439
	mov	specials(%rip), %r13d
	movq	120(%rsp), %r14
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r14, 64(%rsp)
	movl	%esi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r13,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1965, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC55(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r12d, %r12d
	je	.L430
	movq	120(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L430
	.p2align 4,,10
.L449:
	leaq	.LC2(%rip), %rax
	movq	%rax, 120(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %r12
	movl	%eax, %ebp
	call	*%r12
	movl	(%rax), %r13d
	call	*%r12
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	120(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebp, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r11d
	movl	%eax, %r14d
	testl	%r11d, %r11d
	je	.L436
	movq	120(%rsp), %rdx
	mov	specials(%rip), %r15d
	movq	%rdx, 96(%rsp)
	call	pthread_self
	movq	96(%rsp), %rdx
	movq	%r12, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r13d, 72(%rsp)
	movl	%ebp, 56(%rsp)
	movl	$1962, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	1432(%rax,%r15,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC54(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r14d, %r14d
	je	.L435
	movq	120(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L435
	.p2align 4,,10
.L446:
	testw	%dx, %dx
	jne	.L421
	addq	$65535, %rsi
	movq	__imp__get_osfhandle(%rip), %r14
	xorw	%si, %si
	leal	0(%rbp,%rsi), %r13d
	call	*%r14
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	movq	$0, 40(%rsp)
	movl	%r13d, 32(%rsp)
	movl	$128, %r8d
	movq	%rax, %rcx
	movq	__imp_CreateFileMappingA(%rip), %r15
	call	*%r15
	testq	%rax, %rax
	movq	%rax, %r12
	je	.L451
	movl	$1, os_supports_executable_mapping(%rip)
.L423:
	xorl	%edx, %edx
	movl	$32768, %r8d
	movabsq	$68719476736, %rcx
	call	*__imp_VirtualFree(%rip)
	movl	dyndebug_skip_averlax(%rip), %r10d
	testl	%r10d, %r10d
	jne	.L424
	testl	%eax, %eax
	jne	.L424
	leaq	.LC2(%rip), %rax
	movq	%rax, 120(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %r13
	movl	%eax, %edi
	call	*%r13
	movl	(%rax), %r14d
	call	*%r13
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r13
	leaq	120(%rsp), %rax
	movl	$1033, %r9d
	movl	%edi, %r8d
	xorl	%edx, %edx
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %r15d
	mov	specials(%rip), %eax
	movq	120(%rsp), %rdx
	movq	%rdx, 96(%rsp)
	movq	%rax, 104(%rsp)
	call	pthread_self
	movq	96(%rsp), %rdx
	movq	%r13, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r14d, 72(%rsp)
	movl	%edi, 56(%rsp)
	movl	$1929, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	104(%rsp), %rdx
	movq	1432(%rax,%rdx,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC43(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r15d, %r15d
	je	.L424
	movq	120(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	.p2align 4,,10
.L424:
	cmpl	$1, os_supports_executable_mapping(%rip)
	movabsq	$68719476736, %r8
	movl	%ebp, %r9d
	movq	%r8, 40(%rsp)
	movq	%rsi, 32(%rsp)
	movq	%r12, %rcx
	sbbl	%edx, %edx
	xorl	%r8d, %r8d
	andl	$-32, %edx
	addl	$33, %edx
	call	*__imp_MapViewOfFileEx(%rip)
	movl	dyndebug_skip_averlax(%rip), %r9d
	testl	%r9d, %r9d
	jne	.L426
	testq	%rax, %rax
	jne	.L426
	leaq	.LC2(%rip), %rax
	movq	%rax, 120(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %r12
	movl	%eax, %edi
	call	*%r12
	movl	(%rax), %ebp
	call	*%r12
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r12
	leaq	120(%rsp), %rax
	xorl	%edx, %edx
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	$1033, %r9d
	movl	%edi, %r8d
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %ecx
	movl	%eax, %r13d
	testl	%ecx, %ecx
	je	.L427
	mov	specials(%rip), %r14d
	movq	120(%rsp), %r15
	call	pthread_self
	movq	%r12, 80(%rsp)
	movl	%ebp, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r15, 64(%rsp)
	movl	%edi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r14,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$1947, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC52(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r13d, %r13d
	je	.L426
	movq	120(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	.p2align 4,,10
.L426:
	movq	dynamic_space_size(%rip), %rdx
	movabsq	$68719476736, %rdi
	addq	%rsi, %rdi
	movq	%rdi, %rcx
	subq	%rsi, %rdx
	call	os_validate
	movl	os_supports_executable_mapping(%rip), %edx
	leaq	os_mmap_exec_modes(%rip), %rax
	movq	%rdi, core_mmap_end(%rip)
	testl	%edx, %edx
	leaq	os_mmap_noexec_modes(%rip), %rdx
	cmove	%rdx, %rax
	movq	%rax, os_mmap_protect_modes(%rip)
	jmp	.L430
	.p2align 4,,10
.L447:
	movl	mwwFlag(%rip), %r8d
	movq	%rsi, %rdx
	movl	$64, %r9d
	movq	%rbx, %rcx
	orl	$12288, %r8d
	call	*%r12
	xorl	%edx, %edx
	testq	%rax, %rax
	setne	%dl
	jmp	.L431
.L451:
	movl	%edi, %ecx
	call	*%r14
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	movq	$0, 40(%rsp)
	movl	%r13d, 32(%rsp)
	movl	$2, %r8d
	movq	%rax, %rcx
	call	*%r15
	testq	%rax, %rax
	movq	%rax, %r12
	je	.L421
	movl	$0, os_supports_executable_mapping(%rip)
	jmp	.L423
.L436:
	mov	specials(%rip), %ebx
	movq	120(%rsp), %rsi
	call	pthread_self
	movq	%r12, 72(%rsp)
	movl	%r13d, 64(%rsp)
	leaq	.LC54(%rip), %r9
	movq	%rsi, 56(%rsp)
	movl	%ebp, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1962, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L439:
	mov	specials(%rip), %ebx
	movq	120(%rsp), %r12
	call	pthread_self
	movq	%rbp, 72(%rsp)
	movl	%edi, 64(%rsp)
	leaq	.LC55(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%esi, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1965, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L433:
	mov	specials(%rip), %ebx
	movq	120(%rsp), %rsi
	call	pthread_self
	movq	%r13, 72(%rsp)
	movl	%r14d, 64(%rsp)
	leaq	.LC53(%rip), %r9
	movq	%rsi, 56(%rsp)
	movl	%r12d, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1960, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L427:
	mov	specials(%rip), %ebx
	movq	120(%rsp), %rsi
	call	pthread_self
	movq	%r12, 72(%rsp)
	movl	%ebp, 64(%rsp)
	leaq	.LC52(%rip), %r9
	movq	%rsi, 56(%rsp)
	movl	%edi, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$1947, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC56:
	.ascii "VirtualProtect(address, length, os_mmap_protect_modes[prot], &old_prot)\0"
	.align 8
.LC57:
	.ascii "VirtualProtect(address, length, new_prot, &old_prot)|| (VirtualAlloc(address, length, MEM_COMMIT, new_prot) && VirtualProtect(address, length, new_prot, &old_prot))\0"
	.align 8
.LC58:
	.ascii "Protecting %p + %p vmaccess %d newprot %08x oldprot %08x\0"
	.text
	.p2align 4,,15
	.globl	os_protect
	.def	os_protect;	.scl	2;	.type	32;	.endef
	.seh_proc	os_protect
os_protect:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$136, %rsp
	.seh_stackalloc	136
	.seh_endprologue
	movq	%rcx, %rbx
	movq	core_mmap_end(%rip), %rcx
	movq	%rdx, %rsi
	movl	%r8d, %edi
	testq	%rcx, %rcx
	je	.L453
	leaq	(%rbx,%rdx), %rbp
	cmpq	%rbp, %rcx
	jae	.L455
	cmpq	%rbx, %rcx
	ja	.L473
.L455:
	testq	%rcx, %rcx
	movabsq	$68719476735, %rdx
	setne	%al
	cmpq	%rdx, %rbp
	jbe	.L456
	testb	%al, %al
	je	.L456
	cmpq	%rbp, %rcx
	ja	.L457
	cmpq	%rdx, %rbx
	movl	$1, %eax
	seta	%r12b
.L458:
	testb	%r12b, %r12b
	je	.L453
	testb	%al, %al
	je	.L453
	cmpq	%rbx, %rcx
	ja	.L460
.L453:
	movl	mwwFlag(%rip), %r11d
	testl	%r11d, %r11d
	je	.L464
	movabsq	$68719476736, %rax
	addq	dynamic_space_size(%rip), %rax
	xorl	%edx, %edx
	cmpq	%rax, %rbx
	movabsq	$68719476735, %rax
	setb	%dl
	cmpq	%rax, %rbx
	seta	%al
	movzbl	%al, %eax
	testl	%eax, %edx
	jne	.L474
.L464:
	leaq	os_protect_modes(%rip), %rax
	movslq	%edi, %rdx
	leaq	124(%rsp), %r9
	movq	%rbx, %rcx
	movq	__imp_VirtualProtect(%rip), %r12
	movl	(%rax,%rdx,4), %ebp
	movq	%rsi, %rdx
	movl	%ebp, %r8d
	call	*%r12
	testl	%eax, %eax
	movl	$1, %edx
	je	.L475
.L465:
	movl	dyndebug_skip_averlax(%rip), %eax
	testl	%eax, %eax
	jne	.L466
.L478:
	testl	%edx, %edx
	je	.L476
.L466:
	movl	dyndebug_misc(%rip), %r12d
	testl	%r12d, %r12d
	jne	.L477
.L452:
	addq	$136, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L456:
	movabsq	$68719476735, %rdx
	cmpq	%rdx, %rbx
	seta	%r12b
	jmp	.L458
	.p2align 4,,10
.L477:
	movl	124(%rsp), %eax
	leaq	.LC58(%rip), %rcx
	movl	%edi, %r9d
	movq	%rsi, %r8
	movq	%rbx, %rdx
	movl	%ebp, 32(%rsp)
	movl	%eax, 40(%rsp)
	call	odprintf_
	jmp	.L452
	.p2align 4,,10
.L457:
	cmpq	%rdx, %rbx
	seta	%r12b
	jbe	.L459
	cmpq	%rbx, %rcx
	jbe	.L459
.L460:
	movq	os_mmap_protect_modes(%rip), %rax
	movslq	%edi, %rdi
	movq	%rsi, %rdx
	leaq	124(%rsp), %r9
	movq	%rbx, %rcx
	movl	(%rax,%rdi,4), %r8d
	call	*__imp_VirtualProtect(%rip)
	movl	dyndebug_skip_averlax(%rip), %edx
	testl	%edx, %edx
	jne	.L452
	testl	%eax, %eax
	jne	.L452
	leaq	.LC2(%rip), %rax
	movq	%rax, 112(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rdi
	movl	%eax, %ebx
	call	*%rdi
	movl	(%rax), %esi
	call	*%rdi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rdi
	leaq	112(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %ebp
	movl	dyndebug_survive_aver(%rip), %eax
	testl	%eax, %eax
	je	.L462
	mov	specials(%rip), %r12d
	movq	112(%rsp), %r13
	call	pthread_self
	movq	%rdi, 80(%rsp)
	movl	%esi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$2003, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC56(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%ebp, %ebp
	je	.L452
	movq	112(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L452
	.p2align 4,,10
.L459:
	movabsq	$-68719476736, %rdx
	movabsq	$68719476736, %rcx
	movl	%edi, %r8d
	addq	%rbp, %rdx
	movabsq	$68719476736, %rsi
	call	os_protect
	movq	core_mmap_end(%rip), %rcx
	subq	%rbx, %rsi
	testq	%rcx, %rcx
	setne	%al
	jmp	.L458
	.p2align 4,,10
.L475:
	movq	%rsi, %rdx
	movl	%ebp, %r9d
	movl	$4096, %r8d
	movq	%rbx, %rcx
	call	*__imp_VirtualAlloc(%rip)
	xorl	%edx, %edx
	testq	%rax, %rax
	je	.L465
	movq	%rsi, %rdx
	leaq	124(%rsp), %r9
	movl	%ebp, %r8d
	movq	%rbx, %rcx
	call	*%r12
	xorl	%edx, %edx
	testl	%eax, %eax
	movl	dyndebug_skip_averlax(%rip), %eax
	setne	%dl
	testl	%eax, %eax
	je	.L478
	jmp	.L466
	.p2align 4,,10
.L476:
	leaq	.LC2(%rip), %rax
	movq	%rax, 112(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %r13
	movl	%eax, %r12d
	call	*%r13
	movl	(%rax), %r14d
	call	*%r13
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %r13
	leaq	112(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%r12d, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	%eax, %r15d
	movl	dyndebug_survive_aver(%rip), %eax
	testl	%eax, %eax
	je	.L467
	mov	specials(%rip), %eax
	movq	112(%rsp), %rdx
	movq	%rdx, 96(%rsp)
	movq	%rax, 104(%rsp)
	call	pthread_self
	movq	96(%rsp), %rdx
	movq	%r13, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r14d, 72(%rsp)
	movl	%r12d, 56(%rsp)
	movl	$2015, %r9d
	movq	dyndebug_output(%rip), %rcx
	movq	%rdx, 64(%rsp)
	movq	104(%rsp), %rdx
	movq	1432(%rax,%rdx,8), %rax
	leaq	.LC4(%rip), %rdx
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC57(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r15d, %r15d
	je	.L466
	movq	112(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L466
	.p2align 4,,10
.L473:
	movq	%rbp, %rdx
	subq	%rcx, %rdx
	call	os_protect
	movq	core_mmap_end(%rip), %rcx
	movq	%rcx, %rax
	subq	%rbp, %rax
	addq	%rax, %rsi
	leaq	(%rbx,%rsi), %rbp
	jmp	.L455
	.p2align 4,,10
.L474:
	movq	%rsi, %rdx
	movq	%rbx, %rcx
	call	*ptr_ResetWriteWatch(%rip)
	jmp	.L452
.L467:
	mov	specials(%rip), %ebx
	movq	112(%rsp), %rsi
	call	pthread_self
	movq	%r13, 72(%rsp)
	movl	%r14d, 64(%rsp)
	leaq	.LC57(%rip), %r9
	movq	%rsi, 56(%rsp)
	movl	%r12d, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbx,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$2015, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L462:
	mov	specials(%rip), %ebp
	movq	112(%rsp), %r12
	call	pthread_self
	movq	%rdi, 72(%rsp)
	movl	%esi, 64(%rsp)
	leaq	.LC56(%rip), %r9
	movq	%r12, 56(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	movq	1432(%rax,%rbp,8), %rax
	leaq	.LC4(%rip), %rcx
	movl	$2003, %r8d
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
	nop
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC59:
	.ascii "VirtualQuery(address, &minfo, sizeof minfo)\0"
	.text
	.p2align 4,,15
	.globl	os_current_protection
	.def	os_current_protection;	.scl	2;	.type	32;	.endef
	.seh_proc	os_current_protection
os_current_protection:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$168, %rsp
	.seh_stackalloc	168
	.seh_endprologue
	movl	$48, %r8d
	leaq	96(%rsp), %rdx
	call	*__imp_VirtualQuery(%rip)
	movl	dyndebug_skip_averlax(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L480
	testq	%rax, %rax
	je	.L491
.L480:
	movl	132(%rsp), %edx
	xorl	%eax, %eax
	cmpl	os_protect_modes(%rip), %edx
	je	.L482
	cmpl	4+os_protect_modes(%rip), %edx
	movl	$1, %eax
	je	.L482
	cmpl	8+os_protect_modes(%rip), %edx
	movl	$2, %eax
	je	.L482
	cmpl	12+os_protect_modes(%rip), %edx
	movl	$3, %eax
	je	.L482
	cmpl	16+os_protect_modes(%rip), %edx
	movl	$4, %eax
	je	.L482
	cmpl	20+os_protect_modes(%rip), %edx
	movl	$5, %eax
	je	.L482
	cmpl	24+os_protect_modes(%rip), %edx
	movl	$6, %eax
	je	.L482
	xorl	%eax, %eax
	cmpl	28+os_protect_modes(%rip), %edx
	je	.L492
.L482:
	addq	$168, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L491:
	leaq	.LC2(%rip), %rax
	movq	%rax, 152(%rsp)
	call	*__imp_GetLastError(%rip)
	movq	__imp__errno(%rip), %rsi
	movl	%eax, %ebx
	call	*%rsi
	movl	(%rax), %edi
	call	*%rsi
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	152(%rsp), %rax
	xorl	%edx, %edx
	movl	$1033, %r9d
	movl	%ebx, %r8d
	movl	$4352, %ecx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	call	*__imp_FormatMessageA(%rip)
	mov	specials(%rip), %r12d
	movq	152(%rsp), %r13
	movl	%eax, %esi
	call	pthread_self
	movq	%rbp, 80(%rsp)
	movl	%edi, 72(%rsp)
	leaq	.LC3(%rip), %r8
	movq	%r13, 64(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	1432(%rax,%r12,8), %rax
	movq	dyndebug_output(%rip), %rcx
	movl	$2028, %r9d
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC59(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%esi, %esi
	je	.L480
	movq	152(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L480
	.p2align 4,,10
.L492:
	movl	$7, %eax
	jmp	.L482
	.seh_endproc
	.p2align 4,,15
	.globl	is_linkage_table_addr
	.def	is_linkage_table_addr;	.scl	2;	.type	32;	.endef
	.seh_proc	is_linkage_table_addr
is_linkage_table_addr:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	xorl	%eax, %eax
	cmpq	$1078980607, %rcx
	setbe	%al
	xorl	%edx, %edx
	cmpq	$538968063, %rcx
	seta	%dl
	andl	%edx, %eax
	addq	$8, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	is_valid_lisp_addr
	.def	is_valid_lisp_addr;	.scl	2;	.type	32;	.endef
	.seh_proc	is_valid_lisp_addr
is_valid_lisp_addr:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	xorl	%r8d, %r8d
	movl	$1, %eax
	cmpq	$537915391, %rcx
	setbe	%r8b
	xorl	%edx, %edx
	cmpq	$536870911, %rcx
	seta	%dl
	testl	%edx, %r8d
	jne	.L495
	xorl	%r8d, %r8d
	cmpq	$538963967, %rcx
	setbe	%r8b
	xorl	%edx, %edx
	cmpq	$537919487, %rcx
	seta	%dl
	testl	%edx, %r8d
	jne	.L495
	movabsq	$68719476736, %rdx
	addq	dynamic_space_size(%rip), %rdx
	xorl	%r8d, %r8d
	cmpq	%rdx, %rcx
	movabsq	$68719476735, %rdx
	setb	%r8b
	cmpq	%rdx, %rcx
	seta	%dl
	movzbl	%dl, %edx
	testl	%edx, %r8d
	jne	.L495
	movq	all_threads(%rip), %rdx
	xorb	%al, %al
	testq	%rdx, %rdx
	jne	.L504
	jmp	.L495
	.p2align 4,,10
.L507:
	movq	176(%rdx), %rdx
	testq	%rdx, %rdx
	je	.L506
.L504:
	cmpq	64(%rdx), %rcx
	jb	.L496
	cmpq	72(%rdx), %rcx
	jb	.L503
.L496:
	movq	48(%rdx), %rax
	leaq	1048576(%rax), %r8
	cmpq	%r8, %rcx
	setb	%r8b
	cmpq	%rax, %rcx
	setae	%al
	movzbl	%r8b, %r8d
	movzbl	%al, %eax
	testl	%eax, %r8d
	je	.L507
.L503:
	movl	$1, %eax
	.p2align 4,,10
.L495:
	addq	$8, %rsp
	ret
	.p2align 4,,10
.L506:
	xorl	%eax, %eax
	jmp	.L495
	.seh_endproc
	.p2align 4,,15
	.globl	fpu_world_lispy_p
	.def	fpu_world_lispy_p;	.scl	2;	.type	32;	.endef
	.seh_proc	fpu_world_lispy_p
fpu_world_lispy_p:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	xorl	%eax, %eax
	addq	$8, %rsp
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	establish_c_fpu_world
	.def	establish_c_fpu_world;	.scl	2;	.type	32;	.endef
	.seh_proc	establish_c_fpu_world
establish_c_fpu_world:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	addq	$8, %rsp
	ret
	.seh_endproc
	.section .rdata,"dr"
.LC60:
	.ascii "Backtrace: %s (thread %p)\12\0"
	.align 8
.LC61:
	.ascii "[#%02d]: ebp = 0x%p, ret = 0x%p\12\0"
	.text
	.p2align 4,,15
	.globl	c_level_backtrace
	.def	c_level_backtrace;	.scl	2;	.type	32;	.endef
	.seh_proc	c_level_backtrace
c_level_backtrace:
	pushq	%rbp
	.seh_pushreg	%rbp
	movq	%rsp, %rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$56, %rsp
	.seh_stackalloc	56
	.seh_setframe	%rbp, 80
	.seh_endprologue
	movq	%rcx, %rbx
	movl	%edx, %edi
/APP
 # 405 "win32-os.c" 1
	mov %gs:0,%rax
 # 0 "" 2
/NO_APP
	leaq	-1(%rax), %rdx
	cmpq	$-3, %rdx
	ja	.L511
	.p2align 4,,10
.L516:
	movq	(%rax), %rax
	leaq	-1(%rax), %rdx
	cmpq	$-3, %rdx
	jbe	.L516
.L511:
	mov	specials(%rip), %esi
	call	pthread_self
	movq	dyndebug_output(%rip), %rcx
	leaq	.LC60(%rip), %rdx
	movq	%rbx, %r8
	movq	%rbp, %rbx
	movq	1432(%rax,%rsi,8), %r9
	movl	$1, %esi
	call	fprintf
	testl	%edi, %edi
	jns	.L514
	jmp	.L510
	.p2align 4,,10
.L515:
	addl	$1, %esi
	leal	-1(%rsi), %eax
	cmpl	%edi, %eax
	jg	.L510
.L514:
	movq	8(%rbx), %rax
	movq	dyndebug_output(%rip), %rcx
	leaq	.LC61(%rip), %rdx
	movq	%rbx, %r9
	movl	%esi, %r8d
	movq	%rax, 32(%rsp)
	call	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.L515
.L510:
	addq	$56, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	ret
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC62:
	.ascii "SEH: rec %p, frame %p, ctxptr %p, disp %p, rip %p, fault %p\12... thread %p, code %p, rcx %p, fp-tags %p\12\12\0"
	.align 8
.LC63:
	.ascii "SEGV. ThSap %p, Eip %p, Esp %p, Esi %p, Edi %p, Addr %p Access %d\12\0"
	.align 8
.LC64:
	.ascii "Low page access (?) thread %p\12(addr 0x%p, EIP 0x%p ESP 0x%p EBP 0x%p)\12\0"
	.align 8
.LC65:
	.ascii "VirtualAlloc(PTR_ALIGN_DOWN(fault_address,os_vm_page_size), os_vm_page_size, MEM_COMMIT, PAGE_EXECUTE_READWRITE)\0"
	.align 8
.LC66:
	.ascii "Unable to recommit addr %p eip 0x%p\12\0"
.LC67:
	.ascii "BT\0"
.LC68:
	.ascii "Lispy backtrace\0"
	.align 8
.LC69:
	.ascii "VirtualAlloc(PTR_ALIGN_DOWN(fault_address,os_vm_page_size), os_vm_page_size, MEM_COMMIT, PAGE_EXECUTE_READWRITE) ||(fprintf(stderr,\"Unable to recommit addr %p eip 0x%p\\n\", fault_address, (void*)context->Rip) && (c_level_backtrace(\"BT\",5), fake_foreign_function_call(&ctx), lose(\"Lispy backtrace\"), 0))\0"
.LC70:
	.ascii "Exception Code: 0x%p.\12\0"
.LC71:
	.ascii "Faulting IP: 0x%p.\12\0"
.LC72:
	.ascii "page status: 0x%lx.\12\0"
	.align 8
.LC73:
	.ascii "Was writing: %p, where: 0x%p.\12\0"
	.align 8
.LC74:
	.ascii "Exception too early in cold init, cannot continue.\0"
	.text
	.p2align 4,,15
	.globl	veh
	.def	veh;	.scl	2;	.type	32;	.endef
	.seh_proc	veh
veh:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$200, %rsp
	.seh_stackalloc	200
	.seh_endprologue
	movq	%rcx, %rdi
/APP
 # 1594 "/home/anton/sdks/orgbin/cross-amd64-win64-20110206/bin/../lib/gcc/x86_64-w64-mingw32/4.6.0/../../../../x86_64-w64-mingw32/include/winnt.h" 1
	movq	%gs:48,%rax
 # 0 "" 2
/NO_APP
	movq	__imp__errno(%rip), %rbx
	movl	104(%rax), %ebp
	call	*%rbx
	movl	(%rax), %r12d
	call	pthread_self
	testq	%rax, %rax
	movq	%rax, %rsi
	je	.L549
	call	*%rbx
	movl	%r12d, (%rax)
/APP
 # 1594 "/home/anton/sdks/orgbin/cross-amd64-win64-20110206/bin/../lib/gcc/x86_64-w64-mingw32/4.6.0/../../../../x86_64-w64-mingw32/include/winnt.h" 1
	movq	%gs:48,%rax
 # 0 "" 2
/NO_APP
	movl	%ebp, 104(%rax)
	movq	8(%rdi), %rbp
	xorl	%eax, %eax
	movq	(%rdi), %rdi
	testq	%rbp, %rbp
	je	.L519
	testb	$6, 4(%rdi)
	je	.L557
.L519:
	addq	$200, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L557:
	movq	__imp_GetLastError(%rip), %r15
	call	*%r15
	movl	%eax, 108(%rsp)
	call	*%rbx
	mov	specials(%rip), %edx
	movl	(%rax), %r14d
	movq	40(%rdi), %r13
	mov	(%rdi), %eax
	movq	1432(%rsi,%rdx,8), %r12
	movl	dyndebug_seh(%rip), %edx
	testl	%edx, %edx
	jne	.L558
.L520:
	xorl	%edx, %edx
	testq	%r12, %r12
	movq	%rbp, 160(%rsp)
	je	.L521
	movq	8(%r12), %rdx
	movl	40(%rdx), %edx
.L521:
	cmpl	$-2147483645, %eax
	movl	%edx, 168(%rsp)
	je	.L559
	cmpl	$-2147483647, %eax
	je	.L560
	cmpl	$-1073741819, %eax
	je	.L561
.L529:
	movl	internal_errors_enabled(%rip), %r9d
	testl	%r9d, %r9d
	je	.L543
/APP
 # 2763 "win32-os.c" 1
	fnclex
 # 0 "" 2
/NO_APP
	leaq	160(%rsp), %rbp
	xorl	%ecx, %ecx
	leaq	8(%rbp), %rdx
	call	block_blockable_signals
	movq	%rbp, %rcx
	call	fake_foreign_function_call
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	(%rdx), %r12
	testq	%r12, %r12
	jne	.L562
	movq	%rbp, %rcx
	call	alloc_sap
	movq	%rdi, %rcx
	movq	%rax, %r12
	call	alloc_sap
	movq	537922568, %rdx
	movq	%rax, %r8
	movl	168(%rsp), %eax
	andq	$-16, %rdx
	movl	%eax, 40(%rsi)
	movq	16(%rdx), %rcx
	movq	%r12, %rdx
	call	funcall2
.L545:
	movq	%rbp, %rcx
	call	undo_fake_foreign_function_call
	movl	168(%rsp), %eax
	movl	%eax, 40(%rsi)
.L525:
	call	*%rbx
	movl	108(%rsp), %ecx
	movl	%r14d, (%rax)
	call	*__imp_SetLastError(%rip)
	movl	$-1, %eax
	jmp	.L519
	.p2align 4,,10
.L549:
	xorl	%eax, %eax
	jmp	.L519
	.p2align 4,,10
.L558:
	movzbl	260(%rbp), %edx
	leaq	.LC62(%rip), %rcx
	movq	%rbp, %r9
	xorl	%r8d, %r8d
	movl	%edx, 80(%rsp)
	movq	128(%rbp), %rdx
	movq	%rax, 64(%rsp)
	movq	%r12, 56(%rsp)
	movq	%r13, 48(%rsp)
	movq	%rdx, 72(%rsp)
	movq	248(%rbp), %rax
	movq	%rdi, %rdx
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	odprintf_
	movl	(%rdi), %eax
	jmp	.L520
	.p2align 4,,10
.L561:
	movl	dyndebug_pagefaults(%rip), %eax
	testl	%eax, %eax
	jne	.L563
.L530:
	testq	%r12, %r12
	je	.L531
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	testq	%rax, %rax
	je	.L532
	cmpq	%r13, 16(%rax)
	ja	.L533
	cmpq	%r13, %rax
	jbe	.L533
.L534:
	leaq	160(%rsp), %rcx
	movq	%r13, %rdx
	call	handle_guard_page_triggered
	testl	%eax, %eax
	jne	.L564
	.p2align 4,,10
.L532:
	cmpq	$65534, %r13
	ja	.L531
	movq	160(%rbp), %rdi
	movq	152(%rbp), %rsi
	movq	248(%rbp), %rbx
	call	*__imp___iob_func(%rip)
	leaq	.LC64(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%r13, %r9
	movq	%r12, %r8
	movq	%rdi, 48(%rsp)
	movq	%rsi, 40(%rsp)
	movq	%rbx, 32(%rsp)
	call	fprintf
	orl	$-1, %ecx
	call	*__imp_Sleep(%rip)
	xorl	%ecx, %ecx
	call	*__imp_ExitProcess(%rip)
	.p2align 4,,10
.L531:
	cmpq	$553648128, %r13
	je	.L565
	testq	%r12, %r12
	je	.L536
	cmpq	%r13, 240(%r12)
	je	.L566
.L536:
	movq	%r13, %rcx
	call	find_page_index
	cmpl	$-1, %eax
	je	.L537
	cltq
	salq	$4, %rax
	addq	page_table(%rip), %rax
	testb	$1, 10(%rax)
	jne	.L567
	movabsq	$68719476735, %rdx
	movq	core_mmap_end(%rip), %rax
	cmpq	%rdx, %r13
	jbe	.L539
	testq	%rax, %rax
	je	.L539
	cmpq	%r13, %rax
	ja	.L525
.L539:
	movq	os_vm_page_size(%rip), %rdx
	movl	$4096, %r8d
	movl	$64, %r9d
	movq	%rdx, %rcx
	negq	%rcx
	andq	%r13, %rcx
	call	*__imp_VirtualAlloc(%rip)
	movl	dyndebug_skip_averlax(%rip), %r8d
	testl	%r8d, %r8d
	jne	.L525
	testq	%rax, %rax
	jne	.L525
	leaq	.LC65(%rip), %rdx
	xorl	%r9d, %r9d
	movl	$2717, %r8d
	xorl	%ecx, %ecx
	call	win_aver.part.3.constprop.8
	jmp	.L525
	.p2align 4,,10
.L562:
	movq	$0, (%rdx)
	movq	248(%rax), %r13
	movq	$0, 248(%rax)
	movq	%rbp, %rcx
	call	alloc_sap
	movq	%rdi, %rcx
	movq	%rax, %r15
	call	alloc_sap
	movq	537922568, %rdx
	movq	%rax, %r8
	movl	168(%rsp), %eax
	andq	$-16, %rdx
	movl	%eax, 40(%rsi)
	movq	16(%rdx), %rcx
	movq	%r15, %rdx
	call	funcall2
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	%r12, (%rdx)
	movq	%r13, 248(%rax)
	jmp	.L545
	.p2align 4,,10
.L563:
	movq	32(%rdi), %rax
	movq	152(%rbp), %r9
	leaq	.LC63(%rip), %rcx
	movq	248(%rbp), %r8
	movq	%r13, 48(%rsp)
	movq	%r12, %rdx
	movq	%rax, 56(%rsp)
	movq	176(%rbp), %rax
	movq	%rax, 40(%rsp)
	movq	168(%rbp), %rax
	movq	%rax, 32(%rsp)
	call	odprintf_
	jmp	.L530
.L565:
	leaq	160(%rsp), %rcx
	call	thread_in_lisp_raised
	jmp	.L525
	.p2align 4,,10
.L559:
	leaq	160(%rsp), %rbp
	movq	%rbp, %rcx
	call	os_context_pc_addr
	movq	(%rax), %rax
	cmpb	$-52, (%rax)
	je	.L568
.L523:
	movq	%rbp, %rcx
	call	os_context_pc_addr
	movq	(%rax), %rax
	movq	%rbp, %rcx
	movzbl	(%rax), %edi
	cmpl	$9, %edi
	je	.L569
	call	os_context_sp_addr
	movq	(%rax), %rax
	movq	%rax, 232(%r12)
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	(%rdx), %r12
	testq	%r12, %r12
	jne	.L570
	leaq	8(%rbp), %rdx
	xorl	%ecx, %ecx
	call	block_blockable_signals
	movl	%edi, %edx
	movq	%rbp, %rcx
	call	handle_trap
	movl	168(%rsp), %eax
	movl	%eax, 40(%rsi)
	jmp	.L525
	.p2align 4,,10
.L560:
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	240(%rax), %rdx
	xorl	%ebp, %ebp
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	je	.L528
	movq	$0, (%rdx)
	movq	248(%rax), %rbp
	movq	$0, 248(%rax)
.L528:
	leaq	160(%rsp), %rcx
	movq	%r13, %rdx
	call	handle_guard_page_triggered
	testl	%eax, %eax
	jne	.L525
	mov	specials(%rip), %eax
	testq	%rdi, %rdi
	movq	1432(%rsi,%rax,8), %rax
	je	.L525
	movq	240(%rax), %rdx
	movq	%rdi, (%rdx)
	movq	%rbp, 248(%rax)
	jmp	.L525
	.p2align 4,,10
.L533:
	cmpq	%r13, 64(%rax)
	ja	.L532
	cmpq	%r13, 72(%rax)
	jbe	.L532
	.p2align 4,,3
	jmp	.L534
	.p2align 4,,10
.L537:
	cmpq	undefined_alien_address(%rip), %r13
	je	.L529
	movq	os_vm_page_size(%rip), %rdx
	movl	$64, %r9d
	movl	$4096, %r8d
	movq	%rdx, %rcx
	negq	%rcx
	andq	%r13, %rcx
	call	*__imp_VirtualAlloc(%rip)
	testq	%rax, %rax
	movl	$1, %edx
	je	.L571
.L540:
	movl	dyndebug_skip_averlax(%rip), %r11d
	testl	%r11d, %r11d
	jne	.L525
	testl	%edx, %edx
	jne	.L525
	leaq	.LC2(%rip), %rax
	movq	%rax, 184(%rsp)
	call	*%r15
	movl	%eax, %edi
	call	*%rbx
	movl	(%rax), %r12d
	call	*%rbx
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rbp
	leaq	184(%rsp), %rax
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movl	$1033, %r9d
	movq	%rax, 32(%rsp)
	movl	%edi, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r10d
	movl	%eax, %r13d
	testl	%r10d, %r10d
	je	.L541
	movq	184(%rsp), %rax
	movq	%rbp, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%r12d, 72(%rsp)
	movl	%edi, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	dyndebug_output(%rip), %rcx
	movl	$2744, %r9d
	movq	%rax, 64(%rsp)
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC69(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r13d, %r13d
	je	.L525
	movq	184(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L525
	.p2align 4,,10
.L570:
	movq	$0, (%rdx)
	movq	248(%rax), %r13
	movq	$0, 248(%rax)
	leaq	8(%rbp), %rdx
	xorl	%ecx, %ecx
	call	block_blockable_signals
	movl	%edi, %edx
	movq	%rbp, %rcx
	call	handle_trap
	movl	168(%rsp), %eax
	movl	%eax, 40(%rsi)
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	%r12, (%rdx)
	movq	%r13, 248(%rax)
	jmp	.L525
	.p2align 4,,10
.L569:
	call	arch_skip_instruction
	movq	%rbp, %rcx
	call	thread_interrupted
	.p2align 4,,2
	jmp	.L525
.L567:
	movq	%r13, %rcx
	call	gencgc_handle_wp_violation
	.p2align 4,,5
	jmp	.L525
.L566:
	leaq	160(%rsp), %rcx
	call	thread_in_safety_transition
	jmp	.L525
.L568:
	movq	%rbp, %rcx
	call	os_context_pc_addr
	addq	$1, (%rax)
	jmp	.L523
.L564:
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	240(%rax), %rdx
	cmpq	$0, (%rdx)
	je	.L525
	movq	$0, (%rdx)
	movq	$0, 248(%rax)
	jmp	.L525
.L571:
	movq	248(%rbp), %rdi
	call	*__imp___iob_func(%rip)
	leaq	.LC66(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%r13, %r8
	movq	%rdi, %r9
	call	fprintf
	testl	%eax, %eax
	jne	.L572
	xorl	%edx, %edx
	jmp	.L540
.L543:
	mov	(%rdi), %esi
	movq	__imp___iob_func(%rip), %rbx
	call	*%rbx
	leaq	.LC70(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%rsi, %r8
	call	fprintf
	movq	16(%rdi), %rsi
	call	*%rbx
	leaq	.LC71(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%rsi, %r8
	call	fprintf
	cmpl	$-1073741819, (%rdi)
	je	.L573
.L546:
	call	*%rbx
	leaq	96(%rax), %rcx
	call	fflush
	leaq	160(%rsp), %rcx
	call	fake_foreign_function_call
	leaq	.LC74(%rip), %rcx
	call	lose
.L541:
	movq	184(%rsp), %rax
	movq	%rbp, 72(%rsp)
	leaq	.LC69(%rip), %r9
	movl	%r12d, 64(%rsp)
	movl	%edi, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	leaq	.LC4(%rip), %rcx
	movl	$2744, %r8d
	movq	%rax, 56(%rsp)
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L572:
	leaq	.LC67(%rip), %rcx
	movl	$5, %edx
	call	c_level_backtrace
	leaq	160(%rsp), %rcx
	call	fake_foreign_function_call
	leaq	.LC68(%rip), %rcx
	call	lose
.L573:
	movl	$48, %r8d
	leaq	112(%rsp), %rdx
	movq	%r13, %rcx
	call	*__imp_VirtualQuery(%rip)
	testq	%rax, %rax
	je	.L547
	movl	144(%rsp), %esi
	call	*%rbx
	leaq	.LC72(%rip), %rdx
	leaq	96(%rax), %rcx
	movl	%esi, %r8d
	call	fprintf
.L547:
	movq	32(%rdi), %rsi
	call	*%rbx
	leaq	.LC73(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%r13, %r9
	movq	%rsi, %r8
	call	fprintf
	jmp	.L546
	.seh_endproc
	.p2align 4,,15
	.globl	handle_exception
	.def	handle_exception;	.scl	2;	.type	32;	.endef
	.seh_proc	handle_exception
handle_exception:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$216, %rsp
	.seh_stackalloc	216
	.seh_endprologue
	movl	$1, %eax
	testq	%r8, %r8
	movq	%rcx, %rbx
	movq	%r8, %rsi
	je	.L575
	testb	$6, 4(%rcx)
	je	.L612
.L575:
	addq	$216, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L612:
	movq	%rdx, 104(%rsp)
	movq	%r9, 96(%rsp)
	movq	__imp_GetLastError(%rip), %r15
	call	*%r15
	movq	__imp__errno(%rip), %r13
	movl	%eax, 124(%rsp)
	call	*%r13
	mov	specials(%rip), %ebp
	movl	(%rbx), %ecx
	movl	(%rax), %r14d
	movq	40(%rbx), %r12
	movl	%ecx, 112(%rsp)
	call	pthread_self
	movq	%rax, %rdi
	movq	1432(%rax,%rbp,8), %rbp
	movl	dyndebug_seh(%rip), %eax
	movq	104(%rsp), %rdx
	mov	112(%rsp), %ecx
	movq	96(%rsp), %r9
	testl	%eax, %eax
	jne	.L613
.L576:
	xorl	%eax, %eax
	testq	%rbp, %rbp
	movq	%rsi, 176(%rsp)
	je	.L577
	movq	8(%rbp), %rax
	movl	40(%rax), %eax
.L577:
	cmpl	$-2147483645, %ecx
	movl	%eax, 184(%rsp)
	je	.L614
	cmpl	$-2147483647, %ecx
	je	.L615
	cmpl	$-1073741819, %ecx
	je	.L616
.L585:
	movl	internal_errors_enabled(%rip), %r8d
	testl	%r8d, %r8d
	je	.L599
/APP
 # 2763 "win32-os.c" 1
	fnclex
 # 0 "" 2
/NO_APP
	leaq	176(%rsp), %rsi
	xorl	%ecx, %ecx
	leaq	8(%rsi), %rdx
	call	block_blockable_signals
	movq	%rsi, %rcx
	call	fake_foreign_function_call
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	(%rdx), %rbp
	testq	%rbp, %rbp
	jne	.L617
	movq	%rsi, %rcx
	call	alloc_sap
	movq	%rbx, %rcx
	movq	%rax, %rbp
	call	alloc_sap
	movq	537922568, %rdx
	movq	%rax, %r8
	movl	184(%rsp), %eax
	andq	$-16, %rdx
	movl	%eax, 40(%rdi)
	movq	16(%rdx), %rcx
	movq	%rbp, %rdx
	call	funcall2
.L601:
	movq	%rsi, %rcx
	call	undo_fake_foreign_function_call
	movl	184(%rsp), %eax
	movl	%eax, 40(%rdi)
.L581:
	call	*%r13
	movl	124(%rsp), %ecx
	movl	%r14d, (%rax)
	call	*__imp_SetLastError(%rip)
	xorl	%eax, %eax
	jmp	.L575
	.p2align 4,,10
.L616:
	movl	dyndebug_pagefaults(%rip), %r11d
	testl	%r11d, %r11d
	jne	.L618
.L586:
	testq	%rbp, %rbp
	je	.L587
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	testq	%rax, %rax
	je	.L588
	cmpq	%r12, 16(%rax)
	jbe	.L619
.L589:
	cmpq	%r12, 64(%rax)
	ja	.L588
	cmpq	%r12, 72(%rax)
	jbe	.L588
.L590:
	leaq	176(%rsp), %rcx
	movq	%r12, %rdx
	call	handle_guard_page_triggered
	testl	%eax, %eax
	je	.L588
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	240(%rax), %rdx
	cmpq	$0, (%rdx)
	je	.L581
	movq	$0, (%rdx)
	movq	$0, 248(%rax)
	jmp	.L581
	.p2align 4,,10
.L613:
	movzbl	260(%rsi), %eax
	movq	%rdx, %r8
	movq	%rbx, %rdx
	movl	%eax, 80(%rsp)
	movq	128(%rsi), %rax
	movq	%rcx, 64(%rsp)
	movq	%rbp, 56(%rsp)
	leaq	.LC62(%rip), %rcx
	movq	%r12, 48(%rsp)
	movq	%rax, 72(%rsp)
	movq	248(%rsi), %rax
	movq	%r9, 32(%rsp)
	movq	%rsi, %r9
	movq	%rax, 40(%rsp)
	call	odprintf_
	movl	(%rbx), %ecx
	jmp	.L576
	.p2align 4,,10
.L617:
	movq	$0, (%rdx)
	movq	248(%rax), %r12
	movq	$0, 248(%rax)
	movq	%rsi, %rcx
	call	alloc_sap
	movq	%rbx, %rcx
	movq	%rax, %r15
	call	alloc_sap
	movq	537922568, %rdx
	movq	%rax, %r8
	movl	184(%rsp), %eax
	andq	$-16, %rdx
	movl	%eax, 40(%rdi)
	movq	16(%rdx), %rcx
	movq	%r15, %rdx
	call	funcall2
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	%rbp, (%rdx)
	movq	%r12, 248(%rax)
	jmp	.L601
	.p2align 4,,10
.L588:
	cmpq	$65534, %r12
	jbe	.L620
.L587:
	cmpq	$553648128, %r12
	je	.L621
	testq	%rbp, %rbp
	je	.L592
	cmpq	%r12, 240(%rbp)
	je	.L622
.L592:
	movq	%r12, %rcx
	call	find_page_index
	cmpl	$-1, %eax
	je	.L593
	cltq
	salq	$4, %rax
	addq	page_table(%rip), %rax
	testb	$1, 10(%rax)
	jne	.L623
	movabsq	$68719476735, %rdx
	movq	core_mmap_end(%rip), %rax
	cmpq	%rdx, %r12
	jbe	.L595
	testq	%rax, %rax
	je	.L595
	cmpq	%r12, %rax
	ja	.L581
.L595:
	movq	os_vm_page_size(%rip), %rdx
	movl	$64, %r9d
	movl	$4096, %r8d
	movq	%rdx, %rcx
	negq	%rcx
	andq	%r12, %rcx
	call	*__imp_VirtualAlloc(%rip)
	movl	dyndebug_skip_averlax(%rip), %ecx
	testl	%ecx, %ecx
	jne	.L581
	testq	%rax, %rax
	jne	.L581
	leaq	.LC65(%rip), %rdx
	xorl	%r9d, %r9d
	movl	$2717, %r8d
	xorl	%ecx, %ecx
	call	win_aver.part.3.constprop.8
	jmp	.L581
	.p2align 4,,10
.L619:
	cmpq	%r12, %rax
	ja	.L590
	.p2align 4,,8
	jmp	.L589
	.p2align 4,,10
.L593:
	cmpq	undefined_alien_address(%rip), %r12
	je	.L585
	movq	os_vm_page_size(%rip), %rdx
	movl	$64, %r9d
	movl	$4096, %r8d
	movq	%rdx, %rcx
	negq	%rcx
	andq	%r12, %rcx
	call	*__imp_VirtualAlloc(%rip)
	testq	%rax, %rax
	movl	$1, %edx
	je	.L624
.L596:
	movl	dyndebug_skip_averlax(%rip), %r10d
	testl	%r10d, %r10d
	jne	.L581
	testl	%edx, %edx
	jne	.L581
	leaq	.LC2(%rip), %rax
	movq	%rax, 200(%rsp)
	call	*%r15
	movl	%eax, %ebx
	call	*%r13
	movl	(%rax), %ebp
	call	*%r13
	movl	(%rax), %ecx
	call	strerror
	movq	%rax, %rsi
	leaq	200(%rsp), %rax
	movl	$1033, %r9d
	xorl	%edx, %edx
	movq	$0, 48(%rsp)
	movl	$1024, 40(%rsp)
	movq	%rax, 32(%rsp)
	movl	%ebx, %r8d
	movl	$4352, %ecx
	call	*__imp_FormatMessageA(%rip)
	movl	dyndebug_survive_aver(%rip), %r9d
	movl	%eax, %r12d
	testl	%r9d, %r9d
	je	.L597
	movq	200(%rsp), %rax
	movq	%rsi, 80(%rsp)
	leaq	.LC3(%rip), %r8
	movl	%ebp, 72(%rsp)
	movl	%ebx, 56(%rsp)
	leaq	.LC4(%rip), %rdx
	movq	dyndebug_output(%rip), %rcx
	movl	$2744, %r9d
	movq	%rax, 64(%rsp)
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	$0, 40(%rsp)
	movq	%rax, 48(%rsp)
	leaq	.LC69(%rip), %rax
	movq	%rax, 32(%rsp)
	call	fprintf
	testl	%r12d, %r12d
	je	.L581
	movq	200(%rsp), %rcx
	call	*__imp_LocalFree(%rip)
	jmp	.L581
	.p2align 4,,10
.L614:
	leaq	176(%rsp), %rsi
	movq	%rsi, %rcx
	call	os_context_pc_addr
	movq	(%rax), %rax
	cmpb	$-52, (%rax)
	je	.L625
.L579:
	movq	%rsi, %rcx
	call	os_context_pc_addr
	movq	(%rax), %rax
	movq	%rsi, %rcx
	movzbl	(%rax), %ebx
	cmpl	$9, %ebx
	je	.L626
	call	os_context_sp_addr
	movq	(%rax), %rax
	movq	%rax, 232(%rbp)
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	(%rdx), %rbp
	testq	%rbp, %rbp
	jne	.L627
	leaq	8(%rsi), %rdx
	xorl	%ecx, %ecx
	call	block_blockable_signals
	movl	%ebx, %edx
	movq	%rsi, %rcx
	call	handle_trap
	movl	184(%rsp), %eax
	movl	%eax, 40(%rdi)
	jmp	.L581
	.p2align 4,,10
.L615:
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	240(%rax), %rdx
	xorl	%esi, %esi
	movq	(%rdx), %rbx
	testq	%rbx, %rbx
	je	.L584
	movq	$0, (%rdx)
	movq	248(%rax), %rsi
	movq	$0, 248(%rax)
.L584:
	leaq	176(%rsp), %rcx
	movq	%r12, %rdx
	call	handle_guard_page_triggered
	testl	%eax, %eax
	jne	.L581
	mov	specials(%rip), %eax
	testq	%rbx, %rbx
	movq	1432(%rdi,%rax,8), %rax
	je	.L581
	movq	240(%rax), %rdx
	movq	%rbx, (%rdx)
	movq	%rsi, 248(%rax)
	jmp	.L581
	.p2align 4,,10
.L618:
	movq	32(%rbx), %rax
	movq	%r12, 48(%rsp)
	leaq	.LC63(%rip), %rcx
	movq	%rbp, %rdx
	movq	%rax, 56(%rsp)
	movq	176(%rsi), %rax
	movq	%rax, 40(%rsp)
	movq	168(%rsi), %rax
	movq	%rax, 32(%rsp)
	movq	152(%rsi), %r9
	movq	248(%rsi), %r8
	call	odprintf_
	jmp	.L586
	.p2align 4,,10
.L627:
	movq	$0, (%rdx)
	movq	248(%rax), %r12
	movq	$0, 248(%rax)
	leaq	8(%rsi), %rdx
	xorl	%ecx, %ecx
	call	block_blockable_signals
	movl	%ebx, %edx
	movq	%rsi, %rcx
	call	handle_trap
	movl	184(%rsp), %eax
	movl	%eax, 40(%rdi)
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	240(%rax), %rdx
	movq	%rbp, (%rdx)
	movq	%r12, 248(%rax)
	jmp	.L581
	.p2align 4,,10
.L626:
	call	arch_skip_instruction
	movq	%rsi, %rcx
	call	thread_interrupted
	.p2align 4,,2
	jmp	.L581
	.p2align 4,,10
.L623:
	movq	%r12, %rcx
	call	gencgc_handle_wp_violation
	.p2align 4,,5
	jmp	.L581
.L622:
	leaq	176(%rsp), %rcx
	call	thread_in_safety_transition
	jmp	.L581
.L625:
	movq	%rsi, %rcx
	call	os_context_pc_addr
	addq	$1, (%rax)
	jmp	.L579
.L621:
	leaq	176(%rsp), %rcx
	call	thread_in_lisp_raised
	jmp	.L581
.L624:
	movq	248(%rsi), %rbx
	call	*__imp___iob_func(%rip)
	leaq	.LC66(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%r12, %r8
	movq	%rbx, %r9
	call	fprintf
	testl	%eax, %eax
	jne	.L628
	xorl	%edx, %edx
	jmp	.L596
.L599:
	mov	(%rbx), %edi
	movq	__imp___iob_func(%rip), %rsi
	call	*%rsi
	leaq	.LC70(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%rdi, %r8
	call	fprintf
	movq	16(%rbx), %rdi
	call	*%rsi
	leaq	.LC71(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%rdi, %r8
	call	fprintf
	cmpl	$-1073741819, (%rbx)
	je	.L629
.L602:
	call	*%rsi
	leaq	96(%rax), %rcx
	call	fflush
	leaq	176(%rsp), %rcx
	call	fake_foreign_function_call
	leaq	.LC74(%rip), %rcx
	call	lose
.L597:
	movq	200(%rsp), %rax
	movq	%rsi, 72(%rsp)
	leaq	.LC69(%rip), %r9
	movl	%ebp, 64(%rsp)
	movl	%ebx, 48(%rsp)
	leaq	.LC3(%rip), %rdx
	leaq	.LC4(%rip), %rcx
	movl	$2744, %r8d
	movq	%rax, 56(%rsp)
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	$0, 32(%rsp)
	movq	%rax, 40(%rsp)
	call	lose
.L629:
	movl	$48, %r8d
	leaq	128(%rsp), %rdx
	movq	%r12, %rcx
	call	*__imp_VirtualQuery(%rip)
	testq	%rax, %rax
	je	.L603
	movl	160(%rsp), %edi
	call	*%rsi
	leaq	.LC72(%rip), %rdx
	leaq	96(%rax), %rcx
	movl	%edi, %r8d
	call	fprintf
.L603:
	movq	32(%rbx), %rbx
	call	*%rsi
	leaq	.LC73(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%r12, %r9
	movq	%rbx, %r8
	call	fprintf
	jmp	.L602
.L620:
	movq	160(%rsi), %r13
	movq	152(%rsi), %rdi
	movq	248(%rsi), %rbx
	call	*__imp___iob_func(%rip)
	leaq	.LC64(%rip), %rdx
	leaq	96(%rax), %rcx
	movq	%r12, %r9
	movq	%rbp, %r8
	movq	%r13, 48(%rsp)
	movq	%rdi, 40(%rsp)
	movq	%rbx, 32(%rsp)
	call	fprintf
	orl	$-1, %ecx
	call	*__imp_Sleep(%rip)
	xorl	%ecx, %ecx
	call	*__imp_ExitProcess(%rip)
.L628:
	leaq	.LC67(%rip), %rcx
	movl	$5, %edx
	call	c_level_backtrace
	leaq	176(%rsp), %rcx
	call	fake_foreign_function_call
	leaq	.LC68(%rip), %rcx
	call	lose
	nop
	.seh_endproc
	.p2align 4,,15
	.globl	wos_install_interrupt_handlers
	.def	wos_install_interrupt_handlers;	.scl	2;	.type	32;	.endef
	.seh_proc	wos_install_interrupt_handlers
wos_install_interrupt_handlers:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	movl	once.76177(%rip), %eax
	testl	%eax, %eax
	je	.L632
	addq	$8, %rsp
	ret
	.p2align 4,,10
.L632:
	movq	__imp_AddVectoredExceptionHandler(%rip), %rax
	leaq	veh(%rip), %rdx
	xorl	%ecx, %ecx
	movl	$1, once.76177(%rip)
	addq	$8, %rsp
	rex.W jmp *%rax
	.seh_endproc
	.section .rdata,"dr"
	.align 8
.LC75:
	.ascii "Pathname too long in dirname.\12\0"
	.text
	.p2align 4,,15
	.globl	dirname
	.def	dirname;	.scl	2;	.type	32;	.endef
	.seh_proc	dirname
dirname:
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$40, %rsp
	.seh_stackalloc	40
	.seh_endprologue
	movq	%rcx, %rsi
	call	strlen
	cmpq	$260, %rax
	movq	%rax, %rbx
	ja	.L641
	leaq	buf.76181(%rip), %rcx
	movq	%rsi, %rdx
	call	strcpy
	mov	%ebx, %r9d
	leaq	buf.76181(%rip), %r8
	xorl	%eax, %eax
	addq	$1, %r9
	.p2align 4,,10
.L638:
	movl	%ebx, %ecx
	subl	%eax, %ecx
	movslq	%ecx, %rcx
	movzbl	(%r8,%rcx), %edx
	cmpb	$92, %dl
	je	.L639
	cmpb	$47, %dl
	je	.L639
	addq	$1, %rax
	cmpq	%r9, %rax
	jne	.L638
	leaq	buf.76181(%rip), %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%rsi
	ret
	.p2align 4,,10
.L639:
	leaq	buf.76181(%rip), %rax
	movb	$0, (%r8,%rcx)
	addq	$40, %rsp
	popq	%rbx
	popq	%rsi
	ret
.L641:
	leaq	.LC75(%rip), %rcx
	call	lose
	nop
	.seh_endproc
	.p2align 4,,15
	.globl	os_get_runtime_executable_path
	.def	os_get_runtime_executable_path;	.scl	2;	.type	32;	.endef
	.seh_proc	os_get_runtime_executable_path
os_get_runtime_executable_path:
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$304, %rsp
	.seh_stackalloc	304
	.seh_endprologue
	testl	%ecx, %ecx
	je	.L643
	xorl	%ecx, %ecx
	movl	$261, %r8d
	leaq	32(%rsp), %rdx
	xorl	%ebx, %ebx
	call	*__imp_GetModuleFileNameA(%rip)
	testl	%eax, %eax
	je	.L644
	cmpl	$261, %eax
	je	.L648
.L645:
	leaq	32(%rsp), %rcx
	call	copied_string
	movq	%rax, %rbx
.L644:
	movq	%rbx, %rax
	addq	$304, %rsp
	popq	%rbx
	ret
	.p2align 4,,10
.L643:
	leaq	.LC47(%rip), %rcx
	call	copied_string
	movq	%rax, %rbx
	movq	%rbx, %rax
	addq	$304, %rsp
	popq	%rbx
	ret
	.p2align 4,,10
.L648:
	call	*__imp_GetLastError(%rip)
	cmpl	$122, %eax
	jne	.L645
	.p2align 4,,5
	jmp	.L644
	.seh_endproc
	.p2align 4,,15
	.globl	socket_input_available
	.def	socket_input_available;	.scl	2;	.type	32;	.endef
	.seh_proc	socket_input_available
socket_input_available:
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$104, %rsp
	.seh_stackalloc	104
	.seh_endprologue
	movq	%rcx, %rsi
	movl	$0, 80(%rsp)
	movl	$0, 92(%rsp)
	call	*__imp_GetLastError(%rip)
	movl	%eax, %ebx
	leaq	92(%rsp), %rax
	xorl	%r9d, %r9d
	xorl	%r8d, %r8d
	movq	%rsi, %rcx
	movq	$0, 64(%rsp)
	movq	%rax, 48(%rsp)
	leaq	80(%rsp), %rax
	movq	$0, 56(%rsp)
	movl	$4, 40(%rsp)
	movl	$1074030207, %edx
	xorl	%esi, %esi
	movq	%rax, 32(%rsp)
	call	*__imp_WSAIoctl(%rip)
	testl	%eax, %eax
	jne	.L650
	cmpl	$1, 80(%rsp)
	sbbl	%esi, %esi
	notl	%esi
	addl	$2, %esi
.L650:
	movl	%ebx, %ecx
	call	*__imp_SetLastError(%rip)
	movl	%esi, %eax
	addq	$104, %rsp
	popq	%rbx
	popq	%rsi
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	console_handle_p
	.def	console_handle_p;	.scl	2;	.type	32;	.endef
	.seh_proc	console_handle_p
console_handle_p:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	xorl	%eax, %eax
	leaq	-1(%rcx), %rdx
	cmpq	$-3, %rdx
	ja	.L654
	andl	$3, %ecx
	xorl	%eax, %eax
	cmpl	$3, %ecx
	sete	%al
.L654:
	addq	$8, %rsp
	ret
	.seh_endproc
	.section .rdata,"dr"
.LC76:
	.ascii "WriteConsole fails => %u\12\0"
	.text
	.p2align 4,,15
	.globl	win32_write_unicode_console
	.def	win32_write_unicode_console;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_write_unicode_console
win32_write_unicode_console:
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$64, %rsp
	.seh_stackalloc	64
	.seh_endprologue
	cmpq	$0, ptr_CancelIoEx(%rip)
	movq	%rcx, %rbx
	movq	%rdx, %rsi
	movl	%r8d, %ebp
	movl	$0, 48(%rsp)
	je	.L657
	mov	specials(%rip), %r12d
	call	pthread_self
	movq	%rax, %rdi
	movq	1432(%rax,%r12,8), %rdx
	xorl	%eax, %eax
	lock cmpxchgq	%rbx, 264(%rdx)
	jne	.L665
.L657:
	movl	%ebp, %r8d
	movq	$0, 32(%rsp)
	leaq	48(%rsp), %r9
	sarl	%r8d
	movq	%rsi, %rdx
	movq	%rbx, %rcx
	call	*__imp_WriteConsoleW(%rip)
	cmpq	$0, ptr_CancelIoEx(%rip)
	movl	%eax, %edi
	je	.L659
	mov	specials(%rip), %esi
	call	pthread_self
	xorl	%ecx, %ecx
	movq	1432(%rax,%rsi,8), %rdx
	movq	%rbx, %rax
	lock cmpxchgq	%rcx, 264(%rdx)
.L659:
	testl	%edi, %edi
	je	.L660
	movl	48(%rsp), %eax
	testl	%eax, %eax
	je	.L666
	addl	%eax, %eax
	addq	$64, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	ret
	.p2align 4,,10
.L665:
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	144(%rax), %rcx
	call	*__imp_ResetEvent(%rip)
	mov	specials(%rip), %eax
	movq	1432(%rdi,%rax,8), %rax
	movq	$0, 264(%rax)
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
	movl	$-1, %eax
.L658:
	addq	$64, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	ret
	.p2align 4,,10
.L660:
	call	*__imp_GetLastError(%rip)
	movl	%eax, %ebx
	movl	dyndebug_io(%rip), %eax
	testl	%eax, %eax
	jne	.L667
.L662:
	call	*__imp__errno(%rip)
	xorl	%edx, %edx
	cmpl	$995, %ebx
	setne	%dl
	addl	$4, %edx
	movl	%edx, (%rax)
	movl	$-1, %eax
	jmp	.L658
	.p2align 4,,10
.L666:
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
	movl	$-1, %eax
	jmp	.L658
.L667:
	leaq	.LC76(%rip), %rcx
	movl	%ebx, %edx
	call	odprintf_
	jmp	.L662
	.seh_endproc
	.p2align 4,,15
	.globl	win32_tty_listen
	.def	win32_tty_listen;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_tty_listen
win32_tty_listen:
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	leaq	DEAD_MUTEX(%rip), %rbx
	movq	%rcx, %rsi
	movq	32776+ttyinput(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.L679
.L669:
	cmpq	$-1, %rcx
	je	.L680
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L671:
	movl	32888+ttyinput(%rip), %eax
	testl	%eax, %eax
	je	.L681
.L672:
	movl	32904+ttyinput(%rip), %eax
	xorl	%esi, %esi
	testl	%eax, %eax
	jne	.L673
	movl	32772+ttyinput(%rip), %eax
	cmpl	%eax, 32768+ttyinput(%rip)
	movb	$1, %sil
	je	.L682
.L673:
	movq	32776+ttyinput(%rip), %rcx
	cmpq	%rbx, %rcx
	je	.L683
.L674:
	addq	$64, %rcx
	call	*__imp_LeaveCriticalSection(%rip)
	movl	%esi, %eax
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	ret
	.p2align 4,,10
.L681:
	movq	__imp_GetCurrentProcess(%rip), %rbp
	call	*%rbp
	movq	%rax, %rdi
	call	*%rbp
	movl	$2, 48(%rsp)
	movq	%rax, %rcx
	movl	$0, 40(%rsp)
	movl	$0, 32(%rsp)
	leaq	32896+ttyinput(%rip), %r9
	movq	%rdi, %r8
	movq	%rsi, %rdx
	call	*__imp_DuplicateHandle(%rip)
	testl	%eax, %eax
	je	.L672
	leaq	32784+ttyinput(%rip), %rcx
	xorl	%edx, %edx
	call	pthread_cond_init
	leaq	32832+ttyinput(%rip), %rcx
	xorl	%edx, %edx
	call	pthread_cond_init
	leaq	tty_read_line_server(%rip), %r8
	leaq	32880+ttyinput(%rip), %rcx
	xorl	%r9d, %r9d
	xorl	%edx, %edx
	call	pthread_create
	movl	$1, 32888+ttyinput(%rip)
	jmp	.L672
	.p2align 4,,10
.L682:
	leaq	96(%rsp), %r9
	movl	$1, %r8d
	leaq	64(%rsp), %rdx
	movq	32896+ttyinput(%rip), %rcx
	xorb	%sil, %sil
	call	*__imp_PeekConsoleInputA(%rip)
	testl	%eax, %eax
	je	.L673
	movl	96(%rsp), %eax
	testl	%eax, %eax
	je	.L673
	leaq	32832+ttyinput(%rip), %rcx
	movl	$1, 32904+ttyinput(%rip)
	call	pthread_cond_broadcast
	jmp	.L673
	.p2align 4,,10
.L680:
	leaq	32776+ttyinput(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L671
	.p2align 4,,10
.L683:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L674
	.p2align 4,,10
.L679:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L669
	.seh_endproc
	.p2align 4,,15
	.globl	win32_read_unicode_console
	.def	win32_read_unicode_console;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_read_unicode_console
win32_read_unicode_console:
	subq	$8, %rsp
	.seh_stackalloc	8
	.seh_endprologue
	addq	$8, %rsp
	jmp	tty_read_line_client
	.seh_endproc
	.p2align 4,,15
	.globl	win32_maybe_interrupt_io
	.def	win32_maybe_interrupt_io;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_maybe_interrupt_io
win32_maybe_interrupt_io:
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$32, %rsp
	.seh_stackalloc	32
	.seh_endprologue
	xorl	%eax, %eax
	cmpq	$0, ptr_CancelIoEx(%rip)
	movq	%rcx, %rsi
	je	.L686
	movq	$-1, %rbx
/APP
 # 1434 "/home/anton/sdks/orgbin/cross-amd64-win64-20110206/bin/../lib/gcc/x86_64-w64-mingw32/4.6.0/../../../../x86_64-w64-mingw32/include/winnt.h" 1
	lock ; xchgq %rbx,264(%rcx)
 # 0 "" 2
/NO_APP
	leaq	-1(%rbx), %rdx
	cmpq	$-3, %rdx
	ja	.L686
	movl	%ebx, %eax
	andl	$3, %eax
	cmpl	$3, %eax
	je	.L700
	xorl	%edi, %edi
	cmpq	$0, ptr_CancelSynchronousIo(%rip)
	je	.L692
.L707:
	movq	8(%rsi), %rbp
	leaq	DEAD_MUTEX(%rip), %rdi
	movq	160(%rbp), %rcx
	leaq	160(%rbp), %r12
	cmpq	%rdi, %rcx
	je	.L701
.L693:
	cmpq	$-1, %rcx
	je	.L702
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L695:
	movq	8(%rsi), %rax
	movq	152(%rax), %rax
	movq	16(%rax), %rcx
	call	*ptr_CancelSynchronousIo(%rip)
	movq	8(%rsi), %rsi
	movl	%eax, %ebp
	movq	160(%rsi), %rcx
	cmpq	%rdi, %rcx
	je	.L703
.L696:
	addq	$64, %rcx
	call	*__imp_LeaveCriticalSection(%rip)
	testl	%ebp, %ebp
	setne	%dil
.L692:
	xorl	%edx, %edx
	movq	%rbx, %rcx
	call	*ptr_CancelIoEx(%rip)
	testl	%eax, %eax
	setne	%al
	orl	%edi, %eax
	movzbl	%al, %eax
.L686:
	addq	$32, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	ret
	.p2align 4,,10
.L700:
	movq	32776+ttyinput(%rip), %rcx
	leaq	DEAD_MUTEX(%rip), %rdi
	cmpq	%rdi, %rcx
	je	.L704
.L688:
	cmpq	$-1, %rcx
	je	.L705
	addq	$64, %rcx
	call	*__imp_EnterCriticalSection(%rip)
.L690:
	leaq	32784+ttyinput(%rip), %rcx
	call	pthread_cond_broadcast
	movq	32776+ttyinput(%rip), %rcx
	cmpq	%rdi, %rcx
	je	.L706
.L691:
	addq	$64, %rcx
	xorl	%edi, %edi
	call	*__imp_LeaveCriticalSection(%rip)
	cmpq	$0, ptr_CancelSynchronousIo(%rip)
	jne	.L707
	jmp	.L692
	.p2align 4,,10
.L703:
	leaq	160(%rsi), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	160(%rsi), %rcx
	jmp	.L696
	.p2align 4,,10
.L702:
	movq	%r12, %rcx
	call	pthread_mutex_lock
	jmp	.L695
	.p2align 4,,10
.L701:
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	movq	%r12, %r9
	call	pthread_np_lose
	movq	160(%rbp), %rcx
	jmp	.L693
	.p2align 4,,10
.L706:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L691
	.p2align 4,,10
.L705:
	leaq	32776+ttyinput(%rip), %rcx
	call	pthread_mutex_lock
	jmp	.L690
.L704:
	leaq	32776+ttyinput(%rip), %r9
	leaq	.LC5(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	32776+ttyinput(%rip), %rcx
	jmp	.L688
	.seh_endproc
	.p2align 4,,15
	.globl	win32_unix_write
	.def	win32_unix_write;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_unix_write
win32_unix_write:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	mov	specials(%rip), %edi
	movl	%ecx, %ebx
	movq	%rdx, %r14
	movl	%r8d, %r13d
	call	pthread_self
	movl	%ebx, %ecx
	movq	%rax, %rsi
	movq	1432(%rax,%rdi,8), %rbp
	call	*__imp__get_osfhandle(%rip)
	movq	%rax, %rbx
	leaq	-1(%rax), %rax
	cmpq	$-3, %rax
	ja	.L709
	movl	%ebx, %eax
	andl	$3, %eax
	cmpl	$3, %eax
	je	.L723
.L709:
	movq	144(%rbp), %rax
	movl	$1, %r9d
	leaq	80(%rsp), %r8
	movq	zero_large_offset(%rip), %rdx
	movq	%rbx, %rcx
	movq	__imp_SetFilePointerEx(%rip), %r12
	movq	%rax, 72(%rsp)
	call	*%r12
	testl	%eax, %eax
	movl	%eax, %edi
	jne	.L719
	movl	$0, 64(%rsp)
	movl	$0, 68(%rsp)
.L711:
	cmpq	$0, ptr_CancelIoEx(%rip)
	je	.L712
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rdx
	xorl	%eax, %eax
	lock cmpxchgq	%rbx, 264(%rdx)
	jne	.L724
.L712:
	leaq	48(%rsp), %r15
	movl	%r13d, %r8d
	leaq	96(%rsp), %r9
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%r15, 32(%rsp)
	call	*__imp_WriteFile(%rip)
	cmpq	$0, ptr_CancelIoEx(%rip)
	movl	%eax, %r8d
	je	.L713
	mov	specials(%rip), %eax
	xorl	%ecx, %ecx
	movq	1432(%rsi,%rax,8), %rdx
	movq	%rbx, %rax
	lock cmpxchgq	%rcx, 264(%rdx)
.L713:
	testl	%r8d, %r8d
	je	.L725
.L714:
	testl	%edi, %edi
	jne	.L726
.L718:
	movl	96(%rsp), %eax
.L710:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L719:
	movl	80(%rsp), %eax
	movl	%eax, 64(%rsp)
	movl	84(%rsp), %eax
	movl	%eax, 68(%rsp)
	jmp	.L711
	.p2align 4,,10
.L723:
	movl	%r13d, %r8d
	movq	%r14, %rdx
	movq	%rbx, %rcx
	call	win32_write_unicode_console
	jmp	.L710
	.p2align 4,,10
.L724:
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	144(%rax), %rcx
	call	*__imp_ResetEvent(%rip)
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	$0, 264(%rax)
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
	movl	$-1, %eax
	jmp	.L710
	.p2align 4,,10
.L725:
	movq	__imp_GetLastError(%rip), %rsi
	call	*%rsi
	cmpl	$997, %eax
	jne	.L717
	movl	$-1, %r9d
	xorl	%r8d, %r8d
	leaq	144(%rbp), %rdx
	movl	$2, %ecx
	call	*__imp_WaitForMultipleObjects(%rip)
	xorl	%r9d, %r9d
	testl	%eax, %eax
	jne	.L727
.L716:
	leaq	96(%rsp), %r8
	movq	%r15, %rdx
	movq	%rbx, %rcx
	call	*__imp_GetOverlappedResult(%rip)
	testl	%eax, %eax
	jne	.L714
	call	*%rsi
	cmpl	$995, %eax
	.p2align 4,,4
	je	.L728
.L717:
	.p2align 4,,6
	call	*__imp__errno(%rip)
	movl	$5, (%rax)
	movl	$-1, %eax
	jmp	.L710
	.p2align 4,,10
.L726:
	mov	96(%rsp), %eax
	xorl	%r9d, %r9d
	xorl	%r8d, %r8d
	addq	%rax, 80(%rsp)
	movq	%rbx, %rcx
	movq	80(%rsp), %rdx
	call	*%r12
	jmp	.L718
	.p2align 4,,10
.L727:
	movq	%rbx, %rcx
	call	*__imp_CancelIo(%rip)
	movl	$1, %r9d
	jmp	.L716
	.p2align 4,,10
.L728:
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
	movl	$-1, %eax
	jmp	.L710
	.seh_endproc
	.p2align 4,,15
	.globl	win32_unix_read
	.def	win32_unix_read;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_unix_read
win32_unix_read:
	pushq	%r15
	.seh_pushreg	%r15
	pushq	%r14
	.seh_pushreg	%r14
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$120, %rsp
	.seh_stackalloc	120
	.seh_endprologue
	mov	specials(%rip), %edi
	movl	%ecx, %ebx
	movq	%rdx, %r14
	movl	%r8d, %r13d
	movq	$0, 48(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
	movl	$0, 96(%rsp)
	call	pthread_self
	movl	%ebx, %ecx
	movq	%rax, %rsi
	movq	1432(%rax,%rdi,8), %rbp
	call	*__imp__get_osfhandle(%rip)
	movq	%rax, %rbx
	leaq	-1(%rax), %rax
	cmpq	$-3, %rax
	ja	.L730
	movl	%ebx, %eax
	andl	$3, %eax
	cmpl	$3, %eax
	je	.L744
.L730:
	movq	144(%rbp), %rax
	movl	$1, %r9d
	leaq	80(%rsp), %r8
	movq	zero_large_offset(%rip), %rdx
	movq	%rbx, %rcx
	movq	__imp_SetFilePointerEx(%rip), %r12
	movq	%rax, 72(%rsp)
	call	*%r12
	testl	%eax, %eax
	movl	%eax, %edi
	jne	.L740
	movl	$0, 64(%rsp)
	movl	$0, 68(%rsp)
.L732:
	cmpq	$0, ptr_CancelIoEx(%rip)
	je	.L733
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rdx
	xorl	%eax, %eax
	lock cmpxchgq	%rbx, 264(%rdx)
	jne	.L745
.L733:
	leaq	48(%rsp), %r15
	movl	%r13d, %r8d
	leaq	96(%rsp), %r9
	movq	%r14, %rdx
	movq	%rbx, %rcx
	movq	%r15, 32(%rsp)
	call	*__imp_ReadFile(%rip)
	cmpq	$0, ptr_CancelIoEx(%rip)
	movl	%eax, %r8d
	je	.L734
	mov	specials(%rip), %eax
	xorl	%ecx, %ecx
	movq	1432(%rsi,%rax,8), %rdx
	movq	%rbx, %rax
	lock cmpxchgq	%rcx, 264(%rdx)
.L734:
	testl	%r8d, %r8d
	je	.L746
.L735:
	testl	%edi, %edi
	jne	.L747
.L739:
	movl	96(%rsp), %eax
.L731:
	addq	$120, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	ret
	.p2align 4,,10
.L740:
	movl	80(%rsp), %eax
	movl	%eax, 64(%rsp)
	movl	84(%rsp), %eax
	movl	%eax, 68(%rsp)
	jmp	.L732
	.p2align 4,,10
.L744:
	movl	%r13d, %r8d
	movq	%r14, %rdx
	movq	%rbx, %rcx
	call	tty_read_line_client
	jmp	.L731
	.p2align 4,,10
.L745:
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	144(%rax), %rcx
	call	*__imp_ResetEvent(%rip)
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	movq	$0, 264(%rax)
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
	movl	$-1, %eax
	jmp	.L731
	.p2align 4,,10
.L746:
	movq	__imp_GetLastError(%rip), %rsi
	call	*%rsi
	cmpl	$109, %eax
	je	.L735
	cmpl	$38, %eax
	je	.L735
	cmpl	$64, %eax
	.p2align 4,,2
	je	.L735
	cmpl	$997, %eax
	jne	.L738
	movl	$-1, %r9d
	xorl	%r8d, %r8d
	leaq	144(%rbp), %rdx
	movl	$2, %ecx
	call	*__imp_WaitForMultipleObjects(%rip)
	xorl	%r9d, %r9d
	testl	%eax, %eax
	jne	.L748
.L737:
	leaq	96(%rsp), %r8
	movq	%r15, %rdx
	movq	%rbx, %rcx
	call	*__imp_GetOverlappedResult(%rip)
	testl	%eax, %eax
	jne	.L735
	call	*%rsi
	cmpl	$38, %eax
	.p2align 4,,6
	je	.L735
	cmpl	$995, %eax
	.p2align 4,,3
	je	.L749
.L738:
	call	*__imp__errno(%rip)
	movl	$5, (%rax)
	movl	$-1, %eax
	jmp	.L731
	.p2align 4,,10
.L747:
	mov	96(%rsp), %eax
	xorl	%r9d, %r9d
	xorl	%r8d, %r8d
	addq	%rax, 80(%rsp)
	movq	%rbx, %rcx
	movq	80(%rsp), %rdx
	call	*%r12
	jmp	.L739
	.p2align 4,,10
.L748:
	movq	%rbx, %rcx
	call	*__imp_CancelIo(%rip)
	movl	$1, %r9d
	jmp	.L737
.L749:
	call	*__imp__errno(%rip)
	movl	$4, (%rax)
	movl	$-1, %eax
	jmp	.L731
	.seh_endproc
	.p2align 4,,15
	.globl	win32_wait_object_or_signal
	.def	win32_wait_object_or_signal;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_wait_object_or_signal
win32_wait_object_or_signal:
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$56, %rsp
	.seh_stackalloc	56
	.seh_endprologue
	mov	specials(%rip), %esi
	movq	%rcx, %rbx
	call	pthread_self
	leaq	32(%rsp), %rdx
	movl	$-1, %r9d
	xorl	%r8d, %r8d
	movq	1432(%rax,%rsi,8), %rax
	movl	$2, %ecx
	movq	%rbx, 32(%rsp)
	movq	152(%rax), %rax
	movq	%rax, 40(%rsp)
	call	*__imp_WaitForMultipleObjects(%rip)
	addq	$56, %rsp
	popq	%rbx
	popq	%rsi
	ret
	.seh_endproc
	.p2align 4,,15
	.globl	win32_suspend_get_context
	.def	win32_suspend_get_context;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_suspend_get_context
win32_suspend_get_context:
	pushq	%r13
	.seh_pushreg	%r13
	pushq	%r12
	.seh_pushreg	%r12
	pushq	%rbp
	.seh_pushreg	%rbp
	pushq	%rdi
	.seh_pushreg	%rdi
	pushq	%rsi
	.seh_pushreg	%rsi
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$40, %rsp
	.seh_stackalloc	40
	.seh_endprologue
	xorl	%r13d, %r13d
	movq	%rcx, %rsi
	leaq	sprof_suspend_lock(%rip), %rcx
	call	pthread_mutex_trylock
	testl	%eax, %eax
	jne	.L752
	leaq	all_threads_lock(%rip), %rcx
	leaq	DEAD_MUTEX(%rip), %rbp
	call	pthread_mutex_trylock
	testl	%eax, %eax
	movq	__imp_LeaveCriticalSection(%rip), %rdi
	jne	.L767
	movq	all_threads(%rip), %rbx
	testq	%rbx, %rbx
	jne	.L758
	jmp	.L754
	.p2align 4,,10
.L771:
	movq	176(%rbx), %rbx
	testq	%rbx, %rbx
	je	.L754
.L758:
	cmpq	%rsi, 8(%rbx)
	jne	.L771
	movq	%rsi, %rcx
	call	pthread_np_suspend
.L754:
	movq	all_threads_lock(%rip), %rcx
	leaq	DEAD_MUTEX(%rip), %rbp
	cmpq	%rbp, %rcx
	je	.L775
.L759:
	addq	$64, %rcx
	movq	__imp_LeaveCriticalSection(%rip), %rdi
	call	*%rdi
	testq	%rbx, %rbx
	je	.L767
	movl	$1, %edx
	movl	$1280, %ecx
	call	calloc
	leaq	16(%rax), %rdx
	movq	%rax, %r12
	movq	%rsi, %rcx
	movq	%r12, %r13
	movq	%rdx, (%rax)
	call	pthread_np_get_thread_context
	movq	%r12, %rcx
	call	os_context_pc_addr
	cmpq	$0, (%rax)
	jne	.L760
	mov	specials(%rip), %eax
	movq	1432(%rsi,%rax,8), %rax
	testq	%rax, %rax
	je	.L776
	movq	240(%rax), %rax
	movq	%r12, %rcx
	movq	(%rax), %rbx
	movq	(%rbx), %rbp
	movq	-8(%rbx), %rdi
	call	os_context_pc_addr
	movq	%r12, %rcx
	movq	%rbp, (%rax)
	call	os_context_fp_addr
	movq	%r12, %rcx
	movq	%rdi, (%rax)
	call	os_context_sp_addr
	movq	%rbx, (%rax)
.L763:
	movq	%rsi, 1248(%r12)
.L752:
	movq	%r13, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
	.p2align 4,,10
.L760:
	movq	%r12, %rcx
	call	os_context_fp_addr
	movq	72(%rbx), %rcx
	movq	(%rax), %rax
	jmp	.L764
	.p2align 4,,10
.L769:
	movq	%rdx, %rax
.L764:
	cmpq	%rcx, %rax
	jae	.L763
	cmpq	64(%rbx), %rax
	jbe	.L763
	movq	(%rax), %rdx
	cmpq	%rdx, %rax
	jae	.L763
	testb	$3, %dl
	je	.L769
	movq	%rdx, 1264(%r12)
	movq	%rax, 1256(%r12)
	andq	$-4, (%rax)
	jmp	.L763
.L776:
.L762:
	movq	%rsi, %rcx
	call	pthread_np_resume
	movq	%r12, %rcx
	call	free
	.p2align 4,,10
.L767:
	movq	sprof_suspend_lock(%rip), %rax
	cmpq	%rbp, %rax
	je	.L777
.L766:
	leaq	64(%rax), %rcx
	xorl	%r13d, %r13d
	call	*%rdi
	movq	%r13, %rax
	addq	$40, %rsp
	popq	%rbx
	popq	%rsi
	popq	%rdi
	popq	%rbp
	popq	%r12
	popq	%r13
	ret
.L775:
	leaq	all_threads_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	all_threads_lock(%rip), %rcx
	jmp	.L759
.L777:
	leaq	sprof_suspend_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	sprof_suspend_lock(%rip), %rax
	jmp	.L766
	.seh_endproc
	.p2align 4,,15
	.globl	win32_resume
	.def	win32_resume;	.scl	2;	.type	32;	.endef
	.seh_proc	win32_resume
win32_resume:
	pushq	%rbx
	.seh_pushreg	%rbx
	subq	$32, %rsp
	.seh_stackalloc	32
	.seh_endprologue
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	je	.L778
	movq	1256(%rcx), %rax
	testq	%rax, %rax
	je	.L780
	movq	1264(%rcx), %rdx
	movq	%rdx, (%rax)
.L780:
	movq	1248(%rbx), %rcx
	call	pthread_np_resume
	movq	%rbx, %rcx
	call	free
	movq	sprof_suspend_lock(%rip), %rcx
	leaq	DEAD_MUTEX(%rip), %rax
	cmpq	%rax, %rcx
	je	.L782
.L781:
	movq	__imp_LeaveCriticalSection(%rip), %rax
	addq	$64, %rcx
	addq	$32, %rsp
	popq	%rbx
	rex.W jmp *%rax
	.p2align 4,,10
.L778:
	addq	$32, %rsp
	popq	%rbx
	ret
	.p2align 4,,10
.L782:
	leaq	sprof_suspend_lock(%rip), %r9
	leaq	.LC7(%rip), %r8
	leaq	.LC6(%rip), %rdx
	movl	$5, %ecx
	call	pthread_np_lose
	movq	sprof_suspend_lock(%rip), %rcx
	jmp	.L781
	.seh_endproc
	.globl	sprof_suspend_lock
	.data
	.align 16
sprof_suspend_lock:
	.quad	-1
	.globl	ttyinput
	.align 32
ttyinput:
	.space 32776
	.quad	-1
	.space 128
	.globl	sb_stat_fpu_modeswitch_cycling_counter
	.bss
	.align 4
sb_stat_fpu_modeswitch_cycling_counter:
	.space 4
	.globl	sb_control_update_sb_stat_counters
	.data
	.align 4
sb_control_update_sb_stat_counters:
	.long	1
	.globl	sb_control_fpu_normally_untouched_by_non_float_calls
	.align 4
sb_control_fpu_normally_untouched_by_non_float_calls:
	.long	1
	.globl	core_mmap_unshared_pages
	.bss
	.align 4
core_mmap_unshared_pages:
	.space 4
	.globl	mwwFlag
	.align 16
mwwFlag:
	.space 4
	.globl	os_number_of_processors
	.data
	.align 4
os_number_of_processors:
	.long	1
	.comm	ptr_CancelSynchronousIo, 8, 4
	.comm	ptr_CancelIoEx, 8, 4
	.comm	ptr_ResetWriteWatch, 8, 4
	.comm	ptr_GetWriteWatch, 8, 4
	.comm	fiber_funeral_service, 8, 3
	.comm	dead_fiber_queue, 8, 3
	.comm	core_mmap_end, 8, 4
	.comm	real_uwp_seh_handler, 8, 3
	.comm	base_seh_frame, 8, 3
	.globl	dyndebug_output
	.bss
	.align 8
dyndebug_output:
	.space 8
	.globl	dyndebug_charge_count
	.align 4
dyndebug_charge_count:
	.space 4
	.globl	dyndebug_to_odstring
	.align 4
dyndebug_to_odstring:
	.space 4
	.globl	dyndebug_to_filestream
	.data
	.align 16
dyndebug_to_filestream:
	.long	1
	.globl	dyndebug_misc
	.bss
	.align 16
dyndebug_misc:
	.space 4
	.globl	dyndebug_seh
	.align 4
dyndebug_seh:
	.space 4
	.globl	dyndebug_io
	.align 4
dyndebug_io:
	.space 4
	.globl	dyndebug_pagefaults
	.align 16
dyndebug_pagefaults:
	.space 4
	.globl	dyndebug_safepoints
	.align 16
dyndebug_safepoints:
	.space 4
	.globl	dyndebug_runtime_link
	.align 16
dyndebug_runtime_link:
	.space 4
	.globl	dyndebug_survive_aver
	.align 16
dyndebug_survive_aver:
	.space 4
	.globl	dyndebug_skip_averlax
	.align 16
dyndebug_skip_averlax:
	.space 4
	.globl	dyndebug_lazy_fpu_careful
	.align 4
dyndebug_lazy_fpu_careful:
	.space 4
	.globl	dyndebug_lazy_fpu
	.align 4
dyndebug_lazy_fpu:
	.space 4
	.comm	os_vm_page_size, 8, 4
	.comm	DEAD_MUTEX, 128, 7
	.data
	.align 16
loglock.75747:
	.quad	-1
	.align 16
fiber_factory_lock:
	.quad	-1
.lcomm fiber_factory_callback,8,8
.lcomm fiber_factory_fiber,8,16
.lcomm fiber_factory_condition,48,32
.lcomm lisp_fiber_key,4,4
.lcomm save_lisp_tls_key,4,4
.lcomm writeAddrs.75962,4096,32
.lcomm os_supports_executable_mapping,4,16
	.align 16
os_mmap_protect_modes:
	.quad	os_protect_modes
	.align 32
os_mmap_exec_modes:
	.long	1
	.long	2
	.long	8
	.long	8
	.long	16
	.long	32
	.long	128
	.long	128
	.align 32
os_mmap_noexec_modes:
	.long	1
	.long	2
	.long	8
	.long	8
	.long	1
	.long	2
	.long	8
	.long	8
	.align 32
os_protect_modes:
	.long	1
	.long	2
	.long	4
	.long	4
	.long	16
	.long	32
	.long	64
	.long	64
.lcomm once.76177,4,16
.lcomm buf.76181,261,32
	.section .rdata,"dr"
	.align 8
zero_large_offset:
	.space 8
	.def	pthread_np_switch_to_fiber;	.scl	2;	.type	32;	.endef
	.def	fprintf;	.scl	2;	.type	32;	.endef
	.def	strerror;	.scl	2;	.type	32;	.endef
	.def	pthread_self;	.scl	2;	.type	32;	.endef
	.def	lose;	.scl	2;	.type	32;	.endef
	.def	funcall3;	.scl	2;	.type	32;	.endef
	.def	pthread_cond_wait;	.scl	2;	.type	32;	.endef
	.def	pthread_cond_broadcast;	.scl	2;	.type	32;	.endef
	.def	pthread_mutex_lock;	.scl	2;	.type	32;	.endef
	.def	pthread_np_lose;	.scl	2;	.type	32;	.endef
	.def	memcpy;	.scl	2;	.type	32;	.endef
	.def	pthread_cond_init;	.scl	2;	.type	32;	.endef
	.def	pthread_create;	.scl	2;	.type	32;	.endef
	.def	sprintf;	.scl	2;	.type	32;	.endef
	.def	fputs;	.scl	2;	.type	32;	.endef
	.def	t_nil_s;	.scl	2;	.type	32;	.endef
	.def	vsprintf;	.scl	2;	.type	32;	.endef
	.def	block_deferrable_signals;	.scl	2;	.type	32;	.endef
	.def	pthread_cond_signal;	.scl	2;	.type	32;	.endef
	.def	pthread_join;	.scl	2;	.type	32;	.endef
	.def	pthread_np_notice_thread;	.scl	2;	.type	32;	.endef
	.def	pthread_np_run_in_fiber;	.scl	2;	.type	32;	.endef
	.def	pthread_key_create;	.scl	2;	.type	32;	.endef
	.def	pthread_mutex_init;	.scl	2;	.type	32;	.endef
	.def	find_page_index;	.scl	2;	.type	32;	.endef
	.def	page_address;	.scl	2;	.type	32;	.endef
	.def	gencgc_handle_wp_violation;	.scl	2;	.type	32;	.endef
	.def	fwrite;	.scl	2;	.type	32;	.endef
	.def	fflush;	.scl	2;	.type	32;	.endef
	.def	undefined_alien_function;	.scl	2;	.type	32;	.endef
	.def	arch_write_linkage_table_ref;	.scl	2;	.type	32;	.endef
	.def	arch_write_linkage_table_jmp;	.scl	2;	.type	32;	.endef
	.def	fast_bzero;	.scl	2;	.type	32;	.endef
	.def	lseek;	.scl	2;	.type	32;	.endef
	.def	read;	.scl	2;	.type	32;	.endef
	.def	block_blockable_signals;	.scl	2;	.type	32;	.endef
	.def	fake_foreign_function_call;	.scl	2;	.type	32;	.endef
	.def	alloc_sap;	.scl	2;	.type	32;	.endef
	.def	funcall2;	.scl	2;	.type	32;	.endef
	.def	undo_fake_foreign_function_call;	.scl	2;	.type	32;	.endef
	.def	handle_guard_page_triggered;	.scl	2;	.type	32;	.endef
	.def	thread_in_lisp_raised;	.scl	2;	.type	32;	.endef
	.def	os_context_pc_addr;	.scl	2;	.type	32;	.endef
	.def	os_context_sp_addr;	.scl	2;	.type	32;	.endef
	.def	handle_trap;	.scl	2;	.type	32;	.endef
	.def	arch_skip_instruction;	.scl	2;	.type	32;	.endef
	.def	thread_interrupted;	.scl	2;	.type	32;	.endef
	.def	thread_in_safety_transition;	.scl	2;	.type	32;	.endef
	.def	strlen;	.scl	2;	.type	32;	.endef
	.def	strcpy;	.scl	2;	.type	32;	.endef
	.def	copied_string;	.scl	2;	.type	32;	.endef
	.def	pthread_mutex_trylock;	.scl	2;	.type	32;	.endef
	.def	pthread_np_suspend;	.scl	2;	.type	32;	.endef
	.def	calloc;	.scl	2;	.type	32;	.endef
	.def	pthread_np_get_thread_context;	.scl	2;	.type	32;	.endef
	.def	os_context_fp_addr;	.scl	2;	.type	32;	.endef
	.def	pthread_np_resume;	.scl	2;	.type	32;	.endef
	.def	free;	.scl	2;	.type	32;	.endef
