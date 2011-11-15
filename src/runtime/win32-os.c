/*
 * the Win32 incarnation of OS-dependent routines.  See also
 * $(sbcl_arch)-win32-os.c
 *
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. Surprise surprise, this
 * interface looks a lot like the Mach interface (but simpler in some
 * places). For some operating systems, a subset of these functions
 * will have to be emulated.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

/*
 * This file was copied from the Linux version of the same, and
 * likely still has some linuxisms in it have haven't been elimiated
 * yet.
 */

#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/param.h>
#include <sys/file.h>
#include <io.h>
#include "sbcl.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "sbcl.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "runtime.h"
#include "alloc.h"
#include "genesis/primitive-objects.h"
#include "dynbind.h"

#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include <math.h>

#include <excpt.h>

#include "validate.h"
#include "thread.h"
#include "cpputil.h"

os_vm_size_t os_vm_page_size;

#include "gc.h"
#include "gencgc-internal.h"
#include <winsock2.h>

#if 0
int linux_sparc_siginfo_bug = 0;
int linux_supports_futex=0;
#endif

#include <stdarg.h>
#include <string.h>

/* missing definitions for modern mingws */
#ifndef EH_UNWINDING
#define EH_UNWINDING 0x02
#endif
#ifndef EH_EXIT_UNWIND
#define EH_EXIT_UNWIND 0x04
#endif

/* Local `policy' macro, to prefer code that doesn't conceal
 * problems, even if it's less effective. */
#define EXTRA_CHECK_DOES_NOT_HURT

/* Define if core mapping should be done in allocation unit
 * chunks. */

/* #define COMPOSITE_MAPPING 1 */
#undef COMPOSITE_MAPPING

/* As of non-mmapped dynamic space, we may use GetWriteWatch(...)
 * where it's available to monitor ``WP violation'' without actually
 * setting WP and handling exceptions.
 *
 * As of mmapped-space, we can't GetWriteWatch() on it; there is a
 * couple of viable alternatives to actual WP + EH as well, but they
 * require partial-unmapping of the core to be possible (and
 * all-in-one mapping doesn't allow partial unmapping).
 *
 * For now, we resort to good old WP+EH for mmapped space, so the core
 * may be mapped all at once.  */

int dyndebug_lazy_fpu = 0;
int dyndebug_lazy_fpu_careful = 0;
int dyndebug_skip_averlax = 0;
int dyndebug_survive_aver = 0;
int dyndebug_runtime_link = 0;
int dyndebug_safepoints = 0;
int dyndebug_pagefaults = 0;
int dyndebug_io = 0;
int dyndebug_seh = 0;
int dyndebug_misc = 0;

int dyndebug_lowpagefault_halt = 0;
int dyndebug_lowpagefault_log = 0;

int dyndebug_to_filestream = 1;
int dyndebug_to_odstring = 0;


unsigned int dyndebug_charge_count = 0;
FILE* dyndebug_output = NULL;

/* Tired of writing arch_os_get_current_thread each time.  It does
 * make sense, but I'd better "commit" it with search/replace after
 * big chunks of work... */
#define this_thread (arch_os_get_current_thread())

/* dyndebug_... functions and variables solve two problems: sometimes
 * a bug doesn't occur with full :sb-show support (or other similar
 * logging facilities) because of different timings. Then we may
 * enable required dyndebug_[category] separately without getting full
 * report on each step (believe me, levels don't help too much
 * here. TOPICS do).
 *
 * dyndebug_... control flags are settable without recompilation, even
 * without restarting SBCL (as EXTERN_ALIENs). Recompilation and
 * restarting is the second problem I had in mind. Even the latter may
 * change the world in a way that an almost-tracked-down problem
 * vanishes and doesn't reproduce.
 *
 * Setting those flags from (the presense of) OS environment variables
 * may be potentially insecure for production builds, so it will be
 * removed eventually; availability of the flags with (extern-alien)
 * doesn't have such implications, so it'd better be retained. */

static inline void dyndebug_init()
{
    dyndebug_output = stderr;

    dyndebug_lazy_fpu = GetEnvironmentVariableA("SBCL_DYNDEBUG__LAZY_FPU",NULL,0);
    dyndebug_lazy_fpu_careful =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__LAZY_FPU_CAREFUL",NULL,0);
    dyndebug_skip_averlax =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__SKIP_AVERLAX",NULL,0);
    dyndebug_survive_aver =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__SURVIVE_AVER",NULL,0);
    dyndebug_runtime_link =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__RUNTIME_LINK",NULL,0);
    dyndebug_safepoints =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__SAFEPOINTS",NULL,0);
    dyndebug_pagefaults =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__PAGEFAULTS",NULL,0);
    dyndebug_io =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__IO",NULL,0);
    dyndebug_seh =
        GetEnvironmentVariableA("SBCL_DYNDEBUG__SEH",NULL,0);
}

/* wrappers for winapi calls that must be successful (like SBCL's
 * (aver ...) form). */

/* win_aver function: basic building block for miscellaneous
 * ..AVER.. macrology (below) */

static inline
intptr_t win_aver(intptr_t value, char* comment, char* file, int line,
               int justwarn)
{
    if (dyndebug_skip_averlax)
        return value;
    if (!value) {
        LPSTR errorMessage = "<FormatMessage failed>";
        DWORD errorCode = GetLastError(), allocated=0;
        int posixerrno = errno;
        const char* posixstrerror = strerror(errno);
        char* report_template =
            "Expression unexpectedly false: %s:%d\n"
            " ... %s\n"
            "     ===> returned #X%p, \n"
            "     (in thread %p)"
            " ... Win32 thinks:\n"
            "     ===> code %u, message => %s\n"
            " ... CRT thinks:\n"
            "     ===> code %u, message => %s\n";

        allocated =
            FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER|
                           FORMAT_MESSAGE_FROM_SYSTEM,
                           NULL,
                           errorCode,
                           MAKELANGID(LANG_ENGLISH,SUBLANG_ENGLISH_US),
                           (LPSTR)&errorMessage,
                           1024u,
                           NULL);

        if (justwarn || dyndebug_survive_aver) {
            fprintf(dyndebug_output, report_template,
                    file, line,
                    comment, value,
                    this_thread,
                    (unsigned)errorCode, errorMessage,
                    posixerrno, posixstrerror);
        } else {
            lose(report_template,
                    file, line,
                    comment, value,
                    this_thread,
                    (unsigned)errorCode, errorMessage,
                    posixerrno, posixstrerror);
        }
        if (allocated)
            LocalFree(errorMessage);
    }
    return value;
}

/* sys_aver function: really tiny adaptor of win_aver for
 * "POSIX-parody" CRT results ("lowio" and similar stuff):
 * negative number means something... negative. */
static inline
intptr_t sys_aver(long value, char* comment, char* file, int line,
              int justwarn)
{
    win_aver((intptr_t)(value>=0),comment,file,line,justwarn);
    return value;
}

/* Check for (call) result being boolean true. (call) may be arbitrary
 * expression now; massive attack of gccisms ensures transparent type
 * conversion back and forth, so the type of AVER(expression) is the
 * type of expression. Value is the same _if_ it can be losslessly
 * converted to (void*) and back.
 *
 * Failed AVER() is normally fatal. Well, unless dyndebug_survive_aver
 * flag is set. */

#define AVER(call)                                                      \
    ({ __typeof__(call) __attribute__((unused)) me =                    \
            (__typeof__(call))                                          \
            win_aver((intptr_t)(call), #call, __FILE__, __LINE__, 0);      \
        me;})

/* AVERLAX(call): do the same check as AVER did, but be mild on
 * failure: print an annoying unrequested message to stderr, and
 * continue. With dyndebug_skip_averlax flag, AVERLAX stop even to
 * check and complain. */

#define AVERLAX(call)                                                   \
    ({ __typeof__(call) __attribute__((unused)) me =                    \
            (__typeof__(call))                                          \
            win_aver((intptr_t)(call), #call, __FILE__, __LINE__, 1);      \
        me;})

/* Now, when failed AVER... prints both errno and GetLastError(), two
 * variants of "POSIX/lowio" style checks below are almost useless
 * (they build on sys_aver like the two above do on win_aver). */

#define CRT_AVER_NONNEGATIVE(call)                              \
    ({ __typeof__(call) __attribute__((unused)) me =            \
            (__typeof__(call))                                  \
            sys_aver((call), #call, __FILE__, __LINE__, 0);     \
        me;})

#define CRT_AVERLAX_NONNEGATIVE(call)                           \
    ({ __typeof__(call) __attribute__((unused)) me =            \
            (__typeof__(call))                                  \
            sys_aver((call), #call, __FILE__, __LINE__, 1);     \
        me;})

/* to be removed */
#define CRT_AVER(booly)                                         \
    ({ __typeof__(booly) __attribute__((unused)) me = (booly);  \
        sys_aver((booly)?0:-1, #booly, __FILE__, __LINE__, 0);  \
        me;})

/* Another logging mechanism, useful with debugview.exe by
 * sysinternals.com */
void odprint(const char * msg)
{
    char buf[1024];
    DWORD lastError = GetLastError();
#if defined(LISP_FEATURE_SB_THREAD)
    sprintf(buf, "[0x%p] %s\n", pthread_self(), msg);
    OutputDebugString(buf);
#else
    OutputDebugString(msg);
#endif
    SetLastError(lastError);
}
const char * t_nil_s(lispobj symbol);

/* odprintf_: print formatted string (by passing va_list), prefixed by
 * some fixed context information (GC_SAFE and pthread_self() were
 * very important some months back -- TODO now `thread SAP' and
 * csp_/ctx_ stuff took their place).
 *
 * As of 2011/01/23, it's used in odxprint() macro, and odxprint is
 * what I use now for categorized message logging.
 */

void odprintf_(const char * fmt, ...)
{
    char buf[1024];
    va_list args;
    int n;
    DWORD lastError = GetLastError();
    struct thread * self = arch_os_get_current_thread();
#if defined(LISP_FEATURE_SB_THREAD)
    if (self) {
        sprintf(buf, "[0x%p] [0x%04lx] [x%p] %s, %s, %s, %s ", pthread_self(),
                GetCurrentThreadId(), self,
                t_nil_s(GC_SAFE), t_nil_s(GC_INHIBIT), t_nil_s(INTERRUPTS_ENABLED), t_nil_s(IN_SAFEPOINT));
    } else {
        sprintf(buf, "[0x%p] [0x%04lx] (arch_os_get_current_thread() is NULL) ", pthread_self(),GetCurrentThreadId());
    }
#else
    buf[0] = 0;
#endif
    n = strlen(buf);
    va_start(args, fmt);
    vsprintf(buf + n, fmt, args);
    va_end(args);
    n = strlen(buf);
    buf[n] = '\n';
    buf[n + 1] = 0;

    if (dyndebug_to_odstring)
        OutputDebugString(buf);
    if (dyndebug_to_filestream) {
        static pthread_mutex_t loglock = PTHREAD_MUTEX_INITIALIZER;
        pthread_mutex_lock(&loglock);
        fprintf(dyndebug_output,"%s",buf);
        pthread_mutex_unlock(&loglock);
    }
    SetLastError(lastError);
}

/* As of win32, deferrables _do_ matter. gc_signal doesn't. */
unsigned long block_deferrables_and_return_mask()
{
    sigset_t sset;
    block_deferrable_signals(0, &sset);
    return (unsigned long)sset;
}

#if defined(LISP_FEATURE_SB_THREAD)
void apply_sigmask(unsigned long sigmask)
{
    sigset_t sset = (sigset_t)sigmask;
    pthread_sigmask(SIG_SETMASK, &sset, 0);
}
#endif

/* The exception handling function looks like this: */
EXCEPTION_DISPOSITION handle_exception(EXCEPTION_RECORD *,
                                       struct lisp_exception_frame *,
                                       CONTEXT *,
                                       void *);
/* handle_exception is defined further in this file, but it doesn't
 * get registered as SEH handler, not even by
 * wos_install_interrupt_handlers anymore. x86-assem.S provides
 * exception_handler_wrapper; we install it here, and each exception
 * frame on nested funcall()s also points to it
 */


/* Two UL_ funs below aren't called by any SBCL code. Sometimes we
 * have a REPL thread (e.g. in SLIME) and a stream of logged info
 * going separately to stderr; it may be very interesting what
 * messages are about _this REPL_. This pair of functions
 * is what you may need to (ALIEN-FUNCALL) in such situation.
 *
 * (current-thread-sap), equal to arch_os_get_current_thread(), is
 * available in Lisp, but sometimes we need to be one step earlier in
 * the pointer chain. It's possible to get TEB flat-memory pointer and
 * close-to-current EBP value with preexisting SBCL Lisp facilities,
 * but ALIEN-FUNCALL is easier (NB Fibers are deceptive
 * w.r.t. thread-control-stack-end).
 */

void *UL_GetCurrentTeb() { return NtCurrentTeb(); };
void *UL_GetCurrentFrame() { return __builtin_frame_address(0); }

void *base_seh_frame;
void *real_uwp_seh_handler;

os_vm_address_t core_mmap_end;

HMODULE runtime_module_handle = 0u;

static void *get_seh_frame(void)
{
    void* retval;
#ifdef LISP_FEATURE_X86
    asm volatile ("mov %%fs:0,%0": "=r" (retval));
#else
    asm volatile ("mov %%gs:0,%0": "=r" (retval));
#endif
    return retval;
}

static void set_seh_frame(void *frame)
{
#ifdef LISP_FEATURE_X86
    asm volatile ("mov %0,%%fs:0": : "r" (frame));
#else
    asm volatile ("mov %0,%%gs:0": : "r" (frame));
#endif
}

#if 0
static struct lisp_exception_frame *find_our_seh_frame(void)
{
    struct lisp_exception_frame *frame = get_seh_frame();

    while (frame->handler != handle_exception)
        frame = frame->next_frame;

    return frame;
}

inline static void *get_stack_frame(void)
{
    void* retval;
    asm volatile ("movl %%ebp,%0": "=r" (retval));
    return retval;
}
#endif






#if defined(LISP_FEATURE_SB_THREAD)

/* Allocate (reserve+commit) a page so that each thread running Lisp
 * code will never iterate in a loop without a single Load from it.
 *
 * GC stops threads running Lisp code by revoking read permission for
 * this page: then sooner or later all running Lisp threads will
 * trap. Read thread.c for further details.
 */
void alloc_gc_page()
{
    AVER(VirtualAlloc(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj),
                      MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE));
}

/* Permit loads from GC_SAFEPOINT_PAGE_ADDR (NB page state change is
 * "synchronized" with the memory region content/availability --
 * e.g. you won't see other CPU flushing buffered writes after WP --
 * but there is some window when other thread _seem_ to trap AFTER
 * access is granted. You may think of it something like "OS enters
 * SEH handler too slowly" -- what's important is there's no implicit
 * synchronization between VirtualProtect caller and other thread's
 * SEH handler, hence no ordering of events. VirtualProtect is
 * implicitly synchronized with protected memory contents (only).
 *
 * The last fact may be potentially used with many benefits e.g. for
 * foreign call speed, but we don't use it for now: almost the only
 * fact relevant to the current signalling protocol is "sooner or
 * later everyone will trap [everyone will stop trapping]".
 *
 * An interesting source on page-protection-based inter-thread
 * communication is a well-known paper by Dave Dice, Hui Huang,
 * Mingyao Yang: ``Asymmetric Dekker Synchronization''. Last time
 * I checked it was available at
 * http://home.comcast.net/~pjbishop/Dave/Asymmetric-Dekker-Synchronization.txt
 */
void map_gc_page()
{
    DWORD oldProt;
    AVER(VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj),
                        PAGE_READWRITE, &oldProt));
}


/* This one is unused yet. Loads from memory page don't synchronize
 * threads; even on X86 systems -- that are probably the most
 * "user-friendly" w.r.t. memory ordering -- CPU itself normally
 * "doesn't know _when_ a Load happened" (yes, I know there is no
 * "when" in superscalar arch; this is the problem).
 *
 * Stores are different in this respect. IA-32 is not the only
 * architecture guaranting that (normal) Stores from a single CPU _are
 * not observable_ to be done in some other order than the
 * program-specified one.
 *
 * However, storing something to the same word at
 * GC_SAFEPOINT_PAGE_ADDR would (1) be useless it itself (2)
 * constantly ping-pong the cache line, normally being in a nice
 * Shared state when we use Loads.
 *
 * Anyway, Lisp threads may left to be trapped "sooner or later" as
 * they are now. However, there _is_ a kind of threads that is
 * important to synchronize with GC: threads /entering/ or /leaving/ a
 * foreign function call. The next steps seems reasonable: (1) let
 * such threads _store_ to gc page, (2) give each of them an unique
 * range in it (better a cache line per thread), (3) let them store
 * their sensible context information, like current
 * th->csp_around_foreign_call that is currently, simultaneously, our
 * "in foreign code" flag and a half of our context needed for
 * conservative GC if we are doing a foreign call.
 *
 * NB (1) entails two separate semantics for Load and Store GC-page
 * permission: ``Load prohibited'' means ``hey Lispers, it's time to
 * sleep'' (we all know how it happens: sooner or later it will, but
 * when exactly...), and ``Store prohibited'' means ``Anyone entering
 * or leaving the CL community MUST consult [his SEH handler] first''.
 * It's probably better to use separate page, removing an accidental
 * dependency (as I remember, ``write-only'' flag can't be relied to
 * prevent reading on some Windows versions; happy to be corrected,
 * but if it's true, we'd have a hard time using a single page for
 * both signals).
 *
 * To be implemented...
 */
void map_gc_page_readonly()
{
    DWORD oldProt;
    AVER(VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj),
                        PAGE_READONLY, &oldProt));
}

void unmap_gc_page()
{
    DWORD oldProt;
    AVER(VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, sizeof(lispobj),
                        PAGE_NOACCESS, &oldProt));
}

void do_nothing() {}

void** os_get_csp(struct thread* th)
{
    odxprint(safepoints,"Thread %p has CSP [%p] %p => %p",
             th,th->csp_around_foreign_call,
             *(void***)th->csp_around_foreign_call,
             th->control_stack_end);
    return *(void***)th->csp_around_foreign_call;
}

#endif

#if defined(LISP_FEATURE_SB_DYNAMIC_CORE)
/*
 *    #|
 *
 *        Kovalenko: I'll eventually move all huge comments, like this
 *        one, into a separate document, but now let them hang around
 *        related code for a while. Such comment-monsters inspire me
 *        to keep them in sync when I update code, and occasionally
 *        reward me with reminding of something useful, like a
 *        long-planned feature becoming impossible after a change I'm
 *        going to make, an error I'm going to repeat, etc. etc.
 *
 *    |#
 *
 * This feature has already saved me more development time than it
 * took to implement.  In its current state, ``dynamic RT<->core
 * linking'' is a protocol of initialization of C runtime and Lisp
 * core, populating SBCL linkage table with entries for runtime
 * "foreign" symbols that were referenced in cross-compiled code.
 *
 * How it works: a sketch
 *
 * Last Genesis (resulting in cold-sbcl.core) binds foreign fixups in
 * x-compiled lisp-objs to sequential addresses from the beginning of
 * linkage-table space; that's how it ``resolves'' foreign references.
 * Obviously, this process doesn't require pre-built runtime presence.
 *
 * When the runtime loads the core (cold-sbcl.core initially,
 * sbcl.core later), runtime should do its part of the protocol by (1)
 * traversing a list of ``runtime symbols'' prepared by Genesis and
 * dumped as a static symbol value, (2) resolving each name from this
 * list to an address (stubbing unresolved ones with
 * undefined_alien_address or undefined_alien_function), (3) adding an
 * entry for each symbol somewhere near the beginning of linkage table
 * space (location is provided by the core).
 *
 * The implementation of the part described in the last paragraph
 * follows. C side is currently more ``hackish'' and less clear than
 * the Lisp code; OTOH, related Lisp changes are scattered, and some
 * of them play part in complex interrelations -- beautiful but taking
 * much time to understand --- but my subset of PE-i386 parser below
 * is in one place (here) and doesn't have _any_ non-trivial coupling
 * with the rest of the Runtime.
 *
 * What do we gain with this feature, after all?
 *
 * One things that I have to do rather frequently: recompile and
 * replace runtime without rebuilding the core. Doubtlessly, slam.sh
 * was a great time-saver here, but relinking ``cold'' core and bake a
 * ``warm'' one takes, as it seems, more than 10x times of bare
 * SBCL.EXE build time -- even if everything is recompiled, which is
 * now unnecessary. Today, if I have a new idea for the runtime,
 * getting from C-x C-s M-x ``compile'' to fully loaded SBCL
 * installation takes 5-15 seconds.
 *
 * Another thing (that I'm not currently using, but obviously
 * possible) is delivering software patches to remote system on
 * customer site. As you are doing minor additions or corrections in
 * Lisp code, it doesn't take much effort to prepare a tiny ``FASL
 * bundle'' that rolls up your patch, redumps and -- presto -- 100MiB
 * program is fixed by sending and loading a 50KiB thingie.
 *
 * However, until LISP_FEATURE_SB_DYNAMIC_CORE, if your bug were fixed
 * by modifying two lines of _C_ sources, a customer described above
 * had to be ready to receive and reinstall a new 100MiB
 * executable. With the aid of code below, deploying such a fix
 * requires only sending ~300KiB (when stripped) of SBCL.EXE.
 *
 * But there is more to it: as the common linkage-table is used for
 * DLLs and core, its entries may be overridden almost without a look
 * into SBCL internals. Therefore, ``patching'' C runtime _without_
 * restarting target systems is also possible in many situations
 * (it's not as trivial as loading FASLs into a running daemon, but
 * easy enough to be a viable alternative if any downtime is highly
 * undesirable).
 *
 * During my (rather limited) commercial Lisp development experience
 * I've already been through a couple of situations where such
 * ``deployment'' issues were important; from my _total_ programming
 * experience I know -- _sometimes_ they are a two orders of magnitude
 * more important than those I observed.
 *
 * The possibility of entire runtime ``hot-swapping'' in running
 * process is not purely theoretical, as it could seem. There are 2-3
 * problems whose solution is not obvious (call stack patching, for
 * instance), but it's literally _nothing_ if compared with
 * e.g. LISP_FEATURE_SB_AUTO_FPU_SWITCH.  By the way, one of the
 * problems with ``hot-swapping'', that could become a major one in
 * many other environments, is nonexistent in SBCL: we already have a
 * ``global quiesce point'' that is generally required for this kind
 * of worldwide revolution -- around collect_garbage.
 *
 * What's almost unnoticeable from the C side (where you are now, dear
 * reader): using the same style for all linking is beautiful. I tried
 * to leave old-style linking code in place for the sake of
 * _non-linkage-table_ platforms (they probably don't have -ldl or its
 * equivalent, like LL/GPA, at all) -- but i did it usually by moving
 * the entire `old style' code under #!-sb-dynamic-core and
 * refactoring the `new style' branch, instead of cutting the tail
 * piecemeal and increasing #!+-ifdeffery amount & the world enthropy.
 *
 * If we look at the majority of the ``new style'' code units, it's a
 * common thing to observe how #!+-ifdeffery _vanishes_ instead of
 * multiplying: #!-sb-xc, #!+sb-xc-host and #!-sb-xc-host end up
 * needing the same code. Runtime checks of static v. dynamic symbol
 * disappear even faster. STDCALL mangling and leading underscores go
 * out of scope (and GCed, hopefully) instead of surfacing here and
 * there as a ``special case for core static symbols''. What I like
 * the most about CL development in general is a frequency of solving
 * problems and fixing bugs by simplifying code and dropping special
 * cases.
 *
 * Last important thing about the following code: besides resolving
 * symbols provided by the core itself, it detects runtime's own
 * build-time prerequisite DLLs. Any symbol that is unresolved against
 * the core is looked up in those DLLs (normally kernel32, msvcrt,
 * ws2_32... I could forget something). This action (1) resembles
 * implementation of foreign symbol lookup in SBCL itself, (2)
 * emulates shared library d.l. facilities of OSes that use flat
 * dynamic symbol namespace (or default to it). Anyone concerned with
 * portability problems of this PE-i386 stuff below will be glad to
 * hear that it could be ported to most modern Unices _by deletion_:
 * raw dlsym() with null handle usually does the same thing that i'm
 * trying to squeeze out of MS Windows by the brute force.
 *
 * My reason for _desiring_ flat symbol namespace, populated from
 * link-time dependencies, is avoiding any kind of ``requested-by-Lisp
 * symbol lists to be linked statically'', providing core v. runtime
 * independence in both directions. Minimizing future maintenance
 * effort is very important; I had gone for it consistently, starting
 * by turning "CloseHandle@4" into a simple "CloseHandle", continuing
 * by adding intermediate Genesis resulting in autogenerated symbol
 * list (farewell, void scratch(); good riddance), going to take
 * another great step for core/runtime independence... and _without_
 * flat namespace emulation, the ghosts and spirits exiled at the
 * first steps would come and take revenge: well, here are the symbols
 * that are really in msvcrt.dll.. hmm, let's link statically against
 * them, so the entry is pulled from the import library.. and those
 * entry has mangled names that we have to map.. ENOUGH, I though
 * here: fed up with stuff like that.
 *
 * Now here we are, without import libraries, without mangled symbols,
 * and without nm-generated symbol tables. Every symbol exported by
 * the runtime is added to SBCL.EXE export directory; every symbol
 * requested by the core is looked up by GetProcAddress for SBCL.EXE,
 * falling back to GetProcAddress for MSVCRT.dll, etc etc.. All ties
 * between SBCL's foreign symbols with object file symbol tables,
 * import libraries and other pre-linking symbol-resolving entities
 * _having no representation in SBCL.EXE_ were teared.
 *
 * This simplistic approach proved to work well; there is only one
 * problem introduced by it, and rather minor: in real MSVCRT.dll,
 * what's used to be available as open() is now called _open();
 * similar thing happened to many other `lowio' functions, though not
 * every one, so it's not a kind of name mangling but rather someone's
 * evil creative mind in action.
 *
 * When we look up any of those poor `uglified' functions in CRT
 * reference on MSDN, we can see a notice resembling this one:
 *
 * `unixishname()' is obsolete and provided for backward
 * compatibility; new standard-compliant function, `_unixishname()',
 * should be used instead.  Sentences of that kind were there for
 * several years, probably even for a decade or more (a propos,
 * MSVCRT.dll, as the name to link against, predates year 2000, so
 * it's actually possible). Reasoning behing it (what MS people had in
 * mind) always seemed strange to me: if everyone uses open() and that
 * `everyone' is important to you, why rename the function?  If no one
 * uses open(), why provide or retain _open() at all? <kidding>After
 * all, names like _open() are entirely non-informative and just plain
 * ugly; compare that with CreateFileW() or InitCommonControlSEX(),
 * the real examples of beauty and clarity.</kidding>
 *
 * Anyway, if the /standard/ name on Windows is _open() (I start to
 * recall, vaguely, that it's because of _underscore names being
 * `reserved to system' and all other ones `available for user', per
 * ANSI/ISO C89) -- well, if the /standard/ name is _open, SBCL should
 * use it when it uses MSVCRT and not some ``backward-compatible''
 * stuff. Deciding this way, I added a hack to SBCL's syscall macros,
 * so "[_]open" as a syscall name is interpreted as a request to link
 * agains "_open" on win32 and "open" on every other system.
 *
 * Of course, this name-parsing trick lacks conceptual clarity; I'm
 * going to get rid of it eventually.  Maybe I will drop the whole
 * ``lowio'' stuff from win32 builds _before_ getting back to syscall
 * macros (going for kernel32/ntdll handles, after Alastair
 * Bridgewater's advice; or rolling my own ``lowio'' layer; or
 * inventing some other interface -- who knows?). */

u32 os_get_build_time_shared_libraries(u32 excl_maximum,
                                       void* opt_root,
                                       void** opt_store_handles,
                                       const char *opt_store_names[])
{
    void* base = opt_root ? opt_root : (void*)runtime_module_handle;
    /* base defaults to 0x400000 with GCC/mingw32. If you dereference
     * that location, you'll see 'MZ' bytes */
    void* base_magic_location =
        base + ((IMAGE_DOS_HEADER*)base)->e_lfanew;

    /* dos header provided the offset from `base' to
     * IMAGE_FILE_HEADER where PE-i386 really starts */

    void* check_duplicates[excl_maximum];

    if ((*(u32*)base_magic_location)!=0x4550) {
        /* We don't need this DLL thingie _that_ much. If the world
         * has changed to a degree where PE magic isn't found, let's
         * silently return `no libraries detected'. */
        return 0;
    } else {
        /* We traverse PE-i386 structures of SBCL.EXE in memory (not
         * in the file). File and memory layout _surely_ differ in
         * some places and _may_ differ in some other places, but
         * fortunately, those places are irrelevant to the task at
         * hand. */

        IMAGE_FILE_HEADER* image_file_header = (base_magic_location + 4);
        IMAGE_OPTIONAL_HEADER* image_optional_header =
            (void*)(image_file_header + 1);
        IMAGE_DATA_DIRECTORY* image_import_direntry =
            &image_optional_header->DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
        IMAGE_IMPORT_DESCRIPTOR* image_import_descriptor =
            base + image_import_direntry->VirtualAddress;
        u32 nlibrary, i,j;

        for (nlibrary=0u; nlibrary < excl_maximum
                          && image_import_descriptor->FirstThunk;
             ++image_import_descriptor)
        {
            HMODULE hmodule;
            if (dyndebug_runtime_link) {
                fprintf(dyndebug_output,"Now should know DLL: %s\n",
                        (char*)(base + image_import_descriptor->Name));
            }
            /* Code using image thunk data to get its handle was here, with a
             * number of platform-specific tricks (like using VirtualQuery for
             * old OSes lacking GetModuleHandleEx).
             *
             * It's now replaced with requesting handle by name, which is
             * theoretically unreliable (with SxS, multiple modules with same
             * name are quite possible), but good enough to find the
             * link-time dependencies of our executable or DLL. */

            hmodule = (HMODULE)
                GetModuleHandle(base + image_import_descriptor->Name);

            if (hmodule)
            {
                /* We may encouncer some module more than once while
                   traversing import descriptors (it's usually a
                   result of non-trivial linking process, like doing
                   ld -r on some groups of files before linking
                   everything together.

                   Anyway: using a module handle more than once will
                   do no harm, but it slows down the startup (even
                   now, our startup time is not a pleasant topic to
                   discuss when it comes to :sb-dynamic-core; there is
                   an obvious direction to go for speed, though --
                   instead of resolving symbols one-by-one, locate PE
                   export directories -- they are sorted by symbol
                   name -- and merge them, at one pass, with sorted
                   list of required symbols (the best time to sort the
                   latter list is during Genesis -- that's why I don't
                   proceed with memory copying, qsort() and merge
                   right here)). */

                for (j=0; j<nlibrary; ++j)
                {
                    if(check_duplicates[j] == hmodule)
                        break;
                }
                if (j<nlibrary) continue; /* duplicate => skip it in
                                           * outer loop */

                check_duplicates[nlibrary] = hmodule;
                if (opt_store_handles) {
                    opt_store_handles[nlibrary] = hmodule;
                }
                if (opt_store_names) {
                    opt_store_names[nlibrary] = (const char *)
                        (base + image_import_descriptor->Name);
                }
                if (dyndebug_runtime_link) {
                    fprintf(stderr,"DLL detection: %u, base %p: %s\n",
                            nlibrary, hmodule,
                            (char*)(base + image_import_descriptor->Name));
                }
                ++ nlibrary;
            }
        }
        return nlibrary;
    }
}

/* Have you seen a Greenspun's Tenth Rule in action when it manifests
 * itself in C code which is a part of a Lisp implementation? */

/* Object tagged? (dereference (cast (untag (obj)))) */

#define FOLLOW(obj,lowtagtype,ctype)            \
    (*(struct ctype*)(obj - lowtagtype##_LOWTAG))

/* For all types sharing OTHER_POINTER_LOWTAG: */

#define FOTHERPTR(obj,ctype)                    \
    FOLLOW(obj,OTHER_POINTER,ctype)

static inline lispobj car(lispobj conscell)
{
    return FOLLOW(conscell,LIST_POINTER,cons).car;
}

static inline lispobj cdr(lispobj conscell)
{
    return FOLLOW(conscell,LIST_POINTER,cons).cdr;
}

extern void undefined_alien_function(); /* see interrupt.c */

static u32 buildTimeImageCount = 0;
static void* buildTimeImages[16];

int (*msvcrt_resetstkoflw)() = NULL;

static int _dummyresetstkoflw()
{
    return 0;
}

int maybe_resetstkoflw()
{
    if (!msvcrt_resetstkoflw) {
        msvcrt_resetstkoflw =
            (int(*)())GetProcAddress(GetModuleHandleA("MSVCRT.DLL"),"_resetstkoflw");
        if (!msvcrt_resetstkoflw)
            msvcrt_resetstkoflw = _dummyresetstkoflw;
    }
    return msvcrt_resetstkoflw();
}
/* Resolve symbols against the executable and its build-time dependencies */
void* os_dlsym_default(char* name)
{
    unsigned int i;
    void* result = NULL;
    if (buildTimeImageCount == 0) {
        buildTimeImageCount =
            1 + os_get_build_time_shared_libraries(15u,
            NULL, 1+(void**)buildTimeImages, NULL);
    }
    for (i = 0; i<buildTimeImageCount && (!result); ++i) {
        result = GetProcAddress(buildTimeImages[i], name);
    }
    return result;
}

#endif



#if defined(LISP_FEATURE_SB_THREAD)
/* We want to get a slot in TIB that (1) is available at constant
   offset, (2) is our private property, so libraries wouldn't legally
   override it, (3) contains something predefined for threads created
   out of our sight.

   Low 64 TLS slots are adressable directly, starting with
   FS:[#xE10]. When SBCL runtime is initialized, some of the low slots
   may be already in use by its prerequisite DLLs, as DllMain()s and
   TLS callbacks have been called already. But slot 63 is unlikely to
   be reached at this point: one slot per DLL that needs it is the
   common practice, and many system DLLs use predefined TIB-based
   areas outside conventional TLS storage and don't need TLS slots.
   With our current dependencies, even slot 2 is observed to be free
   (as of WinXP and wine).

   Now we'll call TlsAlloc() repeatedly until slot 63 is officially
   assigned to us, then TlsFree() all other slots for normal use. TLS
   slot 63, alias FS:[#.(+ #xE10 (* 4 63))], now belongs to us.

   To summarize, let's list the assumptions we make:

   - TIB, which is FS segment base, contains first 64 TLS slots at the
   offset #xE10 (i.e. TIB layout compatibility);
   - TLS slots are allocated from lower to higher ones;
   - All libraries together with CRT startup have not requested 64
   slots yet.

   All these assumptions together don't seem to be less warranted than
   the availability of TIB arbitrary data slot for our use. There are
   some more reasons to prefer slot 63 over TIB arbitrary data: (1) if
   our assumptions for slot 63 are violated, it will be detected at
   startup instead of causing some system-specific unreproducible
   problems afterwards, depending on OS and loaded foreign libraries;
   (2) if getting slot 63 reliably with our current approach will
   become impossible for some future Windows version, we can add TLS
   callback directory to SBCL binary; main image TLS callback is
   started before _any_ TLS slot is allocated by libraries, and
   some C compiler vendors rely on this fact. */

void os_preinit()
{
#ifdef LISP_FEATURE_X86
    DWORD slots[TLS_MINIMUM_AVAILABLE];
    DWORD key;
    int n_slots = 0, i;
    for (i=0; i<TLS_MINIMUM_AVAILABLE; ++i) {
        key = TlsAlloc();
        if (key == OUR_TLS_INDEX) {
            if (TlsGetValue(key)!=NULL)
                lose("TLS slot assertion failed: fresh slot value is not NULL");
            TlsSetValue(OUR_TLS_INDEX, (void*)(intptr_t)0xFEEDBAC4);
            if ((intptr_t)(void*)arch_os_get_current_thread()!=(intptr_t)0xFEEDBAC4)
                lose("TLS slot assertion failed: TIB layout change detected");
            TlsSetValue(OUR_TLS_INDEX, NULL);
            break;
        }
        slots[n_slots++]=key;
    }
    for (i=0; i<n_slots; ++i) {
        TlsFree(slots[i]);
    }
    if (key!=OUR_TLS_INDEX) {
        lose("TLS slot assertion failed: slot 63 is unavailable "
             "(last TlsAlloc() returned %u)",key);
    }
#endif
}


/* A key for a companion Lisp fiber for foreign threads */
static pthread_key_t lisp_fiber_key;

/* A thread (assigned from the Lisp side) that creates fibers for
   foreign threads on demand */
static pthread_t fiber_factory_fiber = NULL;

/* As pthread_np_run_in_fiber doesn't wait for target fiber to be
   enterable, we need another lock to serialize factory clients.

   If a foreign callback is entered before fiber_factory_fiber is
   initialized [from the Lisp side, see fiber.lisp], it will wait on a
   condition until the factory becomes available.
*/
static pthread_mutex_t fiber_factory_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t fiber_factory_condition;

/* Lisp code creating companion Lisp fibers is wrapped into a callback
   and assigned to this variable: */
static void (*fiber_factory_callback) (void*) = NULL;

/* Interface procedure: announce fiber factory on initialization */
void fiber_announce_factory(pthread_t thread,
                            void(*callback)(void*))
{
    pthread_mutex_lock(&fiber_factory_lock);
    fiber_factory_callback = callback;
    fiber_factory_fiber = thread;
    pthread_cond_broadcast(&fiber_factory_condition);
    pthread_mutex_unlock(&fiber_factory_lock);
}

/* For Lisp fibers whose foreign threads are finished, we need another
   thread to unwind them out of Lisp world. This thread itself doesn't
   have to be a Lisp thread; however, it'd better be a native pthread,
   not a system foreign thread. Otherwise we have a chicken-and-egg
   problem.

   pthread destructors for foreign thread TLS data are currently run
   by a system-thread callback (see RegisterWaitForSingleObject).

   Creating anything like pthread structure or fiber around _that_
   thread is out of question. We'd have to account for _its_
   termination, and so on...

   Strange problems with raw (non-lisp) pthread waiting on a condition
   were encountered; let the fiber-destructor be a lisp thread for now.

   Signalling to Lisp with pthread conditions looks suspicios by now,
   too. Trying to go with CreateEvent (not naive, but simplified
   condition signalling).
*/


/* Well, let's try the same using non-lisp thread as a `fiber
 * cemetry', and IO completion port as a queue */

HANDLE dead_fiber_queue;
pthread_t fiber_funeral_service;

void* fiber_funeral_function(void*arg)
{
    HANDLE port = (HANDLE)arg;
    DWORD useless1;
    ULONG_PTR fiber_pthread;
    LPOVERLAPPED useless2;

    while(GetQueuedCompletionStatus(port,&useless1,&fiber_pthread,
                                    &useless2, INFINITE))
    {
        if (fiber_pthread)
            pthread_np_switch_to_fiber((pthread_t)fiber_pthread);
        else
            break;
    }
    CloseHandle(port);
    return NULL;
}

/* To be called from foreign thread TLS destructor */
static void fiber_is_dead(void* companion_pthread)
{
    AVER(PostQueuedCompletionStatus(dead_fiber_queue,0,
                                    (ULONG_PTR)companion_pthread,NULL));
}

void fiber_deinit_runtime()
{
    pthread_t factory;
    pthread_mutex_lock(&fiber_factory_lock);
    factory = fiber_factory_fiber;
    if (!factory) {
        pthread_mutex_unlock(&fiber_factory_lock);
        return;
    }
    fiber_factory_fiber = NULL;
    pthread_cond_signal(&fiber_factory_condition);
    pthread_mutex_unlock(&fiber_factory_lock);

    fiber_is_dead(factory);
    fiber_is_dead(NULL);
    pthread_join(fiber_funeral_service,NULL);
    /* Lisp side should join factory thread */
}

/* Structure used to pass callback arguments into a companion fiber */
typedef struct fff_call_info {
    lispobj args[3];
    int done;
} fff_call_info;

void fff_foreign_callback( void *v_info_ptr)
{
    fff_call_info *info_ptr = v_info_ptr;
    lispobj *args = info_ptr->args;
    struct thread* self = arch_os_get_current_thread();
    CONTEXT w32ctx;
    os_context_t cbctx;

    cbctx.win32_context = &w32ctx;
    self->gc_safepoint_context = &cbctx;
    *os_context_fp_addr(&cbctx) = *(((os_context_register_t*)args[2])-1);

    BEGIN_GC_UNSAFE_CODE;
    funcall3(SymbolValue(ENTER_ALIEN_CALLBACK,self),
             LISPOBJ(args[0]),LISPOBJ(args[1]),LISPOBJ(args[2]));
    END_GC_UNSAFE_CODE;

    info_ptr->done = 1;
    return;
}

void fff_generic_callback(lispobj arg0,lispobj arg1, lispobj arg2)
{
#ifdef LISP_FEATURE_X86_64
    struct thread* th = (pthread_np_notice_thread(),
                         arch_os_get_current_thread());
#else
    struct thread* th = arch_os_get_current_thread();
#endif
    pthread_t companion_fiber;
    CONTEXT w32ctx;
    os_context_t cbctx;

    if (th) {
        cbctx.win32_context = &w32ctx;
        th->gc_safepoint_context = &cbctx;
        *os_context_fp_addr(&cbctx) = *(((os_context_register_t*)arg2)-1);

        BEGIN_GC_UNSAFE_CODE;
        funcall3(SymbolValue(ENTER_ALIEN_CALLBACK,th),arg0,arg1,arg2);
        END_GC_UNSAFE_CODE;
    } else {
        /* It is a foreign thread */
        pthread_np_notice_thread();
        /* Now we have a pthread_self(), even if we didn't */
        companion_fiber = pthread_getspecific(lisp_fiber_key);
        if (!companion_fiber) {
            /* Create a new companion fiber */
            pthread_mutex_lock(&fiber_factory_lock);
            while (!fiber_factory_fiber) {
                /* No fiber factory (yet/already). Wait for something to
                   happen.

                   When dumping core, all ftc companion fibers will be
                   detached from their threads, and the condition will never
                   be signalled: reentered callbacks will hang forever.

                   Upon restart, when lisp fiber factory is not initialized,
                   foreign callbacks will wait for initialization.
                */
                pthread_cond_wait(&fiber_factory_condition,
                                  &fiber_factory_lock);
            }
            pthread_np_run_in_fiber(fiber_factory_fiber,
                                    fiber_factory_callback,
                                    &companion_fiber);
            pthread_mutex_unlock(&fiber_factory_lock);

            /* Save it for future callbacks */
            pthread_setspecific(lisp_fiber_key, companion_fiber);
        } else {
            if (!fiber_factory_fiber) {
                /* Though we have a companion fiber, foreign callback support
                   is deinitialized, and this fiber is unusable. */

                pthread_setspecific(lisp_fiber_key, NULL);
                /* Do something harmless: when SBCL is dumping or exiting,
                   foreign threads entering callbacks will sleep forever. */
                Sleep(INFINITE);
            }
        }
        /* Run this callback in foreign fiber */
        if (companion_fiber) {
            fff_call_info info = {{arg0, arg1, arg2}, 0};
            if (pthread_np_run_in_fiber(companion_fiber, fff_foreign_callback, &info)) {
                /* Failure to reenter -> shutdown */
                pthread_setspecific(lisp_fiber_key, NULL);
                /* Do something harmless: when SBCL is dumping or exiting,
                   foreign threads entering callbacks will sleep forever. */
                Sleep(INFINITE);
            }
            if (!info.done) {
                /* Stack unwind in a callback, or a failure to reenter. No
                   need to clean up when this thread exits any more. */
                pthread_setspecific(lisp_fiber_key, NULL);
                ExitThread(1);
            }
            /* Normal return */
            if (!fiber_factory_fiber) {
                /* Subsystem is deinitialized. In this case, foreign thread
                   itself destroys its companion fibers.

                   Lisp-level thread-join should be used to wait for active
                   callbacks. */
                pthread_setspecific(lisp_fiber_key, NULL);
                fiber_is_dead(companion_fiber);
            }
        } else {
            /* Companion fiber creation failure */
            ExitThread(1);
            /* RWSO callback in pthreads_win32 takes care on pthread-created
               fiber etc. */
        }
        /* Normal ftc exit */
    }
}


static pthread_key_t save_lisp_tls_key;

static void save_lisp_tls()
{
#ifdef LISP_FEATURE_X86
    pthread_setspecific(save_lisp_tls_key,TlsGetValue(OUR_TLS_INDEX));
#endif
}

static void restore_lisp_tls()
{
#ifdef LISP_FEATURE_X86
    struct thread *self = pthread_getspecific(save_lisp_tls_key);
    struct thread *prev = TlsGetValue(OUR_TLS_INDEX);
    TlsSetValue(OUR_TLS_INDEX, self);
#endif /* LISP_FEATURE_X86 */
}


#endif  /* LISP_FEATURE_SB_THREAD */


#ifdef LISP_FEATURE_X86_64
/* Windows has 32-bit 'longs', so printf...%lX (and other %l patterns) doesn't
 * work well with address-sized values, like it's done all over the place in
 * SBCL. And msvcrt uses I64, not LL, for printing long longs.
 *
 * I've already had enough search/replace with longs/words/intptr_t for today,
 * so I prefer to solve this problem with a format string translator. */

/* There is (will be) defines for printf and friends. */

static int translating_vfprintf(FILE*stream, const char *fmt, va_list args)
{
    char translated[1024];
    unsigned int i=0, delta = 0;

    while (fmt[i-delta] && i<sizeof(translated)-1) {
        if((fmt[i-delta]=='%')&&
           (fmt[i-delta+1]=='l')) {
            translated[i++]='%';
            translated[i++]='I';
            translated[i++]='6';
            translated[i++]='4';
            delta += 2;
        } else {
            translated[i]=fmt[i-delta];
            ++i;
        }
    }
    translated[i++]=0;
    return vfprintf(stream,translated,args);
}

int printf(const char*fmt,...)
{
    va_list args;
    va_start(args,fmt);
    return translating_vfprintf(stdout,fmt,args);
}
int fprintf(FILE*stream,const char*fmt,...)
{
    va_list args;
    va_start(args,fmt);
    return translating_vfprintf(stream,fmt,args);
}

#endif

#ifdef COMPOSITE_MAPPING

/* Our own `pagemaps' to track pages causing exceptions in mmapped
 * core.  Once PAGE_WRITECOPY is dirty, we (may) decide to unmap it,
 * allocate it in a normal way and get write watches in exchange.
 *
 * Unfortunately, one decision must be taken for the entire allocation
 * unit. The best thing to do may be as well leaving mmapped core in
 * its place, using real page protection and allowing SEH to be
 * called. That's what the code currently does without
 * COMPOSITE_MAPPING. Some real benchmarking is needed to tell what's
 * approach is better.
 */

char mmap_faulted[(DYNAMIC_SPACE_END-DYNAMIC_SPACE_START)/BACKEND_PAGE_BYTES];
char mmap_unshared[(DYNAMIC_SPACE_END-DYNAMIC_SPACE_START)/BACKEND_PAGE_BYTES];

#define MMAP_OFF(addr) ((u64)addr - DYNAMIC_SPACE_START)

#define MMAP_FAULTED_BIT(addr)                          \
    mmap_faulted[MMAP_OFF(addr)/os_vm_mmap_unit_size]

#define SET_MMAP_FAULTED_BIT(addr)                                      \
    do {                                                                \
        mmap_faulted[MMAP_OFF(addr)/os_vm_mmap_unit_size] = 1;          \
    } while(0)

#define MMAP_UNSHARED_BIT(addr)                         \
    mmap_unshared[MMAP_OFF(addr)/os_vm_mmap_unit_size]

#define SET_MMAP_UNSHARED_BIT(addr)                                     \
    do {                                                                \
        mmap_unshared[MMAP_OFF(addr)/os_vm_mmap_unit_size] = 1;         \
    } while(0)

#else
#define MMAP_UNSHARED_BIT(addr) 0
#define MMAP_FAULTED_BIT(addr) 0
#define SET_MMAP_FAULTED_BIT(addr) do {} while(0)
#define SET_MMAP_UNSHARED_BIT(addr) do {} while(0)

#endif

typeof(GetWriteWatch) *ptr_GetWriteWatch;
typeof(ResetWriteWatch) *ptr_ResetWriteWatch;
typeof(GetModuleHandleExA) *ptr_GetModuleHandleExA;

BOOL WINAPI CancelIoEx(HANDLE handle, LPOVERLAPPED overlapped);
typeof(CancelIoEx) *ptr_CancelIoEx;
BOOL WINAPI CancelSynchronousIo(HANDLE threadHandle);
typeof(CancelSynchronousIo) *ptr_CancelSynchronousIo;

#define RESOLVE(hmodule,fn)                     \
    do {                                        \
        ptr_##fn = (typeof(ptr_##fn))           \
            GetProcAddress(hmodule,#fn);        \
    } while (0)

static void resolve_optional_imports()
{
    HMODULE kernel32 = GetModuleHandleA("kernel32");
    if (kernel32) {
        RESOLVE(kernel32,GetWriteWatch);
        RESOLVE(kernel32,ResetWriteWatch);
        RESOLVE(kernel32,CancelIoEx);
        RESOLVE(kernel32,CancelSynchronousIo);
        RESOLVE(kernel32,GetModuleHandleExA);
    }
}

#undef RESOLVE

intptr_t win32_get_module_handle_by_address(os_vm_address_t addr)
{
    MEMORY_BASIC_INFORMATION moduleMapping;
    HMODULE result = 0;
    return
        ptr_GetModuleHandleExA ?
        (intptr_t)(ptr_GetModuleHandleExA(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
                                          GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                                          (LPCSTR)addr, &result) ? result : 0)
         :
        (VirtualQuery(addr, &moduleMapping, sizeof (moduleMapping)) ?
         (intptr_t)moduleMapping.AllocationBase : 0);
}

int os_number_of_processors = 1;
DWORD mwwFlag = 0u;

void os_init(char *argv[], char *envp[])
{
    SYSTEM_INFO system_info;
    _set_sbh_threshold(480);
    GetSystemInfo(&system_info);
    os_vm_page_size = system_info.dwPageSize > BACKEND_PAGE_BYTES?
        system_info.dwPageSize : BACKEND_PAGE_BYTES;
#if defined(LISP_FEATURE_X86)
    fast_bzero_pointer = fast_bzero_detect;
#endif
    os_number_of_processors = system_info.dwNumberOfProcessors;
    dyndebug_init();

    base_seh_frame = get_seh_frame();
#if defined(LISP_FEATURE_SB_THREAD)
    pthread_key_create(&save_lisp_tls_key,NULL);
    /* lisp_fiber_key with destructor.  After foreign thread
       exits, its companion Lisp fiber is enqueued for destruction
       too. */
    pthread_key_create(&lisp_fiber_key,fiber_is_dead);
    /* Now INVALID fpu trap should be enabled from the very start */
    pthread_save_context_hook = save_lisp_tls;
    pthread_restore_context_hook = restore_lisp_tls;
    pthread_cond_init(&fiber_factory_condition,NULL);
    pthread_mutex_init(&fiber_factory_lock,NULL);
    alloc_gc_page();
    {
        dead_fiber_queue = CreateIoCompletionPort(INVALID_HANDLE_VALUE,NULL,
                                                  0, 1);
        pthread_create(&fiber_funeral_service,NULL,
                       fiber_funeral_function,
                       (void*)dead_fiber_queue);
    }
#endif
    resolve_optional_imports();
    mwwFlag = (ptr_GetWriteWatch && ptr_ResetWriteWatch) ? MEM_WRITE_WATCH : 0u;
    runtime_module_handle = (HMODULE)win32_get_module_handle_by_address(&runtime_module_handle);
}

unsigned long core_mmap_unshared_pages = 0;

/* Various aspects of GetWriteWatch usage: */

/* if defined: reset write watch data on WP request (from GC);
 * otherwise: reset on os_commit_wp_violation_data() */
#define WRITE_WATCH_RESET_ON_WP


/* How many addresses request from each GetWriteWatch() [notice 4K
 * granularity].
 *
 * In WRITE_WATCH_RESET_ON_WP mode, it arbitrates between doing a lot
 * of syscalls and getting a lot of garbage: bulk size 1 will give one
 * request per WP page (not touching other page);
 *
 * In !defined(WRITE_WATCH_RESET_ON_WP) mode, the consequences may be
 * more significant -- for running non-GC code as well, not only for
 * GC performance. To be tested. */

#define WRITE_WATCH_BULK_SIZE 512

static boolean narrow_address_range_to_wp(LPVOID* startptr, SIZE_T* sizeptr)
{
    int index, wp_min=-1,wp_max=-1;
    for (index = find_page_index(*startptr); index <page_table_pages; ++index) {
        if (page_table[index].write_protected) {
            if (wp_min==-1)
                wp_min = index;
            wp_max = index;
        }
    }
    if (wp_min == -1) {
        return 0;
    }
    *startptr = page_address(wp_min);
    *sizeptr = page_address(wp_max) - page_address(wp_min) + BACKEND_PAGE_BYTES;
    return 1;
}

static os_vm_address_t commit_wp_violations(LPVOID* addrs, ULONG_PTR naddrs,
                                            ULONG_PTR* counter)
{
    ULONG_PTR i;
    os_vm_address_t nextaddr = 0;
    int index = -1;
    for (i=0;i<naddrs; ++i) {
        LPVOID fault_address = addrs[i];
        if (index == find_page_index(fault_address))
            continue;
        index = find_page_index(fault_address);
        if (page_table[index].write_protected) {
            gencgc_handle_wp_violation(fault_address);
            if (counter) ++ (*counter);
        }
        nextaddr = fault_address + os_vm_page_size;
    }
    return nextaddr;
}

void os_commit_wp_violation_data(boolean call_gc)
{
    LPVOID ww_start = core_mmap_end ? core_mmap_end : (LPVOID)DYNAMIC_SPACE_START;
    SIZE_T ww_size;

    /* static array is _allowed_, because this function is
     * called by GC with the world stopped.

     * It is _desirable_ because when it was local/auto, written-to
     * addresses ended up in the stack; when stack space was reused,
     * and some words were uninitialized, it could become a major
     * source of `fake pinning' (gencgc making page unmovable).
     *
     * NB gencgc page-pinning does something worse than just
     * (locally) preventing heap compaction; see code... */

    static LPVOID writeAddrs[WRITE_WATCH_BULK_SIZE];

#ifdef WRITE_WATCH_RESET_ON_WP
    if (!narrow_address_range_to_wp(&ww_start,&ww_size))
        return;
#endif

    if (mwwFlag) {
#ifdef WRITE_WATCH_RESET_ON_WP
        if (call_gc) {
            ULONG_PTR nAddrs;
            unsigned int total = 0;
            unsigned int i;
            const ULONG_PTR maxAddrs = sizeof(writeAddrs)/sizeof(writeAddrs[0]);
            ULONG granularity;
            ULONG_PTR gc_page_counter = 0;
            do {
                nAddrs = maxAddrs;
                AVERLAX(!ptr_GetWriteWatch(0,
                                  ww_start,ww_size,
                                          writeAddrs,&nAddrs,&granularity));
                ww_start = commit_wp_violations(writeAddrs,nAddrs,&gc_page_counter);
                total += nAddrs;
            } while(nAddrs == maxAddrs &&
                    narrow_address_range_to_wp(&ww_start,&ww_size));
            odxprint(pagefaults,"Write watch caught %u minipages"
                     " in %u macropages\n",total,gc_page_counter);
        }
#else
        if (call_gc) {
            ULONG_PTR nAddrs;
            unsigned int total = 0;
            unsigned int i;
            const ULONG_PTR maxAddrs = sizeof(writeAddrs)/sizeof(writeAddrs[0]);
            ULONG granularity;
            do {
                nAddrs = maxAddrs;
                ptr_GetWriteWatch(WRITE_WATCH_FLAG_RESET,
                                  ww_start,ww_size,
                                  writeAddrs,&nAddrs,&granularity);
                commit_wp_violations(writeAddrs,nAddrs,NULL);
                total += nAddrs;
            } while(nAddrs == maxAddrs);
            odxprint(pagefaults,"Write watch caught %u pages\n",total);
        } else {
            ptr_ResetWriteWatch(ww_start,ww_size);
        }
#endif

#ifdef COMPOSITE_MAPPING
        if (core_mmap_end && mwwFlag) {
            for (ww_start = (LPVOID)DYNAMIC_SPACE_START;
                 ww_start<core_mmap_end; ww_start += os_vm_mmap_unit_size) {
                if (MMAP_UNSHARED_BIT(ww_start)) {
                    if (call_gc) {
                        ULONG_PTR nAddrs;
                        unsigned int total = 0;
                        unsigned int i;
                        const ULONG_PTR maxAddrs = sizeof(writeAddrs)/
                            sizeof(writeAddrs[0]);
                        ULONG granularity;
                        do {
                            nAddrs = maxAddrs;
                            ptr_GetWriteWatch(WRITE_WATCH_FLAG_RESET,
                                              ww_start,os_vm_mmap_unit_size,
                                              writeAddrs,&nAddrs,&granularity);
                            for (i=0;i<nAddrs; ++i) {
                                LPVOID fault_address = writeAddrs[i];
                                int index = find_page_index(fault_address);
                                if (page_table[index].write_protected) {
                                    gencgc_handle_wp_violation(fault_address);
                                }
                            }
                            total += nAddrs;
                        } while(nAddrs == maxAddrs);
                        odxprint(pagefaults,"[MMCore] Write watch caught %u pages\n",total);
                    } else {
                        ptr_ResetWriteWatch(ww_start,os_vm_mmap_unit_size);
                    }
                } else {
                    if (MMAP_FAULTED_BIT(ww_start)) {
                        ++core_mmap_unshared_pages;
                        /* unshare? */
                        char copy[os_vm_mmap_unit_size];
                        memcpy(copy,ww_start,os_vm_mmap_unit_size);
                        UnmapViewOfFile(ww_start);
                        SET_MMAP_UNSHARED_BIT(ww_start);
                        os_validate(ww_start,os_vm_mmap_unit_size);
                        os_validate_recommit(ww_start,os_vm_mmap_unit_size);
                        memcpy(ww_start,copy,os_vm_mmap_unit_size);
                    }
                }
            }
        }
#endif
    }
}

static inline boolean local_thread_stack_address_p(os_vm_address_t address)
{
    return this_thread &&
        (((((u64)address >= (u64)this_thread->os_address) &&
           ((u64)address < ((u64)this_thread)))||
          (((u64)address >= (u64)this_thread->control_stack_start)&&
           ((u64)address < (u64)this_thread->control_stack_end))));
}



static DWORD os_protect_modes[8] = {
    PAGE_NOACCESS,
    PAGE_READONLY,
    PAGE_READWRITE,
    PAGE_READWRITE,
    PAGE_EXECUTE,
    PAGE_EXECUTE_READ,
    PAGE_EXECUTE_READWRITE,
    PAGE_EXECUTE_READWRITE,
};


static DWORD os_mmap_noexec_modes[8] = {
    PAGE_NOACCESS,
    PAGE_READONLY,
    PAGE_WRITECOPY,
    PAGE_WRITECOPY,
    PAGE_NOACCESS,
    PAGE_READONLY,
    PAGE_WRITECOPY,
    PAGE_WRITECOPY,
};

static DWORD os_mmap_exec_modes[8] = {
    PAGE_NOACCESS,
    PAGE_READONLY,
    PAGE_WRITECOPY,
    PAGE_WRITECOPY,
    PAGE_EXECUTE,
    PAGE_EXECUTE_READ,
    PAGE_EXECUTE_WRITECOPY,
    PAGE_EXECUTE_WRITECOPY,
};

static DWORD* os_mmap_protect_modes = os_protect_modes;

static inline boolean addr_in_mmapped_core(os_vm_address_t addr)
{
    return
        (core_mmap_end != NULL) &&
        (((u64)addr) >= (u64)DYNAMIC_SPACE_START) &&
        (((u64)addr) < (u64)core_mmap_end);
}

#define RECURSIVE_REDUCE_TO_ONE_SPACE_VOID(function,addr,len,...)       \
    do {                                                                \
        if (core_mmap_end) {                                            \
            if (((u64)core_mmap_end<(u64)(addr+len))&&                  \
                ((u64)core_mmap_end>(u64)(addr))) {                     \
                function(core_mmap_end,                                 \
                         addr+len-core_mmap_end                         \
                         __VA_ARGS__);                                  \
                len -= (addr+len-core_mmap_end);                        \
            };                                                          \
            if (addr_in_mmapped_core(addr+len)&&                        \
                !(addr_in_mmapped_core(addr))) {                        \
                function((os_vm_address_t)DYNAMIC_SPACE_START,          \
                         addr+len-((os_vm_address_t)DYNAMIC_SPACE_START \
                             ) __VA_ARGS__);                            \
                len = (os_vm_address_t)DYNAMIC_SPACE_START - addr;      \
            };                                                          \
        };                                                              \
    } while(0)                                                          \

#define RECURSIVE_REDUCE_TO_ONE_SPACE_ADDR(function,addr,len,...)       \
    do {                                                                \
        if (core_mmap_end) {                                            \
            if (((u64)core_mmap_end<(u64)(addr+len))&&                  \
                ((u64)core_mmap_end>(u64)(addr))) {                     \
                if(!function(core_mmap_end,addr+len-core_mmap_end       \
                             __VA_ARGS__))                              \
                    return NULL;                                        \
                len -= (addr+len-core_mmap_end);                        \
            };                                                          \
            if (addr_in_mmapped_core(addr+len)&&                        \
                !(addr_in_mmapped_core(addr))) {                        \
                if (!function((os_vm_address_t)DYNAMIC_SPACE_START,     \
                              addr+len-                                 \
                              ((os_vm_address_t)DYNAMIC_SPACE_START)    \
                              __VA_ARGS__))                             \
                    return NULL;                                        \
                len = (os_vm_address_t)DYNAMIC_SPACE_START - addr;      \
            };                                                          \
        }                                                               \
    } while(0)                                                          \

/*
 * So we have three fun scenarios here.
 *
 * First, we could be being called to reserve the memory areas
 * during initialization (prior to loading the core file).
 *
 * Second, we could be being called by the GC to commit a page
 * that has just been decommitted (for easy zero-fill).
 *
 * Third, we could be being called by create_thread_struct()
 * in order to create the sundry and various stacks.
 *
 * The third case is easy to pick out because it passes an
 * addr of 0.
 *
 * The second case is easy to pick out because it will be for
 * a range of memory that is MEM_RESERVE rather than MEM_FREE.
 *
 * The second case is also an easy implement, because we leave
 * the memory as reserved (since we do lazy commits).
 */

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    MEMORY_BASIC_INFORMATION mem_info;
    DWORD memWatch = mwwFlag;

    /* align len to page boundary for any operation (especially
     * important as we're going to experiment with larger "pages" than
     * the OS-supplied minimum) */

    if (!addr) {
        /* the simple case first */
        return
            AVERLAX(VirtualAlloc(addr, len, MEM_RESERVE|MEM_COMMIT, PAGE_EXECUTE_READWRITE));
    }

    RECURSIVE_REDUCE_TO_ONE_SPACE_ADDR(os_validate,addr,len);

    if (addr_in_mmapped_core(addr)) {
#ifndef COMPOSITE_MAPPING
        return addr;
#else
        if (len > os_vm_mmap_unit_size)
            return addr;
        if (!MMAP_UNSHARED_BIT(addr)) {
            SET_MMAP_FAULTED_BIT(addr);
            return addr;
        }
#endif
    }
    if (!AVERLAX(VirtualQuery(addr, &mem_info, sizeof mem_info)))
        return 0;

    if ((mem_info.State == MEM_RESERVE) && (mem_info.RegionSize >=len)) {
      /* It would be correct to return here. However, support for Wine
       * is beneficial, and Wine has a strange behavior in this
       * department. It reports all memory below KERNEL32.DLL as
       * reserved, but disallows MEM_COMMIT.
       *
       * Let's work around it: reserve the region we need for a second
       * time. The second reservation is documented to fail on normal NT
       * family, but it will succeed on Wine if this region is
       * actually free.
       */
      VirtualAlloc(addr, len, MEM_RESERVE|mwwFlag, PAGE_EXECUTE_READWRITE);
      /* If it is wine, the second call has succeded, and now the region
       * is really reserved. */
      return addr;
    }

    if (mem_info.State == MEM_RESERVE) {
        fprintf(stderr, "validation of reserved space too short.\n");
        fflush(stderr);
    }

    if (!(((u64)addr>=DYNAMIC_SPACE_START) && ((u64)addr<DYNAMIC_SPACE_END)))
        memWatch = 0u;


    if(!AVERLAX(VirtualAlloc(addr, len, (mem_info.State == MEM_RESERVE)?
                             MEM_COMMIT: MEM_RESERVE|memWatch, PAGE_EXECUTE_READWRITE)
                /* Win2k may pretend to have write watch support, but
                 * fail here.  We revert to `no write watch' mode
                 * (globally) as soon as possible. */
                ||(memWatch=0,mwwFlag=0,VirtualAlloc(addr, len,
                                                     (mem_info.State == MEM_RESERVE)?
                                                     MEM_COMMIT: MEM_RESERVE,
                                                     PAGE_EXECUTE_READWRITE))))
        return 0;

    return addr;
}

os_vm_address_t
os_validate_recommit(os_vm_address_t addr, os_vm_size_t len)
{
    RECURSIVE_REDUCE_TO_ONE_SPACE_ADDR(os_validate_recommit,addr,len);
    if (addr_in_mmapped_core(addr)) {
        return addr;
    }
    return
        AVERLAX(VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE));
}

os_vm_address_t
os_allocate_lazily(os_vm_size_t len)
{
    /* align len to page boundary for any operation */
    /* len = ALIGN_UP(len,os_vm_page_size); */
    return
        AVERLAX(VirtualAlloc(NULL, len, MEM_RESERVE, PAGE_EXECUTE_READWRITE));
}

/*
 * For os_invalidate(), we merely decommit the memory rather than
 * freeing the address space. This loses when freeing per-thread
 * data and related memory since it leaks address space. It's not
 * too lossy, however, since the two scenarios I'm aware of are
 * fd-stream buffers, which are pooled rather than torched, and
 * thread information, which I hope to pool (since windows creates
 * threads at its own whim, and we probably want to be able to
 * have them callback without funky magic on the part of the user,
 * and full-on thread allocation is fairly heavyweight). Someone
 * will probably shoot me down on this with some pithy comment on
 * the use of (setf symbol-value) on a special variable. I'm happy
 * for them.
 */

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    RECURSIVE_REDUCE_TO_ONE_SPACE_VOID(os_invalidate,addr,len);

    if (addr_in_mmapped_core(addr)) {
        fast_bzero(addr, len);
    } else {
        AVERLAX(VirtualFree(addr, len, MEM_DECOMMIT));
    }
}

void
os_invalidate_free(os_vm_address_t addr, os_vm_size_t len)
{
    AVERLAX(VirtualFree(addr, 0, MEM_RELEASE));
}

void
os_invalidate_free_by_any_address(os_vm_address_t addr, os_vm_size_t len)
{
    MEMORY_BASIC_INFORMATION minfo;
    AVERLAX(VirtualQuery(addr, &minfo, sizeof minfo));
    AVERLAX(minfo.AllocationBase);
    AVERLAX(VirtualFree(minfo.AllocationBase, 0, MEM_RELEASE));
}

static int os_supports_executable_mapping = 0;
#ifndef FILE_MAP_EXECUTE
#define FILE_MAP_EXECUTE 0x20
#endif

static char* non_external_self_name = "//////<SBCL executable>";

WCHAR override_runtime_ucs2_name[32768];
size_t override_runtime_ucs2_bytes = sizeof(override_runtime_ucs2_name);

#ifdef LISP_FEATURE_FDS_ARE_WINDOWS_HANDLES
#define maybe_open_osfhandle(handle,mode) (handle)
#define maybe_get_osfhandle(fd) (fd)
#define FDTYPE HANDLE
#else
#define maybe_open_osfhandle _open_osfhandle
#define maybe_get_osfhandle _get_osfhandle
#define FDTYPE int
#endif

int win32_open_for_mmap(const char* fileName)
{
    HANDLE handle;
    int retries = 1;
    do {
        if (strcmp(fileName,non_external_self_name)) {
            handle = CreateFileA(fileName,FILE_GENERIC_READ|FILE_GENERIC_EXECUTE,
                                 FILE_SHARE_READ,NULL,
                                 OPEN_EXISTING,0,NULL);
        } else {
            WCHAR mywpath[MAX_PATH+1];
            BOOL override = !!override_runtime_ucs2_name[0];
            LPWSTR wpath = override? override_runtime_ucs2_name : mywpath;
            DWORD gmfnResult = override ? 1 : GetModuleFileNameW(NULL,mywpath,MAX_PATH+1);
            AVER(gmfnResult>0 && gmfnResult<(MAX_PATH+1));
            handle = CreateFileW(wpath,FILE_GENERIC_READ|FILE_GENERIC_EXECUTE,
                                 FILE_SHARE_READ,NULL,
                                 OPEN_EXISTING,0,NULL);

        }
    } while (handle == INVALID_HANDLE_VALUE && retries--);
    AVER(handle && (handle!=INVALID_HANDLE_VALUE));
    /* Beware: runtime initialization in C still uses lowio even when
     * fds are handles, so _open_osfhandle below should be
     * retained (until runtime is fixed too). */
    return _open_osfhandle((intptr_t)handle,O_BINARY);
}


FILE* win32_fopen_runtime()
{
    int fd = win32_open_for_mmap(non_external_self_name);
    CRT_AVER_NONNEGATIVE(fd);
    return _fdopen(fd,"rb");
}
/*
 * os_map() is called to map a chunk of the core file into memory.
 *
 * Unfortunately, Windows semantics completely screws this up, so
 * we just add backing store from the swapfile to where the chunk
 * goes and read it up like a normal file. We could consider using
 * a lazy read (demand page) setup, but that would mean keeping an
 * open file pointer for the core indefinately (and be one more
 * thing to maintain).
 */

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    os_vm_size_t count, suboffset;

    if (addr == (os_vm_address_t)DYNAMIC_SPACE_START)
    {
        if (IS_ALIGNED(offset,os_vm_mmap_unit_size)) {
            len = ALIGN_UP(len,os_vm_mmap_unit_size);
            /* Beware as well (see comment on _open_osfhandle above). */
            HANDLE mapping =
                CreateFileMapping((HANDLE)_get_osfhandle(fd),NULL,
                                  PAGE_EXECUTE_WRITECOPY, 0, offset+len,
                                  NULL);
            if (!mapping) {
#ifdef LISP_FEATURE_X86_64
                /* Dirty trick (FIXME if you know how): WinXP/x64 apparently
                 * doesn't support executable-copy-on-write, at least for
                 * private pages, or needs some special workaround which I don't
                 * know (?). It also rejects PAGE_EXECUTE_WRITECOPY, and this
                 * fact is used here to skip core-mmapping on such systems
                 * altogether. */
                goto use_read;
#else
                mapping =
                    CreateFileMapping((HANDLE)_get_osfhandle(fd),NULL,
                                      PAGE_READONLY, 0, offset+len,
                                      NULL);
                if (mapping)
                    os_supports_executable_mapping = 0;
#endif
            } else {
                os_supports_executable_mapping = 1;
            }
            if (mapping) {
                /* Was validate()d, that's why we can't mmap with copy-on-write.
                   Well, let's turn it back... */
                AVERLAX(VirtualFree(addr, 0, MEM_RELEASE));
#ifdef COMPOSITE_MAPPING
                for (suboffset = 0; suboffset < len; suboffset += os_vm_mmap_unit_size) {
                    odxprint(io,"MVOF: %p %p %p %p\n",mapping,
                             offset + suboffset,
                             os_vm_mmap_unit_size,
                             ((void*)addr) + suboffset);
                    AVER(MapViewOfFileEx(mapping, FILE_MAP_COPY
                                         |(os_supports_executable_mapping?
                                           FILE_MAP_EXECUTE: 0), 0,
                                         offset + suboffset,
                                         os_vm_mmap_unit_size,
                                         ((void*)addr) + suboffset));
                }
#else
                AVER(MapViewOfFileEx(mapping, FILE_MAP_COPY
                                     |(os_supports_executable_mapping?
                                       FILE_MAP_EXECUTE: 0), 0,
                                     offset, len, addr));
#endif
                /* Reserve the rest of dynamic space... */
                os_validate(addr+len, dynamic_space_size-len);
                core_mmap_end = addr + len;
                os_mmap_protect_modes = os_supports_executable_mapping ?
                    os_mmap_exec_modes : os_mmap_noexec_modes;
                return addr;
            }
        }
    }
use_read:
    AVER(VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE)||
         VirtualAlloc(addr, len, MEM_RESERVE|MEM_COMMIT|mwwFlag,
                      PAGE_EXECUTE_READWRITE));

    CRT_AVER_NONNEGATIVE(lseek(fd, offset, SEEK_SET));

    count = _read(fd, addr, len);
    CRT_AVER( count == len );
    return addr;
}



/* FIXME: Now that FOO_END, rather than FOO_SIZE, is the fundamental
 * description of a space, we could probably punt this and just do
 * (FOO_START <= x && x < FOO_END) everywhere it's called. */
static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*)((uword_t)sbeg);
    char* end = (char*)((uword_t)sbeg) + slen;
    char* adr = (char*)a;
    return (adr >= beg && adr < end);
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    DWORD old_prot;

    RECURSIVE_REDUCE_TO_ONE_SPACE_VOID(os_protect,address,length,,prot);

    if (addr_in_mmapped_core(address)) {
#ifdef COMPOSITE_MAPPING
        while (length) {
            os_vm_size_t chunk = PTR_ALIGN_UP(address+1,os_vm_mmap_unit_size) - address;
            if (chunk > length) chunk = length;
            AVER(VirtualProtect(address, chunk, os_mmap_protect_modes[prot],
                                &old_prot));
            length -= chunk;
            address += chunk;
        }
#else
        AVER(VirtualProtect(address, length, os_mmap_protect_modes[prot],
                            &old_prot));
#endif
    } else {
        if (mwwFlag && in_range_p(address, DYNAMIC_SPACE_START, dynamic_space_size)) {
#ifdef WRITE_WATCH_RESET_ON_WP
            ptr_ResetWriteWatch(address,length);
#endif
            return;
        } else {
            DWORD new_prot = os_protect_modes[prot];
            AVER(VirtualProtect(address, length, new_prot, &old_prot)||
                 (VirtualAlloc(address, length, MEM_COMMIT, new_prot) &&
                  VirtualProtect(address, length, new_prot, &old_prot)));
            odxprint(misc,"Protecting %p + %p vmaccess %d "
                     "newprot %08x oldprot %08x",
                     address,length,prot,new_prot,old_prot
                );
        }
    }
}

os_vm_prot_t os_current_protection(os_vm_address_t address)
{
    MEMORY_BASIC_INFORMATION minfo;
    size_t prot;
    AVERLAX(VirtualQuery(address, &minfo, sizeof minfo));
    for (prot = 0; prot<(sizeof(os_protect_modes)/sizeof(os_protect_modes[0])); ++prot) {
        if (os_protect_modes[prot] == minfo.Protect)
            return prot;
    }
    return OS_VM_PROT_NONE;
}

boolean
is_linkage_table_addr(os_vm_address_t addr)
{
    return in_range_p(addr, LINKAGE_TABLE_SPACE_START, LINKAGE_TABLE_SPACE_END);
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    if(in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
       in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
       in_range_p(addr, DYNAMIC_SPACE_START  , dynamic_space_size) ||
       is_some_thread_local_addr(addr))
        return 1;
    return 0;
}


/* A tiny bit of interrupt.c state we want our paws on. */
extern boolean internal_errors_enabled;

#ifdef LISP_FEATURE_UD2_BREAKPOINTS
#define IS_TRAP_EXCEPTION(exception_record, context)                    \
     (((exception_record)->ExceptionCode == EXCEPTION_ILLEGAL_INSTRUCTION) && \
      (((unsigned short *)((context.win32_context)->Eip))[0] == 0x0b0f))
#define TRAP_CODE_WIDTH 2
#else
#define IS_TRAP_EXCEPTION(exception_record, context)            \
    ((exception_record)->ExceptionCode == EXCEPTION_BREAKPOINT)

#define TRAP_CODE_WIDTH 1
#endif
extern void exception_handler_wrapper();

void c_level_backtrace(const char* header, int depth)
{
#ifndef LISP_FEATURE_X86_64
    void* frame;
    int n = 0;
    void** lastseh;

    for (lastseh = get_seh_frame(); lastseh && (lastseh!=(void*)-1);
         lastseh = *lastseh);

    fprintf(dyndebug_output, "Backtrace: %s (thread %p)\n", header, this_thread);
    for (frame = __builtin_frame_address(0); frame; frame=*(void**)frame)
    {
        if ((n++)>depth)
            return;
        fprintf(dyndebug_output, "[#%02d]: ebp = 0x%p, ret = 0x%p\n",n,
                frame, ((void**)frame)[1]);
    }
#endif
}

#ifdef LISP_FEATURE_X86
#define voidreg(ctxptr,name) ((void*)((ctxptr)->E##name))
#else
#define voidreg(ctxptr,name) ((void*)((ctxptr)->R##name))
#endif

/*
 * We return ExceptionContinueSearch for memory fault errors by default, so any
 * handler established by foreign libraries has a chance to handle the
 * exception. Old behavior of letting memory faults to Lisp-side handler can
 * interfere with non-Lisp GCs and similar software; however, it may still be
 * preferred during development, so let's provide a variable to control this.
 */
int sbcl_enable_lisp_pagefault_handler = 0;

/*
 * A good explanation of the exception handling semantics is
 * http://win32assembly.online.fr/Exceptionhandling.html .
 */

EXCEPTION_DISPOSITION
handle_exception(EXCEPTION_RECORD *exception_record,
                 struct lisp_exception_frame *exception_frame,
                 CONTEXT *context,
                 void *dispatcher_context)
{
    if (!context) return ExceptionContinueSearch;
    if (exception_record->ExceptionFlags & (EH_UNWINDING|EH_EXIT_UNWIND))
        return  ExceptionContinueSearch;
    DWORD lastError = GetLastError();
    DWORD lastErrno = errno;
    DWORD code = exception_record->ExceptionCode;
    EXCEPTION_DISPOSITION disposition = ExceptionContinueExecution;
    void* fault_address = (void*)exception_record->ExceptionInformation[1];
    struct thread* self = arch_os_get_current_thread();
    os_context_t ctx, *oldctx = NULL;
    if (self) {
        oldctx = self ? self->gc_safepoint_context : 0;
        self->gc_safepoint_context = &ctx;
    }
#define myreg(xx) (voidreg(context,xx))
    odxprint(seh,
             "SEH: rec %p, ctxptr %p, rip %p, fault %p\n"
             "... code %p, rcx %p, fp-tags %p\n\n",
             exception_record,
             context,
             myreg(ip),
             fault_address,
             (void*)(intptr_t)code,
             myreg(cx),
             context->FloatSave.TagWord);

    ctx.win32_context = context;
    ctx.sigmask = self ? self->os_thread->blocked_signal_set : 0;

    if (code == EXCEPTION_STACK_OVERFLOW && self) {
        self->control_stack_guard_page_protected = NIL;
        BEGIN_GC_UNSAFE_CODE;
        funcall0(StaticSymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR));
        END_GC_UNSAFE_CODE;
        goto finish;
    }
    if (code == EXCEPTION_SINGLE_STEP) {
        /* We are doing a displaced instruction. At least function
         * end breakpoints uses this. */
        if (single_stepping) {
            restore_breakpoint_from_single_step(&ctx);
        }
        goto finish;
    }

    if (IS_TRAP_EXCEPTION(exception_record, ctx)) {
        unsigned trap;

        /* Unlike some other operating systems, Win32 leaves EIP
         * pointing to the breakpoint instruction. */

        if (*(unsigned char*)(*os_context_pc_addr(&ctx))==0xCC)
            (*os_context_pc_addr(&ctx)) += TRAP_CODE_WIDTH;

        /* Now EIP points just after the INT3 byte and aims at the
         * 'kind' value (eg trap_Cerror). */
        trap = *(unsigned char *)(*os_context_pc_addr(&ctx));
        /* Before any other trap handler: gc_safepoint ensures that
           inner alloc_sap for passing the context won't trap on
           pseudo-atomic. */
        if (trap == trap_PendingInterrupt) {
            /* Done everything needed for this trap, except EIP
               adjustment */
            arch_skip_instruction(&ctx);
            thread_interrupted(&ctx);
            goto finish;
        }
        /* This is just for info in case the monitor wants to print an
         * approximation. */
        access_control_stack_pointer(self) =
            (lispobj *)*os_context_sp_addr(&ctx);

        BEGIN_GC_UNSAFE_CODE;
        block_blockable_signals(0,&ctx.sigmask);
        handle_trap(&ctx, trap);
        thread_sigmask(SIG_SETMASK,&ctx.sigmask,NULL);
        END_GC_UNSAFE_CODE;

        goto finish;
    }
    else if (exception_record->ExceptionCode == STATUS_GUARD_PAGE_VIOLATION) {
        BEGIN_GC_UNSAFE_CODE;
        if (handle_guard_page_triggered(&ctx, fault_address))
            goto finish;
        END_GC_UNSAFE_CODE;
        disposition = ExceptionContinueSearch;
        goto finish;
    }
    else if (exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        odxprint(pagefaults,
                     "SEGV. ThSap %p, Eip %p, Esp %p, Esi %p, Edi %p, "
                     "Addr %p Access %d\n",
                     self,
                     voidreg(context,ip),
                     voidreg(context,sp),
                     voidreg(context,si),
                     voidreg(context,di),
                     fault_address,
                     exception_record->ExceptionInformation[0]);

        os_vm_size_t commit_size = os_vm_page_size;
        if (self) {
            if (local_thread_stack_address_p(fault_address)) {
                if (handle_guard_page_triggered(&ctx, fault_address)) {
                    goto finish; /* gc safety? */
                } else {
                    /* Vop for alien stack allocation touches the very first
                     * word of the new region to force commit.  We detect it
                     * here and commit the whole active alien stack instead
                     * of just a page. */
                    if (fault_address == self->alien_stack_pointer) {
                        /* FIXME: no "official" way to get THREAD_STRUCT_SIZE or
                         * alien stack end */
                        MEMORY_BASIC_INFORMATION minfo;
                        AVERLAX(VirtualQuery(fault_address, &minfo, sizeof minfo));
                        commit_size =
                            minfo.RegionSize -
                            (fault_address - minfo.BaseAddress);
                    }
                    goto try_recommit;
                }
            }

            if (dyndebug_lowpagefault_log && (((lispobj)fault_address)<0xFFFF)) {
                fprintf(stderr,
                        "Low page access (?) thread %p\n"
                        "(addr 0x%p, EIP 0x%p ESP 0x%p EBP 0x%p)\n",
                        self,
                        fault_address,
                        voidreg(context,ip),
                        voidreg(context,sp),
                        voidreg(context,bp));
                if (dyndebug_lowpagefault_halt) {
                    Sleep(INFINITE);
                    ExitProcess(0);
                } else {
                    goto complain;
                }
            }
        }
        if (fault_address == GC_SAFEPOINT_PAGE_ADDR) {
            /* Pick off GC-related memory fault next. */
            thread_in_lisp_raised(&ctx);
            goto finish;
        }
        if (self && (((u64)fault_address) == ((u64)self->csp_around_foreign_call))) {
            thread_in_safety_transition(&ctx);
            goto finish;
        }

        int index = find_page_index(fault_address);
        if (index != -1) {
            if (addr_in_mmapped_core(fault_address) && mwwFlag &&
                !MMAP_UNSHARED_BIT(fault_address)) {
                SET_MMAP_FAULTED_BIT(fault_address);
            }
            if (page_table[index].write_protected) {
                gencgc_handle_wp_violation(fault_address);
            } else {
                if (!addr_in_mmapped_core(fault_address)) {
                    AVER(VirtualAlloc(PTR_ALIGN_DOWN(fault_address,os_vm_page_size),
                                      os_vm_page_size,
                                      MEM_COMMIT, PAGE_EXECUTE_READWRITE));
                }
            }
            goto finish;
        }
        if (fault_address == undefined_alien_address)
            goto complain;
        if (!is_valid_lisp_addr(fault_address)) {
            if (sbcl_enable_lisp_pagefault_handler && internal_errors_enabled)
                goto complain;
            disposition = ExceptionContinueSearch;
            goto finish;
        }

    try_recommit:
        AVER(VirtualAlloc(PTR_ALIGN_DOWN(fault_address,os_vm_page_size),
                          commit_size,
                          MEM_COMMIT, PAGE_EXECUTE_READWRITE));
        goto finish;
    }
complain:
    /* All else failed, drop through to the lisp-side exception handler. */

    /*
     * If we fall through to here then we need to either forward
     * the exception to the lisp-side exception handler if it's
     * set up, or drop to LDB.
     */

    if (internal_errors_enabled) {
        struct gcing_safety safety;
        lispobj context_sap;
        lispobj exception_record_sap;

        asm("fnclex");
        /* We're making the somewhat arbitrary decision that having
         * internal errors enabled means that lisp has sufficient
         * marbles to be able to handle exceptions, but exceptions
         * aren't supposed to happen during cold init or reinit
         * anyway. */

        block_blockable_signals(0,&ctx.sigmask);
        fake_foreign_function_call(&ctx);
        BEGIN_GC_UNSAFE_CODE;

        /* Allocate the SAP objects while the "interrupts" are still
         * disabled. */
        context_sap = alloc_sap(&ctx);
        exception_record_sap = alloc_sap(exception_record);
        thread_sigmask(SIG_SETMASK, &ctx.sigmask, NULL);

        /* The exception system doesn't automatically clear pending
         * exceptions, so we lose as soon as we execute any FP
         * instruction unless we do this first. */
        /* Call into lisp to handle things. */
        funcall2(StaticSymbolFunction(HANDLE_WIN32_EXCEPTION), context_sap,
                 exception_record_sap);

        /* If Lisp doesn't nlx, we need to put things back. */
        END_GC_UNSAFE_CODE;
        undo_fake_foreign_function_call(&ctx);
        thread_sigmask(SIG_SETMASK, &ctx.sigmask, NULL);
        /* FIXME: HANDLE-WIN32-EXCEPTION should be allowed to decline */
        goto finish;
    }

    fprintf(stderr, "Exception Code: 0x%p.\n",
            (void*)(intptr_t)exception_record->ExceptionCode);
    fprintf(stderr, "Faulting IP: 0x%p.\n",
            (void*)(intptr_t)exception_record->ExceptionAddress);
    if (exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
        MEMORY_BASIC_INFORMATION mem_info;

        if (VirtualQuery(fault_address, &mem_info, sizeof mem_info)) {
            fprintf(stderr, "page status: 0x%lx.\n", mem_info.State);
        }

        fprintf(stderr, "Was writing: %p, where: 0x%p.\n",
                (void*)exception_record->ExceptionInformation[0],
                fault_address);
    }

    fflush(stderr);

    fake_foreign_function_call(&ctx);
    lose("Exception too early in cold init, cannot continue.");

    /* FIXME: WTF? How are we supposed to end up here? */
#if defined(LISP_FEATURE_SB_THREAD)
    pthread_sigmask(SIG_SETMASK, &ctx.sigmask, NULL);
#endif

    /* Common return point. */
finish:
    if (self) {
        self->gc_safepoint_context = oldctx;
    }
    errno = lastErrno;
    SetLastError(lastError);
    return disposition;
}

os_context_register_t carry_frame_pointer(os_context_register_t default_value)
{
    struct thread* self = arch_os_get_current_thread();
    return
        (self->gc_safepoint_context) ?
        (os_context_register_t)
        voidreg(self->gc_safepoint_context->win32_context,bp)
        : default_value;
}

#ifdef LISP_FEATURE_X86_64

LONG veh(EXCEPTION_POINTERS *ep)
{
    EXCEPTION_DISPOSITION disp;
    PUSH_ERRNO;

    if (!pthread_self())
        return EXCEPTION_CONTINUE_SEARCH;
    POP_ERRNO;

    disp = handle_exception(ep->ExceptionRecord,0,ep->ContextRecord,0);

    switch (disp)
    {
    case ExceptionContinueExecution:
        return EXCEPTION_CONTINUE_EXECUTION;
    case ExceptionContinueSearch:
        return EXCEPTION_CONTINUE_SEARCH;
    default:
        fprintf(stderr,"Exception handler is mad\n");
        ExitProcess(0);
    }
}
#endif

void
wos_install_interrupt_handlers(struct lisp_exception_frame *handler)
{
#ifdef LISP_FEATURE_X86
    handler->next_frame = get_seh_frame();
    handler->handler = (void*)exception_handler_wrapper;
    set_seh_frame(handler);
#else
    static int once = 0;
    if (!once++)
        AddVectoredExceptionHandler(1,veh);
#endif
}

char *dirname(char *path)
{
    static char buf[PATH_MAX + 1];
    size_t pathlen = strlen(path);
    int i;

    if (pathlen >= sizeof(buf)) {
        lose("Pathname too long in dirname.\n");
        return NULL;
    }

    strcpy(buf, path);
    for (i = pathlen; i >= 0; --i) {
        if (buf[i] == '/' || buf[i] == '\\') {
            buf[i] = '\0';
            break;
        }
    }

    return buf;
}

char *
os_get_runtime_executable_path(int external)
{
    if (external) {
        char path[MAX_PATH + 1];
        DWORD bufsize = sizeof(path);
        DWORD size;

        if ((size = GetModuleFileNameA(NULL, path, bufsize)) == 0)
            return NULL;
        else if (size == bufsize && GetLastError() == ERROR_INSUFFICIENT_BUFFER)
            return NULL;

        return copied_string(path);
    } else {
        return copied_string(non_external_self_name);
    }
}

// 0 - not a socket or other error, 1 - has input, 2 - has no input
int
socket_input_available(HANDLE socket)
{
    unsigned long count = 0, count_size = 0;
    int wsaErrno = GetLastError();
    int err = WSAIoctl((SOCKET)socket, FIONREAD, NULL, 0,
                       &count, sizeof(count), &count_size, NULL, NULL);

    int ret;

    if (err == 0) {
        ret = (count > 0) ? 1 : 2;
    } else
        ret = 0;
    SetLastError(wsaErrno);
    return ret;
}

/* Unofficial but widely used property of console handles: they have
   #b11 in two minor bits, opposed to other handles, that are
   machine-word-aligned. Properly emulated even on wine.

   Console handles are special in many aspects, e.g. they aren't NTDLL
   system handles: kernel32 redirects console operations to CSRSS
   requests. Using the hack below to distinguish console handles is
   justified, as it's the only method that won't hang during
   outstanding reads, won't try to lock NT kernel object (if there is
   one; console isn't), etc. */
int console_handle_p(HANDLE handle)
{
    return (handle != NULL)&&
        (handle != INVALID_HANDLE_VALUE)&&
        ((((int)(intptr_t)handle)&3)==3);
}

/* Atomically mark current thread as (probably) doing synchronous I/O
 * on handle, if no cancellation is requested yet (and return TRUE),
 * otherwise clear thread's I/O cancellation flag and return false.
 */
static boolean io_begin_interruptible(HANDLE handle)
{
    /* No point in doing it unless OS supports cancellation from other
     * threads */
    if (!ptr_CancelIoEx)
        return 1;

    if (!__sync_bool_compare_and_swap(&this_thread->synchronous_io_handle_and_flag,
                                      0, handle)) {
        ResetEvent(this_thread->private_events.events[0]);
        this_thread->synchronous_io_handle_and_flag = 0;
        return 0;
    } else {
        return 1;
    }
}

/* Unmark current thread as (probably) doing synchronous I/O; if an
 * I/O cancellation was requested, postpone it until next
 * io_begin_interruptible */
static void io_end_interruptible(HANDLE handle)
{
    if (!ptr_CancelIoEx)
        return;
    __sync_bool_compare_and_swap(&this_thread->synchronous_io_handle_and_flag,
                                 handle, 0);
}
/* Documented limit for ReadConsole/WriteConsole is 64K bytes.
   Real limit observed on W2K-SP3 is somewhere in between 32KiB and 64Kib...
*/
#define MAX_CONSOLE_TCHARS 16384

int win32_write_unicode_console(HANDLE handle, void * buf, int count)
{
    DWORD written = 0;
    DWORD nchars;
    BOOL result;
    nchars = count>>1;
    if (nchars>MAX_CONSOLE_TCHARS) nchars = MAX_CONSOLE_TCHARS;

    if (!io_begin_interruptible(handle)) {
        errno = EINTR;
        return -1;
    }
    result = WriteConsoleW(handle,buf,count>>1,&written,NULL);
    io_end_interruptible(handle);

    if (result) {
        if (!written) {
            errno = EINTR;
            return -1;
        } else {
            return 2*written;
        }
    } else {
        DWORD err = GetLastError();
        odxprint(io,"WriteConsole fails => %u\n", err);
        errno = (err==ERROR_OPERATION_ABORTED ? EINTR : EIO);
        return -1;
    }
}

#define THREADED_CONSOLE_INPUT

/* It may be unobvious, but (probably) the most straightforward way of
 * providing some sane CL:LISTEN semantics for line-mode console
 * channel requires _dedicated input thread_.
 *
 * LISTEN should return true iff the next (READ-CHAR) won't have to
 * wait. As our console may be shared with another process, entirely
 * out of our control, looking at the events in PeekConsoleEvent
 * result (and searching for #\Return) doesn't cut it.
 *
 * We decided that console input thread must do something smarter than
 * a bare loop of continuous ReadConsoleW(). On Unix, user experience
 * with the terminal is entirely unaffected by the fact that some
 * process does (or doesn't) call read(); the situation on MS Windows
 * is different.
 *
 * Echo output and line editing present on MS Windows while some
 * process is waiting in ReadConsole(); otherwise all input events are
 * buffered. If our thread were calling ReadConsole() all the time, it
 * would feel like Unix cooked mode.
 *
 * But we don't write a Unix emulator here, even if it sometimes feels
 * like that; therefore preserving this aspect of console I/O seems a
 * good thing to us.
 *
 * LISTEN itself becomes trivial with dedicated input thread, but the
 * goal stated above -- provide `native' user experience with blocked
 * console -- don't play well with this trivial implementation.
 *
 * What's currently implemented is a compromise, looking as something
 * in between Unix cooked mode and Win32 line mode.
 *
 * 1. As long as no console I/O function is called (incl. CL:LISTEN),
 * console looks `blocked': no echo, no line editing.
 *
 * 2. (READ-CHAR), (READ-SEQUENCE) and other functions doing real
 * input result in the ReadConsole request (in a dedicated thread);
 *
 * 3. Once ReadConsole is called, it is not cancelled in the
 * middle. In line mode, it returns when <Enter> key is hit (or
 * something like that happens). Therefore, if line editing and echo
 * output had a chance to happen, console won't look `blocked' until
 * the line is entered (even if line input was triggered by
 * (READ-CHAR)).
 *
 * 4. LISTEN may request ReadConsole too (if no other thread is
 * reading the console and no data are queued). It's the only case
 * when the console becomes `unblocked' without any actual input
 * requested by Lisp code.  LISTEN check if there is at least one
 * input event in PeekConsole queue; unless there is such an event,
 * ReadConsole is not triggered by LISTEN.
 *
 * We _could_ make win32_tty_listen check for buffered #\Return
 * before requesting ReadConsole, but it would break a common CL
 * idiom for doing things in background, i.e. code like this:
 *
 * (PROGN (LOOP DOING (FUNCTION-TAKING-SOME-MILLISECONDS) UNTIL (LISTEN))
 *        .... (READ-CHAR) or (READ-LINE)... ;; handle user
 *                                           ;; input when needed
 *
 * As stated above, our main goal is ensuring that there _is_
 * something to read when LISTEN returns true; thus only a completed
 * line of input makes LISTEN return true. Given that, if LISTEN would
 * look for #\Return before requesting ReadConsole, the entire line
 * would have to be typed without line editing and echo output, and
 * that's inconvenient.
 *
 * Former SBCL behavior was to return true from (LISTEN) as soon as
 * there was some keyboard event buffered; the code above, obviously,
 * would then hang on (READ-CHAR), not calling
 * (FUNCTION-TAKING-SOME-MILLISECONDS) regularly any more.
 *
 * 5. Console-reading Lisp thread now may be interrupted immediately;
 * ReadConsole call itself, however, continues until the line is
 * entered.
 */

#ifdef THREADED_CONSOLE_INPUT

struct {
    WCHAR buffer[MAX_CONSOLE_TCHARS];
    DWORD head, tail;
    pthread_mutex_t lock;
    pthread_cond_t cond_has_data;
    pthread_cond_t cond_has_client;
    pthread_t thread;
    boolean initialized;
    HANDLE handle;
    boolean in_progress;
} ttyinput = {.lock = PTHREAD_MUTEX_INITIALIZER};

static void* tty_read_line_server()
{
    pthread_mutex_lock(&ttyinput.lock);
    while (ttyinput.handle) {
        DWORD nchars;
        BOOL ok;

        while (!ttyinput.in_progress)
            pthread_cond_wait(&ttyinput.cond_has_client,&ttyinput.lock);

        pthread_mutex_unlock(&ttyinput.lock);

        ok = ReadConsoleW(ttyinput.handle,
                          &ttyinput.buffer[ttyinput.tail],
                          MAX_CONSOLE_TCHARS-ttyinput.tail,
                          &nchars,NULL);

        pthread_mutex_lock(&ttyinput.lock);

        if (ok) {
            ttyinput.tail += nchars;
            pthread_cond_broadcast(&ttyinput.cond_has_data);
        }
        ttyinput.in_progress = 0;
    }
    pthread_mutex_unlock(&ttyinput.lock);
    return NULL;
}


static boolean tty_maybe_initialize_unlocked(HANDLE handle)
{
    if (!ttyinput.initialized) {
        if (!DuplicateHandle(GetCurrentProcess(),handle,
                             GetCurrentProcess(),&ttyinput.handle,
                             0,FALSE,DUPLICATE_SAME_ACCESS)) {
            return 0;
        }
        pthread_cond_init(&ttyinput.cond_has_data,NULL);
        pthread_cond_init(&ttyinput.cond_has_client,NULL);
        pthread_create(&ttyinput.thread,NULL,tty_read_line_server,NULL);
        ttyinput.initialized = 1;
    }
    return 1;
}

boolean win32_tty_listen(HANDLE handle)
{
    boolean result = 0;
    INPUT_RECORD ir;
    DWORD nevents;
    pthread_mutex_lock(&ttyinput.lock);
    if (!tty_maybe_initialize_unlocked(handle))
        result = 0;

    if (ttyinput.in_progress) {
        result = 0;
    } else {
        if (ttyinput.head != ttyinput.tail) {
            result = 1;
        } else {
            if (PeekConsoleInput(ttyinput.handle,&ir,1,&nevents) && nevents) {
                ttyinput.in_progress = 1;
                pthread_cond_broadcast(&ttyinput.cond_has_client);
            }
        }
    }
    pthread_mutex_unlock(&ttyinput.lock);
    return result;
}

static int tty_read_line_client(HANDLE handle, void* buf, int count)
{
    int result = 0;
    int nchars = count / sizeof(WCHAR);
    sigset_t pendset;

    if (!nchars)
        return 0;
    if (nchars>MAX_CONSOLE_TCHARS)
        nchars=MAX_CONSOLE_TCHARS;

    count = nchars*sizeof(WCHAR);

    pthread_mutex_lock(&ttyinput.lock);

    if (!tty_maybe_initialize_unlocked(handle)) {
        result = -1;
        errno = EIO;
        goto unlock;
    }

    while (!result) {
        while (ttyinput.head == ttyinput.tail) {
            if (!io_begin_interruptible(ttyinput.handle)) {
                ttyinput.in_progress = 0;
                result = -1;
                errno = EINTR;
                goto unlock;
            } else {
                if (!ttyinput.in_progress) {
                    /* We are to wait */
                    ttyinput.in_progress=1;
                    /* wake console reader */
                    pthread_cond_broadcast(&ttyinput.cond_has_client);
                }
                if (WaitForSingleObject(this_thread->private_events.events[1],
                                        0)!=WAIT_TIMEOUT) {
                    result= -1;
                    errno = EINTR;
                    goto unlock;
                }
                pthread_cond_wait(&ttyinput.cond_has_data, &ttyinput.lock);
                io_end_interruptible(ttyinput.handle);
            }
        }
        result = sizeof(WCHAR)*(ttyinput.tail-ttyinput.head);
        if (result > count) {
            result = count;
        }
        if (result) {
            if (result > 0) {
                DWORD nch,offset = 0;
                LPWSTR ubuf = buf;

                memcpy(buf,&ttyinput.buffer[ttyinput.head],count);
                ttyinput.head += (result / sizeof(WCHAR));
                if (ttyinput.head == ttyinput.tail)
                    ttyinput.head = ttyinput.tail = 0;

                for (nch=0;nch<result/sizeof(WCHAR);++nch) {
                    if (ubuf[nch]==13) {
                        ++offset;
                    } else {
                        ubuf[nch-offset]=ubuf[nch];
                    }
                }
                result-=offset*sizeof(WCHAR);

            }
        } else {
            result = -1;
            ttyinput.head = ttyinput.tail = 0;
            errno = EIO;
        }
    }
unlock:
    pthread_mutex_unlock(&ttyinput.lock);
    return result;
}

int win32_read_unicode_console(HANDLE handle, void* buf, int count)
{

    int result;
    result = tty_read_line_client(handle,buf,count);
    return result;
}
#else  /* THREADED_CONSOLE_INPUT */

int win32_read_unicode_console(HANDLE handle, void* buf, int count)
{

    DWORD nread = 0;
    BOOL ok;
    HANDLE ownhandle;
    DWORD nchars;

again:
    nchars = count>>1;
    if (nchars>MAX_CONSOLE_TCHARS) nchars = MAX_CONSOLE_TCHARS;

    if (WaitForSingleObject(this_thread->private_events.events[1],
                            0)!=WAIT_TIMEOUT) {
        errno = EINTR;
        return -1;
    }
    if (!io_begin_interruptible(handle)) {
        errno = EINTR;
        return -1;
    }
    ok = ReadConsoleW(handle,buf,nchars,&nread,NULL);
    io_end_interruptible(handle);
    if (ok) {
        if (!nread) {
            DWORD err = GetLastError();
            odxprint(io,"[EINTR] ReadConsole succeeds w/o nread => %u\n", err);
            errno = EINTR;
            return -1;
        } else {
            DWORD nch,offset = 0;
            LPWSTR ubuf = buf;
            for (nch=0;nch<nread;++nch) {
                if (ubuf[nch]==13) {
                    ++offset;
                } else {
                    ubuf[nch-offset]=ubuf[nch];
                }
            }
            nread-=offset;
            if(!nread)
                goto again;
            return 2*nread;
        }
    } else {
        DWORD err = GetLastError();
        odxprint(io,"ReadConsole fails => %u\n", err);
        errno = (err==ERROR_OPERATION_ABORTED ? EINTR : EIO);
        return -1;
    }
}

#endif  /* THREADED_CONSOLE_INPUT */


boolean win32_maybe_interrupt_io(void* thread)
{
    struct thread *th = thread;
    boolean done = 0;
    if (ptr_CancelIoEx) {
        HANDLE h = (HANDLE)
            InterlockedExchangePointer((volatile LPVOID *)
                                       &th->synchronous_io_handle_and_flag,
                                       (LPVOID)INVALID_HANDLE_VALUE);
        if (h && (h!=INVALID_HANDLE_VALUE)) {
#ifdef THREADED_CONSOLE_INPUT
            if (console_handle_p(h)) {
                pthread_mutex_lock(&ttyinput.lock);
                pthread_cond_broadcast(&ttyinput.cond_has_data);
                pthread_mutex_unlock(&ttyinput.lock);
            }
#endif
            if (ptr_CancelSynchronousIo) {
                pthread_mutex_lock(&th->os_thread->fiber_lock);
                done = ptr_CancelSynchronousIo(th->os_thread->fiber_group->handle);
                pthread_mutex_unlock(&th->os_thread->fiber_lock);
            }
            return (!!done)|(!!ptr_CancelIoEx(h,NULL));
        }
    }
    return 0;
}


static const LARGE_INTEGER zero_large_offset = {.QuadPart = 0LL};

int win32_unix_write(FDTYPE fd, void * buf, int count)
{
    HANDLE handle;
    DWORD written_bytes;
    OVERLAPPED overlapped;
    struct thread * self = arch_os_get_current_thread();
    BOOL waitInGOR;
    LARGE_INTEGER file_position;
    BOOL seekable;
    BOOL ok;

    handle =(HANDLE)maybe_get_osfhandle(fd);
    if (console_handle_p(handle))
        return win32_write_unicode_console(handle,buf,count);

    odprintf("handle = 0x%p", handle);
    overlapped.hEvent = self->private_events.events[0];
    seekable = SetFilePointerEx(handle,
                                zero_large_offset,
                                &file_position,
                                FILE_CURRENT);
    if (seekable) {
        overlapped.Offset = file_position.LowPart;
        overlapped.OffsetHigh = file_position.HighPart;
    } else {
        overlapped.Offset = 0;
        overlapped.OffsetHigh = 0;
    }
    if (!io_begin_interruptible(handle)) {
        errno = EINTR;
        return -1;
    }
    ok = WriteFile(handle, buf, count, &written_bytes, &overlapped);
    io_end_interruptible(handle);

    if (ok) {
        goto done_something;
    } else {
        if (GetLastError()!=ERROR_IO_PENDING) {
            errno = EIO;
            return -1;
        } else {
            if(WaitForMultipleObjects(2,self->private_events.events,
                                      FALSE,INFINITE) != WAIT_OBJECT_0) {
                odprintf("write(%d, 0x%p, %d) EINTR",fd,buf,count);
                CancelIo(handle);
                waitInGOR = TRUE;
            } else {
                waitInGOR = FALSE;
            }
            if (!GetOverlappedResult(handle,&overlapped,&written_bytes,waitInGOR)) {
                if (GetLastError()==ERROR_OPERATION_ABORTED) {
                    errno = EINTR;
                } else {
                    errno = EIO;
                }
                return -1;
            } else {
                goto done_something;
            }
        }
    }
  done_something:
    if (seekable) {
        file_position.QuadPart += written_bytes;
        SetFilePointerEx(handle,file_position,NULL,FILE_BEGIN);
    }
    return written_bytes;
}

int win32_unix_read(FDTYPE fd, void * buf, int count)
{
    HANDLE handle;
    OVERLAPPED overlapped = {.Internal=0};
    DWORD read_bytes = 0;
    struct thread * self = arch_os_get_current_thread();
    DWORD errorCode = 0;
    BOOL waitInGOR = FALSE;
    BOOL ok = FALSE;
    LARGE_INTEGER file_position;
    BOOL seekable;

    handle = (HANDLE)maybe_get_osfhandle(fd);

    if (console_handle_p(handle)) {
        /* 1. Console is a singleton.
           2. The only way to cancel console handle I/O is to close it.
        */
        return win32_read_unicode_console(handle,buf,count);
    }
    odprintf("handle = 0x%p", handle);
    overlapped.hEvent = self->private_events.events[0];
    /* If it has a position, we won't try overlapped */
    seekable = SetFilePointerEx(handle,
                                zero_large_offset,
                                &file_position,
                                FILE_CURRENT);
    if (seekable)
    {
        overlapped.Offset = file_position.LowPart;
        overlapped.OffsetHigh = file_position.HighPart;
    } else {
        overlapped.Offset = 0;
        overlapped.OffsetHigh = 0;
    }
    if (!io_begin_interruptible(handle)) {
        errno = EINTR;
        return -1;
    }
    ok = ReadFile(handle,buf,count,&read_bytes, &overlapped);
    io_end_interruptible(handle);
    if (ok) {
        /* immediately */
        goto done_something;
    } else {
        errorCode = GetLastError();
        if (errorCode == ERROR_HANDLE_EOF ||
            errorCode == ERROR_BROKEN_PIPE ||
            errorCode == ERROR_NETNAME_DELETED) {
            read_bytes = 0;
            goto done_something;
        }
        if (errorCode!=ERROR_IO_PENDING) {
            /* is it some _real_ error? */
            errno = EIO;
            return -1;
        } else {
            if(WaitForMultipleObjects(2,self->private_events.events,
                                      FALSE,INFINITE) != WAIT_OBJECT_0) {
                CancelIo(handle);
                waitInGOR = TRUE;
                /* Waiting for IO only */
            } else {
                waitInGOR = FALSE;
            }
            ok = GetOverlappedResult(handle,&overlapped,&read_bytes,waitInGOR);
            if (!ok) {
                errorCode = GetLastError();
                if (errorCode == ERROR_HANDLE_EOF ||
                    errorCode == ERROR_BROKEN_PIPE ||
                    errorCode == ERROR_NETNAME_DELETED) {
                    read_bytes = 0;
                    goto done_something;
                } else {
                    if (errorCode == ERROR_OPERATION_ABORTED) {
                        errno = EINTR;      /* that's it. */
                    } else {
                        errno = EIO;        /* something unspecific */
                    }
                    return -1;
                }
            } else {
                goto done_something;
            }
        }
    }
  done_something:
    if (seekable) {
        file_position.QuadPart += read_bytes;
        SetFilePointerEx(handle,file_position,NULL,FILE_BEGIN);
    }
    return read_bytes;
}

int win32_wait_object_or_signal(HANDLE waitFor)
{
    struct thread * self = arch_os_get_current_thread();
    HANDLE handles[2];
    handles[0] = waitFor;
    handles[1] = self->private_events.events[1];
    return
        WaitForMultipleObjects(2,handles, FALSE,INFINITE);
}

/* Support routines for statistical profiler (contrib/sb-sprof).
 * (despite its `contribness', upstream SBCL already includes some
 * code for sb-sprof support, at least apparently).
 *
 * We do some tricks here to avoid deadlocks with GC; however, current
 * implementation is still potentially fragile -- a chunk of Lisp code
 * analyzing the stack runs between SuspendThread(otherthread) and
 * ResumeThread(otherthread), and it smells danger.
 *
 * WITHOUT-GCING is required, of course. Also, Lisp code in
 * resume-suspend shouldn't malloc() under the hood, and its probably
 * not the end of story. But it seems to work at least thus far so I
 * can profile HUNCHENTOOT threaded server in :TIME (wallclock) mode
 * without hanging [and even get informative results].
 *
 * Using SuspendThread and ResumeThread (as a replacement for SIGPROF)
 * seems to be required for representative result; we may issue a
 * `controlled software interrupt' (with page unmapping), and it would
 * be much safer, but the accuracy will suffer greatly (to the point
 * where sb-sprof becomes pretty useless, IMHO).
 *
 * Other threads may attempt malloc() and (gc); it does no harm,
 * they'll just be blocked until we are done with current SB-SPROF
 * sample. */


/* Data structure which is allocated before retrieving thread context
 * information and freed after the thread is resumed */

struct ctx_package {
    /* Runtime functions below operate with a pointer to ctx member,
     * calculating the entire structure address by subtracting offset
     * of &ctx. Thus we pass and accept only one argument -- context
     * SAP, and avoid mentioning _any_ auxilary data for the
     * context in SPROF<->runtime interface. */
    os_context_t ctx;

    /* on Win32, os_context_t doesn't include register set but a
     * pointer to CONTEXT where the registers reside. We have to pass
     * all this stuff to upper frames, so CONTEXT is allocated
     * dynamically as well. */
    CONTEXT win32_context;

    /* Pthread identifier of a target thread (to resume it). */
    os_thread_t os_thread;

    /* Low-level variant of pseudo-atomic bits is currently
     * represented by misaligned frame pointer below the pseudo-atomic
     * frame. We've no idea where SuspendThread() will freeze the
     * target (and I'd like to avoid rolling/deferring, both for
     * accuracy and simplicity), so we fix frame pointer chain after
     * suspend and restore it on resume.
     *
     * paframe: address of flagged frame pointer in target stack */
    os_vm_address_t paframe;

    /* pacont: original frame pointer, containing "pseudo-atomic
     * misalignment", that should be restored. */
    os_context_register_t pacont;
};

pthread_mutex_t sprof_suspend_lock = PTHREAD_MUTEX_INITIALIZER;

os_context_t* win32_suspend_get_context(pthread_t os_thread)
{
    struct ctx_package *data = NULL;
    extern pthread_mutex_t all_threads_lock;
    struct thread* p;

    /* Not only GC shouldn't happen -- any attempt to stop the world
     * results in deadlock (including interruption). However,
     * concurrent stop-the-world may have begun at this point; we use
     * trylock, and back off (returning NULL) when this situation is
     * detected. */
    if (pthread_mutex_trylock(&sprof_suspend_lock))
        return NULL;

    /* That one below should be replaced with unconditional lock, as
     * soon as I'll review and simplify complicated locking schemes in
     * thread.c. It may be safe to lock unconditionally even now, but
     * I can't know for sure. */
    if (pthread_mutex_trylock(&all_threads_lock))
        goto error_cleanup;

    /* Our pthread compatibility layer, unlike real pthreads, abhors
     * any invalid pthread_t in _any_ call, and can't check validity.
     * It's fixable and should be fixed, but until then we have to
     * validate any pthread_t at SBCL runtime level, by looking it up
     * in all_threads. */

    for_each_thread (p) {
        if (p->os_thread == os_thread) {
            /* Suspend thread immediately when we found it, so
             * all_threads_lock may be released and the thread still
             * won't die in our hands. */
            pthread_np_suspend(os_thread);
            break;
        }
    }
    pthread_mutex_unlock(&all_threads_lock);
    /* for_each_thread terminates with p==NULL unless break is
     * issued. It may be regarded as implementation detail, but it's
     * not the only opinion possible: I grasped it immediately without
     * looking at the definition of for_each_thread. */
    if (!p)
        goto error_cleanup;

    data = calloc(sizeof(*data),1); /* allocate with zero-fill */
    data->ctx.win32_context = &data->win32_context;

    pthread_np_get_thread_context(os_thread,&data->win32_context);

    /* Getting context of inactive fiber now fails. As any inactive
     * fiber is `in foreign call' from the Lisp point of view, we are
     * able to provide stack pointer, frame pointer and PC,
     * i.e. everything expected from foreign call context by
     * SB-SPROF.
     *
     * We regard zero PC as an indicator of invalid context. Zero ESP
     * would be just as good. */
    if (!*os_context_pc_addr(&data->ctx)) {
#ifdef LISP_FEATURE_X86
        struct thread* sap = os_thread->specifics[save_lisp_tls_key];
#else
        struct thread* sap = os_thread->specifics[specials];
#endif
        /* If pthread_t happens to be reused for non-lisp fiber, lisp
         * TSD pointer is empty. We have to back off and cleanup... */
        if (!sap) {
            /* ...but, as we've already suspended an innocent foreign
             * thread, we have to resume it as well. */
            pthread_np_resume(os_thread);
            goto error_cleanup;
        }
        /* csp_around_foreign_call == stack pointer before CALL insn.
         * When CALL happens, this address contains return PC.
         * A word below contains outer frame pointer. */
        void **csp = *(void***)sap->csp_around_foreign_call;
        void **topfp = *(csp-1);
        void *toppc = *csp;
        *os_context_pc_addr(&data->ctx) = (os_context_register_t)toppc;
        *os_context_fp_addr(&data->ctx) = (os_context_register_t)topfp;
        /* SB-SPROF shouldn't expect anything sensible from SP, at
         * least in foreign calls, except that it `covers' the frame
         * pointer (BTW, using `below/above' terms for stack locations
         * results in an awful mess almost inevitably, so I'd continue
         * to use `covers' / `covered by' for logical stack ordering
         * predicates) */
        *os_context_sp_addr(&data->ctx) = (os_context_register_t)csp;
    } else {
        os_vm_address_t fp = *(void**)os_context_fp_addr(&data->ctx);

        /* Fix low-level pseudo-atomic representation: traverse frame
         * pointer chain and fix first misaligned address we meet,
         * remembering its location. */
        while ((void*)fp < (void*)p->control_stack_end &&
               (void*)fp > (void*)p->control_stack_start &&
               /* Avoid infinite loop if the chain happens to be
                * circular */
               *(void**)fp > fp) {
            if ((*(os_context_register_t*)fp)&3) {
                data->pacont = *(os_context_register_t*)fp;
                data->paframe = fp;
                *(os_context_register_t*)fp &= ~((intptr_t)3);
                break;
            }
            fp = *(os_vm_address_t*)fp;
        }
    }
    data->os_thread = os_thread;
    return &data->ctx;

error_cleanup:
    if (data)
        free(data);
    pthread_mutex_unlock(&sprof_suspend_lock);
    return NULL;
}

void win32_resume(void *ctx)
{
    if (ctx) {
        /* Given CTX member, calculate entire data block address
         * (actually no-op on X86 as soon as CTX is at the beginning) */
        struct ctx_package *data = ctx - offsetof(struct ctx_package,ctx);

        /* Restore pseudo-atomic misalignment in the `beautified'
         * frame pointer (if any) */
        if (data->paframe)
            *(os_context_register_t*)data->paframe = data->pacont;

        /* Resume (for inactive fibers it means `unlock and allow
         * activation'). NB os_thread may become invalid after that
         * point. */
        pthread_np_resume(data->os_thread);
        free(data);

        /* Allow GC stop-the-world and interrupt signalling to take
         * place */
        pthread_mutex_unlock(&sprof_suspend_lock);
    }
}

HANDLE win32_dup_and_unwrap_fd(int fd, boolean inheritable)
{
    HANDLE outer;
    if (fd<0 || (!DuplicateHandle(GetCurrentProcess(),
                                  (HANDLE)_get_osfhandle(fd),
                                  GetCurrentProcess(),
                                  &outer,
                                  0u,
                                  inheritable,
                                  DUPLICATE_SAME_ACCESS)))
        return INVALID_HANDLE_VALUE;
    _close(fd);
    return outer;
}

/* EOF */

/* Local Variables: */
/* c-file-style: "stroustrup" */
/* End: */
