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

#include "sbcl.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include "runtime.h"
#include "interrupt.h"
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
#endif

#include "runtime.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */
#include "thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#include "dynbind.h"
#include "genesis/cons.h"
#include "genesis/fdefn.h"
#include "interr.h"             /* for lose() */
#include "alloc.h"
#include "gc-internal.h"
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
#include "pseudo-atomic.h"
#define IMMEDIATE_POST_MORTEM
#endif

#include "cpputil.h"

#ifdef LISP_FEATURE_WIN32
/*
 * Win32 doesn't have SIGSTKSZ, and we're not switching stacks anyway,
 * so define it arbitrarily
 */
#define SIGSTKSZ 1024
#endif

#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_SB_THREAD)
#define DELAY_THREAD_POST_MORTEM 5
#define LOCK_CREATE_THREAD
#endif

#ifdef LISP_FEATURE_FREEBSD
#define CREATE_CLEANUP_THREAD
#define LOCK_CREATE_THREAD
#endif

#ifdef LISP_FEATURE_SB_THREAD
struct thread_post_mortem {
#ifdef DELAY_THREAD_POST_MORTEM
    struct thread_post_mortem *next;
#endif
    os_thread_t os_thread;
    pthread_attr_t *os_attr;
    os_vm_address_t os_address;
};

#ifdef DELAY_THREAD_POST_MORTEM
static int pending_thread_post_mortem_count = 0;
pthread_mutex_t thread_post_mortem_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
static struct thread_post_mortem * volatile pending_thread_post_mortem = 0;
#endif

int dynamic_values_bytes=TLS_SIZE*sizeof(lispobj);  /* same for all threads */
struct thread *all_threads;
extern struct interrupt_data * global_interrupt_data;

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t all_threads_lock = PTHREAD_MUTEX_INITIALIZER;
#ifdef LOCK_CREATE_THREAD
static pthread_mutex_t create_thread_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#ifdef LISP_FEATURE_GCC_TLS
__thread struct thread *current_thread;
#endif
pthread_key_t lisp_thread = 0;
#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
extern lispobj call_into_lisp_first_time(lispobj fun, lispobj *args, int nargs)
#ifdef LISP_FEATURE_X86_64
    __attribute__((sysv_abi))
#endif
    ;
#endif

static void
link_thread(struct thread *th)
{
    if (all_threads) all_threads->prev=th;
    th->next=all_threads;
    th->prev=0;
    all_threads=th;
}

#ifdef LISP_FEATURE_SB_THREAD
static void
unlink_thread(struct thread *th)
{
    if (th->prev)
        th->prev->next = th->next;
    else
        all_threads = th->next;
    if (th->next)
        th->next->prev = th->prev;
}
#endif

static int run_lisp_function(lispobj function)
{
#if defined(LISP_FEATURE_X86)
    static int first_time = 1;
    if (first_time) {
	lispobj *args = NULL;
	first_time = 0;
	arch_os_get_current_thread()->control_stack_end =
	    __builtin_frame_address(0);
	return call_into_lisp_first_time(function,args,0);
    }
#endif
    return funcall0(function);
}

/* For safepoint-based builds, together with thread's
   csp_around_foreign_call pointer target, thread_qrl(thread) makes
   `quickly revokable lock'. Unlike most mutexes, this one is normally
   locked; by convention, other thread may read and use the thread's
   FFI-CSP location _either_ when the former holds the lock(mutex)
   _or_ when page permissions for FFI-CSP location were set to
   read-only.

   Combined semantic of QRL is not the same as the semantic of mutex
   returned by this function; rather, the mutex, when released by
   the owning thread, provides an edge-triggered notification of QRL release,
   which is represented by writing non-null *csp_around_foreign_call.
   
   When owner thread is `in Lisp' (i.e. a heap mutator), its FFI-CSP
   contains null, otherwise it points to the top of C stack that
   should be preserved by GENCGC. If another thread needs to wait for
   mutator state change with `in Lisp => in C' direction, it disables
   FFI-CSP overwrite using page protection, and takes the mutex
   returned by thread_qrl(). Page fault handler normally ends up in a
   routine releasing this mutex and waiting for some appropriate event
   to take it back.

   This way, each thread may modify its own FFI-CSP content freely
   without memory barriers (paying with exception handling overhead
   whenever a contention happens).
*/
static inline pthread_mutex_t* thread_qrl(struct thread* p)
{
    return 1 + p->state_lock;
}

lispobj reset_dynamic_values[TLS_SIZE];

static int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
    unsigned int i,j;
#ifdef LISP_FEATURE_SB_THREAD
    pthread_setspecific(lisp_thread, (void *)1);
#endif
#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_PPC)
    /* SIG_STOP_FOR_GC defaults to blocked on PPC? */
    unblock_gc_signals(0,0);
#endif
    function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;
    pthread_mutex_lock(thread_qrl(th));
    link_thread(th);
    th->os_thread=thread_self();
    protect_control_stack_hard_guard_page(1, NULL);
    protect_binding_stack_hard_guard_page(1, NULL);
    protect_alien_stack_hard_guard_page(1, NULL);
    protect_control_stack_guard_page(1, NULL);
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);

    memcpy(reset_dynamic_values, th, sizeof(reset_dynamic_values));

    return run_lisp_function(function);
}

#ifdef LISP_FEATURE_SB_THREAD
#define THREAD_STATE_LOCK_SIZE \
    2*(sizeof(pthread_mutex_t))+(sizeof(pthread_cond_t))
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
#define THREAD_CSP_PAGE_SIZE BACKEND_PAGE_BYTES
#else
#define THREAD_CSP_PAGE_SIZE 0
#endif
#else
#define THREAD_STATE_LOCK_SIZE 0
#define THREAD_CSP_PAGE_SIZE 0
#endif


#define THREAD_STRUCT_SIZE (thread_control_stack_size + BINDING_STACK_SIZE + \
                            ALIEN_STACK_SIZE +				\
                            THREAD_STATE_LOCK_SIZE +			\
                            dynamic_values_bytes +			\
                            32 * SIGSTKSZ +				\
                            THREAD_ALIGNMENT_BYTES +			\
			    THREAD_CSP_PAGE_SIZE)

#ifdef LISP_FEATURE_SB_THREAD
/* THREAD POST MORTEM CLEANUP
 *
 * Memory allocated for the thread stacks cannot be reclaimed while
 * the thread is still alive, so we need a mechanism for post mortem
 * cleanups. FIXME: We actually have three, for historical reasons as
 * the saying goes. Do we really need three? Nikodemus guesses that
 * not anymore, now that we properly call pthread_attr_destroy before
 * freeing the stack. */

static struct thread_post_mortem *
plan_thread_post_mortem(struct thread *corpse)
{
    if (corpse) {
        struct thread_post_mortem *post_mortem = malloc(sizeof(struct thread_post_mortem));
        gc_assert(post_mortem);
        post_mortem->os_thread = corpse->os_thread;
        post_mortem->os_attr = corpse->os_attr;
        post_mortem->os_address = corpse->os_address;
#ifdef DELAY_THREAD_POST_MORTEM
        post_mortem->next = NULL;
#endif
        return post_mortem;
    } else {
        /* FIXME: When does this happen? */
        return NULL;
    }
}

static void
perform_thread_post_mortem(struct thread_post_mortem *post_mortem)
{
#if defined(CREATE_POST_MORTEM_THREAD) || defined(IMMEDIATE_POST_MORTEM)
    pthread_detach(pthread_self());
#endif
    if (post_mortem) {
        #ifndef IMMEDIATE_POST_MORTEM
        gc_assert(!pthread_join(post_mortem->os_thread, NULL));
        #endif
        gc_assert(!pthread_attr_destroy(post_mortem->os_attr));
        free(post_mortem->os_attr);
#if defined(LISP_FEATURE_WIN32)
        os_invalidate_free(post_mortem->os_address, THREAD_STRUCT_SIZE);
#else
        os_invalidate(post_mortem->os_address, THREAD_STRUCT_SIZE);
#endif
        free(post_mortem);
    }
}

static void
schedule_thread_post_mortem(struct thread *corpse)
{
    struct thread_post_mortem *post_mortem = NULL;
    if (corpse) {
        post_mortem = plan_thread_post_mortem(corpse);

#ifdef DELAY_THREAD_POST_MORTEM
        pthread_mutex_lock(&thread_post_mortem_lock);
        /* First stick the new post mortem to the end of the queue. */
        if (pending_thread_post_mortem) {
            struct thread_post_mortem *next = pending_thread_post_mortem;
            while (next->next) {
                next = next->next;
            }
            next->next = post_mortem;
        } else {
            pending_thread_post_mortem = post_mortem;
        }
        /* Then, if there are enough things in the queue, clean up one
         * from the head -- or increment the count, and null out the
         * post_mortem we have. */
        if (pending_thread_post_mortem_count > DELAY_THREAD_POST_MORTEM) {
            post_mortem = pending_thread_post_mortem;
            pending_thread_post_mortem = post_mortem->next;
        } else {
            pending_thread_post_mortem_count++;
            post_mortem = NULL;
        }
        pthread_mutex_unlock(&thread_post_mortem_lock);
        /* Finally run, the cleanup, if any. */
        perform_thread_post_mortem(post_mortem);
#elif defined(CREATE_POST_MORTEM_THREAD)
        gc_assert(!pthread_create(&thread, NULL, perform_thread_post_mortem, post_mortem));
#else
#ifndef IMMEDIATE_POST_MORTEM
        post_mortem = (struct thread_post_mortem *)
            swap_lispobjs((lispobj *)(void *)&pending_thread_post_mortem,
                          (lispobj)post_mortem);
#endif
        perform_thread_post_mortem(post_mortem);
#endif
    }
}

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT

pthread_mutex_t resurrected_lock = PTHREAD_MUTEX_INITIALIZER;
struct thread *resurrected_thread;

unsigned int resurrectable_waiters = 0;
unsigned int max_resurrectable_waiters = 0;

#endif

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
int
new_thread_trampoline(struct thread *th)
{
    lispobj function;
    int result, lock_ret;
#if defined(LISP_FEATURE_WIN32)
    int i;
    struct lisp_exception_frame exception_frame;
#endif
#if defined(LISP_FEATURE_SB_AUTO_FPU_SWITCH)
    x87_fldcw(th->saved_c_fpu_mode);
#endif
    FSHOW((stderr,"/creating thread %lu\n", thread_self()));
    wos_install_interrupt_handlers(&exception_frame);
#ifndef LISP_FEATURE_WIN32
    check_deferrables_blocked_or_lose(0);
    check_gc_signals_unblocked_or_lose(0);
    pthread_setspecific(lisp_thread, (void *)1);
#endif
    function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) {
        /* FIXME: handle error */
        lose("arch_os_thread_init failed\n");
    }

    th->os_thread=thread_self();
    protect_control_stack_guard_page(1, NULL);
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);
    /* Since GC can only know about this thread from the all_threads
     * list and we're just adding this thread to it, there is no
     * danger of deadlocking even with SIG_STOP_FOR_GC blocked (which
     * it is not). */

 resurrect:
    /* Experimental: allow create_thread reuse threads that are about
       to die */
    
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    *th->csp_around_foreign_call = (lispobj)&function;
    odxprint(safepoints, "New thread to be linked: %p\n", th);
#endif
    pthread_mutex_lock(thread_qrl(th));

    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    link_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);


    odxprint(safepoints, "...Linked: %p\n", th);
    gc_assert(lock_ret == 0);

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    BEGIN_GC_UNSAFE_CODE;
#endif
    result = run_lisp_function(function);

#ifndef LISP_FEATURE_SB_GC_SAFEPOINT
    block_blockable_signals(0, 0);
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);    
    unlink_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    odxprint(safepoints, "...Unlinked: %p\n", th);
    gc_assert(lock_ret == 0);

#else
    /* Here we know that GC is blocked -- we are in unsafe code */
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
    END_GC_UNSAFE_CODE;
    /* Here we are in a `foreign call' again. GC won't wait for us, so
       it's safe to unlink. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);    
    unlink_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    pthread_mutex_unlock(thread_qrl(th));

#ifdef LISP_FEATURE_WIN32
    if (th->os_thread->created_as_fiber) {
	goto die;
    }
#endif

    if (resurrectable_waiters >= max_resurrectable_waiters)
	goto die;
    
    th->next = NULL;
    th->prev = NULL;
    struct timespec deadline;
    int ret;
    int relative = 5000;
    struct timeval tv;
    ret = gettimeofday(&tv, NULL);
    if (ret)
	goto die;
    deadline.tv_sec = tv.tv_sec + (tv.tv_usec * 1000 + relative) / 1000000000;
    deadline.tv_nsec = (tv.tv_usec * 1000 + relative) % 1000000000;

    if (pthread_mutex_trylock(&resurrected_lock)) {
	odxprint(safepoints, "Resurrect lock busy at %p, dying", th);
	goto die;
    }
    odxprint(safepoints, "Resurrect lock taken by %p", th);
    ++resurrectable_waiters;
    
    struct thread* lastwaiter;
    
    if (resurrected_thread) {
	for (lastwaiter = resurrected_thread; lastwaiter->next;
	     lastwaiter = lastwaiter->next);
	th->prev = lastwaiter;
	lastwaiter->next = th;
    } else {
	th->prev = NULL;
	resurrected_thread = th;
    }

    odxprint(safepoints, "Before timed wait %p", th);
    pthread_cond_timedwait(th->state_cond, &resurrected_lock, &deadline);
    odxprint(safepoints, "After timed wait %p", th);

    --resurrectable_waiters;
    /* owning reslock */
    if (th->state == STATE_DEAD) {
	odxprint(safepoints, "State DEAD, final unlinking.. %p", th);
	/* no change, unlink */
	if (th->next)
	    th->next->prev = th->prev;
	if (th->prev)
	    th->prev->next = th->next;
	else
	    resurrected_thread = th->next;
	odxprint(safepoints, "State DEAD, dying.. %p", th);
	pthread_mutex_unlock(&resurrected_lock);
	goto die;
    }

    odxprint(safepoints, "State UNDEAD - Resurrecting, %p", th);


    /* odxprint(safepoints, "Resurrecting, %p", th); */
    pthread_mutex_unlock(&resurrected_lock);
    
    function = th->no_tls_value_marker;

#ifdef LISP_FEATURE_SB_AUTO_FPU_SWITCH
    unsigned int fpu_mode = th->in_lisp_fpu_mode;
#endif
    int os_data_size = offsetof(struct thread, binding_stack_start);

    memcpy(((void*)th)+MAX_INTERRUPTS*sizeof(lispobj)+sizeof(*th),
	   ((void*)reset_dynamic_values)+MAX_INTERRUPTS*sizeof(lispobj)+sizeof(*th),
	   (TLS_SIZE*sizeof(lispobj))-(MAX_INTERRUPTS*sizeof(lispobj)+sizeof(*th)));
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
#ifdef LISP_FEATURE_SB_AUTO_FPU_SWITCH
    th->in_lisp_fpu_mode = fpu_mode;
#endif

#ifdef LISP_FEATURE_GENCGC
    gc_set_region_empty(&th->alloc_region);
#endif
    goto resurrect;


die:
#endif	/* safepoints */


    /* lock_ret = pthread_mutex_lock(&all_threads_lock); */
    /* gc_assert(lock_ret == 0); */
    /* unlink_thread(th); */
    /* pthread_mutex_unlock(&all_threads_lock); */

    /* FIXME: Seen th->tls_cookie>=0 below.
       Meaningless for unsigned (lispobj) tls_cookie.
       What it's supposed to mean? */
    if(th->tls_cookie!=0) arch_os_thread_cleanup(th);
    pthread_mutex_destroy(th->state_lock);
    pthread_mutex_destroy(th->state_lock+1);
    pthread_cond_destroy(th->state_cond);

#if defined(LISP_FEATURE_WIN32)
    free((os_vm_address_t)th->interrupt_data);
#else
    os_invalidate_free((os_vm_address_t)th->interrupt_data,
                  (sizeof (struct interrupt_data)));
#endif

#if defined(LISP_FEATURE_WIN32)
    for (i = 0; i<
	     (int) (sizeof(th->private_events.events)/
		    sizeof(th->private_events.events[0])); ++i) {
      CloseHandle(th->private_events.events[i]);
    }
    TlsSetValue(OUR_TLS_INDEX,NULL);
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    FSHOW((stderr, "Deallocating mach port %x\n", THREAD_STRUCT_TO_EXCEPTION_PORT(th)));
    mach_port_move_member(mach_task_self(),
                          THREAD_STRUCT_TO_EXCEPTION_PORT(th),
                          MACH_PORT_NULL);
    mach_port_deallocate(mach_task_self(),
                         THREAD_STRUCT_TO_EXCEPTION_PORT(th));
    mach_port_destroy(mach_task_self(),
                      THREAD_STRUCT_TO_EXCEPTION_PORT(th));
#endif

    schedule_thread_post_mortem(th);
    FSHOW((stderr,"/exiting thread %lu\n", thread_self()));
    return result;
}

#endif /* LISP_FEATURE_SB_THREAD */

static void
free_thread_struct(struct thread *th)
{
#if defined(LISP_FEATURE_WIN32)
    if (th->interrupt_data) {
        os_invalidate_free((os_vm_address_t) th->interrupt_data,
                      (sizeof (struct interrupt_data)));
    }
    os_invalidate_free((os_vm_address_t) th->os_address,
                  THREAD_STRUCT_SIZE);
#else
    if (th->interrupt_data)
        os_invalidate((os_vm_address_t) th->interrupt_data,
                      (sizeof (struct interrupt_data)));
    os_invalidate((os_vm_address_t) th->os_address,
                  THREAD_STRUCT_SIZE);
#endif
}

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another
 * thread
 */

static struct thread *
create_thread_struct(lispobj initial_function) {
    union per_thread_data *per_thread;
    struct thread *th=0;        /*  subdue gcc */
    void *spaces=0;
    void *aligned_spaces=0;
    
#if defined(LISP_FEATURE_WIN32)
    size_t allocate_control_stack = 0;
#else
    size_t allocate_control_stack = thread_control_stack_size;
#endif

#if defined(LISP_FEATURE_SB_THREAD) || defined(LISP_FEATURE_WIN32)
    unsigned int i;
#endif

    /* May as well allocate all the spaces at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed. SPACES must be appropriately aligned, since the GC
     * expects the control stack to start at a page boundary -- and
     * the OS may have even more rigorous requirements. We can't rely
     * on the alignment passed from os_validate, since that might
     * assume the current (e.g. 4k) pagesize, while we calculate with
     * the biggest (e.g. 64k) pagesize allowed by the ABI. */
    spaces=os_allocate_lazily(THREAD_STRUCT_SIZE);

    if(!spaces)
        return NULL;
    /* Aligning up is safe as THREAD_STRUCT_SIZE has
     * THREAD_ALIGNMENT_BYTES padding. */
    aligned_spaces = (void *)((((uword_t)(char *)spaces)
                               + THREAD_ALIGNMENT_BYTES-1)
                              &~(uword_t)(THREAD_ALIGNMENT_BYTES-1));
    per_thread=(union per_thread_data *)
        (aligned_spaces+
         allocate_control_stack+
         BINDING_STACK_SIZE+
         ALIEN_STACK_SIZE +
         THREAD_STATE_LOCK_SIZE);

#ifdef LISP_FEATURE_SB_THREAD
    /* If lazy allocation is used, it makes sense to recommit a page
       or two here and there in advance, if we know they're going to
       be used. It may be omitted safely, we just save a few
       pagefaults #!+win32 (and SEH traps) */
    
    /* A page of alien stack + TLS dynamic values */
    os_validate_recommit(((void*)per_thread)-os_vm_page_size,
			 dynamic_values_bytes + os_vm_page_size);

    /* A page of binding stack (the first one, surely it will be used) */
    os_validate_recommit(aligned_spaces+allocate_control_stack,
			 os_vm_page_size);

    /* A page for top-of-stack address storage */
    os_validate_recommit(PTR_ALIGN_UP(&per_thread->dynamic_values[TLS_SIZE],
				      os_vm_page_size),
			 sizeof(lispobj));
    
    for(i = 0; i < (dynamic_values_bytes / sizeof(lispobj)); i++)
        per_thread->dynamic_values[i] = NO_TLS_VALUE_MARKER_WIDETAG;
    if (all_threads == 0) {
        if(SymbolValue(FREE_TLS_INDEX,0)==UNBOUND_MARKER_WIDETAG) {
            SetSymbolValue
                (FREE_TLS_INDEX,
                 /* FIXME: should be MAX_INTERRUPTS -1 ? */
                 make_fixnum(MAX_INTERRUPTS+
                             sizeof(struct thread)/sizeof(lispobj)),
                 0);
            SetSymbolValue(TLS_INDEX_LOCK,make_fixnum(0),0);
        }
#define STATIC_TLS_INIT(sym,field) \
  ((struct symbol *)(sym-OTHER_POINTER_LOWTAG))->tls_index= \
  make_fixnum(THREAD_SLOT_OFFSET_WORDS(field))

        STATIC_TLS_INIT(BINDING_STACK_START,binding_stack_start);
#ifdef BINDING_STACK_POINTER
        STATIC_TLS_INIT(BINDING_STACK_POINTER,binding_stack_pointer);
#endif
        STATIC_TLS_INIT(CONTROL_STACK_START,control_stack_start);
        STATIC_TLS_INIT(CONTROL_STACK_END,control_stack_end);
#ifdef ALIEN_STACK
        STATIC_TLS_INIT(ALIEN_STACK,alien_stack_pointer);
#endif
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
        STATIC_TLS_INIT(PSEUDO_ATOMIC_BITS,pseudo_atomic_bits);
#endif
#undef STATIC_TLS_INIT
    }
#endif

    th=&per_thread->thread;
    th->os_address = spaces;
    th->control_stack_start = aligned_spaces;
    th->binding_stack_start=
        (lispobj*)((void*)th->control_stack_start+allocate_control_stack);
    th->control_stack_end = th->binding_stack_start;
    th->control_stack_guard_page_protected = T;
    th->alien_stack_start=
        (lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    set_binding_stack_pointer(th,th->binding_stack_start);
    th->this=th;
    th->os_thread=0;

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    th->gc_safepoint_context = 0;
    th->csp_around_foreign_call = 0;
    th->pc_around_foreign_call = 0;
#endif

#ifdef LISP_FEATURE_SB_THREAD
    th->os_attr=malloc(sizeof(pthread_attr_t));
    th->state_lock=(pthread_mutex_t *)((void *)th->alien_stack_start +
                                       ALIEN_STACK_SIZE);
    pthread_mutex_init(th->state_lock, NULL);
    pthread_mutex_init(th->state_lock+1, NULL);
    th->state_cond=(pthread_cond_t *)((void *)th->state_lock +
                                      2*(sizeof(pthread_mutex_t)));
    pthread_cond_init(th->state_cond, NULL);
#endif
    th->state=STATE_RUNNING;
#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD
    th->alien_stack_pointer=((void *)th->alien_stack_start
                             + ALIEN_STACK_SIZE
			     - N_WORD_BYTES
			     /* see win32-os.h whose THREAD_ALIEN_RESERVE
				is now defined to non-zero */
			     - THREAD_ALIEN_RESERVE);
#else
    th->alien_stack_pointer=((void *)th->alien_stack_start);
#endif
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64) || defined(LISP_FEATURE_SB_THREAD)
    th->pseudo_atomic_bits=0;
#endif
#ifdef LISP_FEATURE_GENCGC
    gc_set_region_empty(&th->alloc_region);
#endif
#ifdef LISP_FEATURE_SB_THREAD
    /* This parallels the same logic in globals.c for the
     * single-threaded foreign_function_call_active, KLUDGE and
     * all. */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    th->foreign_function_call_active = 0;
#else
    th->foreign_function_call_active = 1;
#endif
#endif

#ifndef LISP_FEATURE_SB_THREAD
    /* the tls-points-into-struct-thread trick is only good for threaded
     * sbcl, because unithread sbcl doesn't have tls.  So, we copy the
     * appropriate values from struct thread here, and make sure that
     * we use the appropriate SymbolValue macros to access any of the
     * variable quantities from the C runtime.  It's not quite OAOOM,
     * it just feels like it */
    SetSymbolValue(BINDING_STACK_START,(lispobj)th->binding_stack_start,th);
    SetSymbolValue(CONTROL_STACK_START,(lispobj)th->control_stack_start,th);
    SetSymbolValue(CONTROL_STACK_END,(lispobj)th->control_stack_end,th);
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
    SetSymbolValue(ALIEN_STACK,(lispobj)th->alien_stack_pointer,th);
    SetSymbolValue(PSEUDO_ATOMIC_BITS,(lispobj)th->pseudo_atomic_bits,th);
#endif
#endif
    bind_variable(CURRENT_CATCH_BLOCK,make_fixnum(0),th);
    bind_variable(CURRENT_UNWIND_PROTECT_BLOCK,make_fixnum(0),th);
    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,make_fixnum(0),th);
    bind_variable(INTERRUPT_PENDING, NIL,th);
    bind_variable(INTERRUPTS_ENABLED,T,th);
    bind_variable(ALLOW_WITH_INTERRUPTS,T,th);
    bind_variable(GC_PENDING,NIL,th);
    bind_variable(ALLOC_SIGNAL,NIL,th);
#ifdef PINNED_OBJECTS
    bind_variable(PINNED_OBJECTS,NIL,th);
#endif
#ifdef LISP_FEATURE_SB_THREAD
    bind_variable(STOP_FOR_GC_PENDING,NIL,th);
#endif

#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
    bind_variable(GC_SAFE,NIL,th);
    bind_variable(IN_SAFEPOINT,NIL,th);
    bind_variable(DISABLE_SAFEPOINTS,NIL,th);
#endif

#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    access_control_stack_pointer(th)=th->control_stack_start;
#endif

#if defined(LISP_FEATURE_WIN32)
    th->interrupt_data = (struct interrupt_data *)
        calloc((sizeof (struct interrupt_data)),1);
#else
    th->interrupt_data = (struct interrupt_data *)
        os_validate(0,(sizeof (struct interrupt_data)));
#endif
    if (!th->interrupt_data) {
        free_thread_struct(th);
        return 0;
    }
    th->interrupt_data->pending_handler = 0;
    th->interrupt_data->gc_blocked_deferrables = 0;
#ifdef LISP_FEATURE_PPC
    th->interrupt_data->allocation_trap_context = 0;
#endif
    th->no_tls_value_marker=initial_function;

#if defined(LISP_FEATURE_WIN32)
    for (i = 0; i<sizeof(th->private_events.events)/
           sizeof(th->private_events.events[0]); ++i) {
      th->private_events.events[i] = CreateEvent(NULL,FALSE,FALSE,NULL);
    }
#ifdef LISP_FEATURE_SB_AUTO_FPU_SWITCH
    th->in_lisp_fpu_mode = 0;
    {
	struct thread* parent = arch_os_get_current_thread();
	if (parent) {
	    th->saved_c_fpu_mode = parent->saved_c_fpu_mode;
	    th->saved_lisp_fpu_mode = parent->saved_lisp_fpu_mode;
	} else {
	    th->saved_c_fpu_mode = (x87_fnstcw() & ~1);
	    th->saved_lisp_fpu_mode =
		((th->saved_c_fpu_mode & 0xf2ff) | 0x0200);
	}
    }
#endif
    th->gc_safepoint_context = 0;
    th->csp_around_foreign_call = 0;
    th->pc_around_foreign_call = 0;
    th->synchronous_io_handle_and_flag = 0;
#endif
    th->stepping = NIL;
    return th;
}

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
mach_port_t setup_mach_exception_handling_thread();
kern_return_t mach_thread_init(mach_port_t thread_exception_port);

#endif


void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
#ifdef LISP_FEATURE_SB_THREAD
    pthread_key_create(&lisp_thread, 0);
#endif
    if(th) {
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
        setup_mach_exception_handling_thread();
#endif
        initial_thread_trampoline(th); /* no return */
    } else lose("can't create initial thread\n");
}

#ifdef LISP_FEATURE_SB_THREAD

#ifndef __USE_XOPEN2K
extern int pthread_attr_setstack (pthread_attr_t *__attr, void *__stackaddr,
                                  size_t __stacksize);
#endif

boolean create_os_thread(struct thread *th,os_thread_t *kid_tid)
{
    /* The new thread inherits the restrictive signal mask set here,
     * and enables signals again when it is set up properly. */
    sigset_t oldset;
    boolean r=1;
    int retcode = 0, initcode;

    FSHOW_SIGNAL((stderr,"/create_os_thread: creating new thread\n"));

    /* Blocking deferrable signals is enough, no need to block
     * SIG_STOP_FOR_GC because the child process is not linked onto
     * all_threads until it's ready. */
    block_deferrable_signals(0, &oldset);
    
#ifdef LOCK_CREATE_THREAD
    retcode = pthread_mutex_lock(&create_thread_lock);
    gc_assert(retcode == 0);
    FSHOW_SIGNAL((stderr,"/create_os_thread: got lock\n"));
#endif

    if((initcode = pthread_attr_init(th->os_attr)) ||
       /* call_into_lisp_first_time switches the stack for the initial
        * thread. For the others, we use this. */
#if defined(LISP_FEATURE_WIN32)
       (pthread_attr_setstacksize(th->os_attr, thread_control_stack_size)) ||
#else
       (pthread_attr_setstack(th->os_attr,th->control_stack_start,
                              thread_control_stack_size)) ||
#endif
       (retcode = pthread_create
        (kid_tid,th->os_attr,(void *(*)(void *))new_thread_trampoline,th))) {
        FSHOW_SIGNAL((stderr, "init = %d\n", initcode));
        FSHOW_SIGNAL((stderr, "pthread_create returned %d, errno %d\n",
                      retcode, errno));
        if(retcode < 0) {
            perror("create_os_thread");
        }
        r=0;
    }

#ifdef LOCK_CREATE_THREAD
    retcode = pthread_mutex_unlock(&create_thread_lock);
    gc_assert(retcode == 0);
    FSHOW_SIGNAL((stderr,"/create_os_thread: released lock\n"));
#endif
    thread_sigmask(SIG_SETMASK,&oldset,0);
    return r;
}

os_thread_t create_thread(lispobj initial_function) {
    struct thread *th, *thread = arch_os_get_current_thread();
    os_thread_t kid_tid = 0;

#ifdef LISP_FEATURE_WIN32
    if (!pthread_self()->fiber_factory) {
#else
    if (1) {
#endif
	if (resurrected_thread && !pthread_mutex_trylock(&resurrected_lock)) {
	    if (resurrected_thread) {
		th = resurrected_thread;
		odxprint(safepoints, "%p reused by %p", th,
			 arch_os_get_current_thread());
		if (th->next) th->next->prev = NULL;
		resurrected_thread = th->next;
		th->no_tls_value_marker = initial_function;
		th->state = STATE_RUNNING;
		pthread_cond_signal(th->state_cond);
	    } else {
		th = NULL;
	    }
	    pthread_mutex_unlock(&resurrected_lock);
	    if (th) {
		/* wait_for_thread_state_change(th,STATE_SUSPENDED); */
		return th->os_thread;
	    }
	}
    }
    /* Experimental: going to test the interpretation of
       runtime-targeted calls as `floatless'.

       create_thread is the only place I know that is definitely NOT
       floatless. */
    
    establish_c_fpu_world();
    
    /* Must defend against async unwinds. */
    if (SymbolValue(INTERRUPTS_ENABLED, thread) != NIL)
        lose("create_thread is not safe when interrupts are enabled.\n");

    /* Assuming that a fresh thread struct has no lisp objects in it,
     * linking it to all_threads can be left to the thread itself
     * without fear of gc lossage. initial_function violates this
     * assumption and must stay pinned until the child starts up. */
    th = create_thread_struct(initial_function);
    if (th && !create_os_thread(th,&kid_tid)) {
        free_thread_struct(th);
        kid_tid = 0;
    }
    return kid_tid;
}

/* stopping the world is a two-stage process.  From this thread we signal
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */

#if defined(LISP_FEATURE_SB_GC_SAFEPOINT)

const char * t_nil_str(lispobj value)
{
        if (value == T) return "T";
        if (value == NIL) return "NIL";
        return "?";
}

/* Factored out stuff used to interrupt blocking IO in the target
   thread.

   Our purpose in wake_thread (and friends) is to make a blocking
   function running in target thread return EINTR or something
   equivalent.

   Win32 API (before Vista) doesn't provide means to cancel blocking
   IO asynchronously, hence we reimplement cancellable blocking IO,
   e.g. with OVERLAPPED operations. */

void wake_thread_io(struct thread * thread)
{
#ifdef LISP_FEATURE_WIN32
    SetEvent(thread->private_events.events[1]);
    win32_maybe_interrupt_io(thread);
#endif
}

static inline int thread_may_gc()
{
  // Thread may gc if all of these are true:
  // 1) SIG_STOP_FOR_GC is unblocked
  // 2) GC_INHIBIT is NIL
  // 3) INTERRUPTS_ENABLED is not-NIL //? not so
  // 4) !pseudo_atomic

  struct thread * self = arch_os_get_current_thread();

  if (SymbolValue(GC_INHIBIT, self) != NIL) {
    return 0;
  }

  if (SymbolTlValue(GC_PENDING, self) != T &&
      SymbolTlValue(GC_PENDING, self) != NIL) {
    return 0;
  }

  return 1;
}

static inline int thread_may_interrupt()
{
  struct thread * self = arch_os_get_current_thread();
  // Thread may be interrupted if all of these are true:
  // 1) deferrables are unblocked
  // 2) INTERRUPTS_ENABLED is not-nil
  // 3) !pseudo_atomic (now guaranteed by safepoint-related callers)

  if (SymbolValue(INTERRUPTS_ENABLED, self) == NIL)
      return 0;

  if (SymbolValue(GC_PENDING, self) != NIL)
      return 0;

  if (SymbolValue(STOP_FOR_GC_PENDING, self) != NIL)
      return 0;

  if (deferrables_blocked_p(&self->os_thread->blocked_signal_set))
      return 0;

  return 1; 
}

// returns 0 if skipped, 1 otherwise
int check_pending_interrupts(os_context_t *ctx)
{
  struct thread * p = arch_os_get_current_thread();
  pthread_t pself = p->os_thread;
  sigset_t oldset;
  if (pself->pending_signal_set) {
      if (__sync_fetch_and_and(&pself->pending_signal_set,0)) {
	  SetSymbolValue(INTERRUPT_PENDING, T, p);
      }
  }
  if (!thread_may_interrupt())
    return 0;
  if (SymbolValue(INTERRUPT_PENDING, p) == NIL)
    return 0;
  SetSymbolValue(INTERRUPT_PENDING, NIL, p);
  oldset = pself->blocked_signal_set;
  pself->blocked_signal_set = deferrable_sigset;

  if (ctx) fake_foreign_function_call(ctx);
  funcall0(StaticSymbolFunction(RUN_INTERRUPTION));
  if (ctx) undo_fake_foreign_function_call(ctx);

  pself->blocked_signal_set = oldset;
  if (ctx) ctx->sigmask = oldset;
  return 1;
}

// returns 0 if skipped, 1 otherwise
int check_pending_gc()
{
    struct thread * self = arch_os_get_current_thread();
    int done = 0;
    sigset_t sigset = 0;

    /* gc_assert(!(*self->csp_around_foreign_call)); */
    
    if ((SymbolValue(IN_SAFEPOINT,self) == T) &&
        ((SymbolValue(GC_INHIBIT,self) == NIL) &&
	 (SymbolValue(GC_PENDING,self) == NIL))) {
        SetSymbolValue(IN_SAFEPOINT,NIL,self);
    }
    if (thread_may_gc() && (SymbolValue(IN_SAFEPOINT, self) == NIL)) {
        if ((SymbolTlValue(GC_PENDING, self) == T)) {
	    lispobj gc_happened = NIL;

	    bind_variable(IN_SAFEPOINT,T,self);
	    block_deferrable_signals(NULL,&sigset);
	    if(SymbolTlValue(GC_PENDING,self)==T)
		gc_happened = funcall0(StaticSymbolFunction(SUB_GC));
	    unbind_variable(IN_SAFEPOINT,self);
	    thread_sigmask(SIG_SETMASK,&sigset,NULL);
	    if (gc_happened == T) {
		/* POST_GC wants to enable interrupts */
		if (SymbolValue(INTERRUPTS_ENABLED,self) == T ||
		    SymbolValue(ALLOW_WITH_INTERRUPTS,self) == T) {
		    funcall0(StaticSymbolFunction(POST_GC));
		}
		done = 1;
	    }
	}
    }
    return done;
}

lispobj fn_by_pc(uword_t pc)
{
  lispobj obj = (lispobj)search_read_only_space((void*)pc);
  if (!obj)
    obj = (lispobj)search_static_space((void*)pc);
  if (!obj)
    obj = (lispobj)search_dynamic_space((void*)pc);
  return obj;
}

const char * fn_name(lispobj fn)
{
  return "unknown";
}

const char * t_nil_s(lispobj symbol)
{
  struct thread * self = arch_os_get_current_thread();
  return t_nil_str(SymbolValue(symbol, self));
}


struct gc_dispatcher {
    pthread_mutex_t mx_initiator;
    pthread_mutex_t mx_gpunmapped;
    pthread_mutex_t mx_gptransition;
    pthread_mutex_t mx_gcing;

    struct thread *th_gpunmapper;
    int stopped;
    boolean interrupt;
} gc_dispatcher = {
    .mx_initiator = PTHREAD_MUTEX_INITIALIZER,
    .mx_gpunmapped = PTHREAD_MUTEX_INITIALIZER,
    .mx_gptransition = PTHREAD_MUTEX_INITIALIZER,
    .mx_gcing = PTHREAD_MUTEX_INITIALIZER,
};


static inline boolean set_thread_csp_access(struct thread* p, boolean writable)
{
    os_protect(p->csp_around_foreign_call,sizeof(lispobj),
	       writable? (OS_VM_PROT_READ|OS_VM_PROT_WRITE)
	       : (OS_VM_PROT_READ));
    return !!*p->csp_around_foreign_call;
}


/* maybe_become_stw_initiator -- if there is no stop-the-world action
   in progress, begin it by unmapping GC page, and record current
   thread as GC initiator.

   Return true if current thread becomes a GC initiator, or already is
   a GC initiator.

   GC initiator is a thread responsible for preliminary stopping the
   world and conducting an automatically-triggered GC.
*/

static inline boolean maybe_become_stw_initiator(boolean interrupt)
{
    struct thread* self = arch_os_get_current_thread();
    if (!gc_dispatcher.th_gpunmapper) {
	pthread_mutex_lock(&gc_dispatcher.mx_gptransition);
	if (!gc_dispatcher.th_gpunmapper) {
	    /* Register */
	    gc_dispatcher.th_gpunmapper = self;
	    gc_dispatcher.interrupt = interrupt;
	    /* take a lock, released after GC page is mapped. */
	    pthread_mutex_lock(&gc_dispatcher.mx_gpunmapped);
	    pthread_mutex_lock(&gc_dispatcher.mx_gcing);
	}
	gc_dispatcher.stopped = 0;
	pthread_mutex_unlock(&gc_dispatcher.mx_gptransition);
	/* Moved past unlock to make locking interval shorter */
	unmap_gc_page();
    }
    return gc_dispatcher.th_gpunmapper == self;
}

static inline boolean maybe_let_the_world_go()
{
    struct thread* self = arch_os_get_current_thread();
    if (gc_dispatcher.th_gpunmapper == self) {
	gc_dispatcher.th_gpunmapper = NULL;
	pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
	return 1;
    } else {
	return 0;
    }
}


void gc_stop_the_world()
{
    struct thread* self = arch_os_get_current_thread(), *p;
    boolean interrupt;
    while (!maybe_become_stw_initiator(0)) {
	pthread_mutex_lock(&gc_dispatcher.mx_gcing);
	pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
    }
    interrupt = gc_dispatcher.interrupt;
    if (!gc_dispatcher.stopped++) {
	pthread_mutex_lock(&all_threads_lock);
	/* Phase 1: ensure all threads are aware of the need to
	   stop, or locked in the foreign code if appropriate. */
	for_each_thread(p) {
	    pthread_mutex_t *p_qrl = thread_qrl(p);
	    if (p==self)
		continue;
		
	    if (!set_thread_csp_access(p,0)) {
		/* No CSP, it's bound to trap soon (and bring
		   aw-stop when trapped). */
		odxprint(safepoints,"taking qrl %p of %p", p_qrl, p);
		pthread_mutex_lock(p_qrl);
		odxprint(safepoints,"taken qrl %p of %p", p_qrl, p);
		/* Here we are holding the lock. */
		if (SymbolTlValue(GC_INHIBIT,p)!=T) {
		    SetTlSymbolValue(GC_SAFE,T,p);
		} else {
		    SetTlSymbolValue(GC_SAFE,NIL,p);
		}
		pthread_mutex_unlock(p_qrl);
	    } else {
		if (!interrupt) {
		    if (SymbolTlValue(GC_INHIBIT,p)==T) {
			SetTlSymbolValue(STOP_FOR_GC_PENDING,T,p);
			set_thread_csp_access(p,1);
			SetTlSymbolValue(GC_SAFE,NIL,p);
		    } else {
			SetTlSymbolValue(GC_SAFE,T,p);
		    }
		}
	    }
	}
	map_gc_page();
	pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
	/* All threads with GP unmapped are rolling further. */
	odxprint(safepoints,"after remapping GC page %p",self);
	    
	SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
	if (!interrupt) {
	    for_each_thread(p) {
		if (p==self)
		    continue;
		if (SymbolTlValue(GC_SAFE,p)!=T) {
		    /* Wait for thread to `park'. NB it _always_ does
		       it with pending interrupt, so CSP locking is
		       not needed */
		    odxprint(safepoints,"waiting final parking %p (qrl %p)",p, thread_qrl(p));
		    pthread_mutex_lock(thread_qrl(p));
		    pthread_mutex_unlock(thread_qrl(p));
		}
		SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,p);
		SetTlSymbolValue(GC_PENDING,NIL,p);
	    }
	}
    }
}

void gc_start_the_world()
{
    struct thread* self = arch_os_get_current_thread(), *p;
    boolean interrupt = gc_dispatcher.interrupt;
    if (!--gc_dispatcher.stopped) {
	for_each_thread(p) {
	    if (!interrupt) {
		SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,p);
		SetTlSymbolValue(GC_PENDING,NIL,p);
	    }
	    if (SymbolTlValue(INTERRUPT_PENDING,p)!=T ||
		SymbolTlValue(INTERRUPTS_ENABLED,p)!=T)
		set_thread_csp_access(p,1);
	}
	pthread_mutex_unlock(&all_threads_lock);
	    
	/* Release everyone */
	maybe_let_the_world_go();
    }
}


static inline void thread_pitstop(os_context_t *ctxptr)
{
    struct thread* self = arch_os_get_current_thread(), *p;
    boolean inhibitor = (SymbolTlValue(GC_INHIBIT,self)==T);

    odxprint(safepoints,"pitstop [%p]", ctxptr);
    if (inhibitor) {
	SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
	/* Free qrl to let know we're ready... */
	pthread_mutex_unlock(thread_qrl(self));
	pthread_mutex_lock(&gc_dispatcher.mx_gpunmapped);
	pthread_mutex_lock(thread_qrl(self));
	pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
	/* Enable FF-CSP recording (not hurt: will gc at pit-stop, and
	   pit-stop always waits for GC end) */
	set_thread_csp_access(self,1);
    } else {
	if (maybe_become_stw_initiator(0)) {
	    odxprint(safepoints,"pitstop initiator [%p]", ctxptr);
	    gc_stop_the_world();
	    check_pending_gc();
	    gc_start_the_world();
	} else {
	    /* An innocent thread which is not an initiator _and_ is
	       not objecting. */
	    odxprint(safepoints,"pitstop yelding [%p]", ctxptr);
	    if (!set_thread_csp_access(self,1)) {
		*self->csp_around_foreign_call = (lispobj)ctxptr;
		pthread_mutex_unlock(thread_qrl(self));
		pthread_mutex_lock(&gc_dispatcher.mx_gcing);
		*self->csp_around_foreign_call = 0;
		pthread_mutex_lock(thread_qrl(self));
		pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
	    } else {
		pthread_mutex_lock(&gc_dispatcher.mx_gcing);
		set_thread_csp_access(self,1);
		BEGIN_GC_UNSAFE_CODE;
		pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
		while(check_pending_interrupts(ctxptr));
		END_GC_UNSAFE_CODE;
		return;
	    }
	}
    }
    while(check_pending_interrupts(ctxptr));
}

static inline void thread_edge(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();
    set_thread_csp_access(self,1);
    if (os_get_csp(self)) {
	if (!self->pc_around_foreign_call)
	    return;		/* trivialize */
	odxprint(safepoints,"edge leaving [%p]", ctxptr);
	if (SymbolTlValue(GC_INHIBIT,self)!=T) {
	    if (SymbolTlValue(INTERRUPT_PENDING,self)==T &&
		SymbolTlValue(INTERRUPTS_ENABLED,self)==T) {
		pthread_mutex_lock(&gc_dispatcher.mx_gcing);
		set_thread_csp_access(self,1);
		BEGIN_GC_UNSAFE_CODE;
		pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
		while(check_pending_interrupts(ctxptr));
		END_GC_UNSAFE_CODE;
	    } else {
		pthread_mutex_lock(&gc_dispatcher.mx_gcing);
		odxprint(safepoints,"edge leaving [%p] took gcing", ctxptr);
		pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
		odxprint(safepoints,"edge leaving [%p] released gcing", ctxptr);
	    }
	}
    } else {
	/* Entering. */
	odxprint(safepoints,"edge entering [%p]", ctxptr);
	while(check_pending_interrupts(ctxptr));
	*self->csp_around_foreign_call = (lispobj)ctxptr;
	if (SymbolTlValue(GC_INHIBIT,self)!=T) {
	    pthread_mutex_unlock(thread_qrl(self));
	    pthread_mutex_lock(&gc_dispatcher.mx_gcing);
	    pthread_mutex_lock(thread_qrl(self));
	    pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
	} else {
	    SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
	    pthread_mutex_unlock(thread_qrl(self));
	    pthread_mutex_lock(&gc_dispatcher.mx_gpunmapped);
	    pthread_mutex_lock(thread_qrl(self));
	    pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
	}
    }
}


/* thread_register_gc_trigger --

   Called by GENCGC in each thread where GC_PENDING becomes T because
   allocated memory size has crossed the threshold in
   auto_gc_trigger. For the new collective GC sequence, its first call
   marks a process-wide beginning of GC.
*/
void thread_register_gc_trigger()
{
    struct thread* self = arch_os_get_current_thread();
    /* This function should be called instead of former
       set_pseudo_atomic_interrupted(), e.g. never with true
       GC_INHIBIT */
    gc_assert(SymbolTlValue(GC_INHIBIT,self)!=T);

    /* unmap GC page, signal other threads... */
    maybe_become_stw_initiator(0);
}



/* GC exception handlers, preliminary:
   
   When any gc-inhibitor traps on LD(GSPA), it sets
   STOP_FOR_GC_PENDING, then waits for gc page remapping and returns.

   When !gc-inhibitor thread traps on LD(GSPA), it compares itself
   (electorally) to th_actor. If there is no th_actor yet, it wins and
   takes mx_initiator. If there is th_actor and current thread is
   better, current thread doesn't complete QRL release until
   gc_start_the_world(): this way, thread owning mx_initiator always
   releases it after GC end.

   [STW] Elected actor should wait for each of the rest threads.  
 */

void wake_thread(struct thread * thread)
{
    struct thread * p;
    struct thread * self = arch_os_get_current_thread();

    wake_thread_io(thread);

    if (SymbolTlValue(INTERRUPT_PENDING,thread)==T)
    	return;

    SetTlSymbolValue(INTERRUPT_PENDING,T,thread);
    
    if ((SymbolTlValue(GC_PENDING,thread)==T)||
    	(SymbolTlValue(STOP_FOR_GC_PENDING,thread)==T))
    	return;

    pthread_mutex_unlock(&all_threads_lock);

    if (maybe_become_stw_initiator(1)) {
	gc_stop_the_world();
	gc_start_the_world();
    }
    pthread_mutex_lock(&all_threads_lock);
    return;
}

void thread_in_safety_transition(os_context_t *ctx)
{
    thread_edge(ctx);
}

void thread_in_lisp_raised(os_context_t *ctx)
{
    thread_pitstop(ctx);
}

void thread_interrupted(os_context_t *ctx)
{
    thread_pitstop(ctx);
}


#endif

#ifndef LISP_FEATURE_SB_GC_SAFEPOINT
/* To avoid deadlocks when gc stops the world all clients of each
 * mutex must enable or disable SIG_STOP_FOR_GC for the duration of
 * holding the lock, but they must agree on which. */
void gc_stop_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int status, lock_ret;
    int gc_page_signalling = 0;
    int gc_blockers = 0;
    
#ifdef LOCK_CREATE_THREAD
    /* KLUDGE: Stopping the thread during pthread_create() causes deadlock
     * on FreeBSD. */
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on create_thread_lock\n"));
    lock_ret = pthread_mutex_lock(&create_thread_lock);
    gc_assert(lock_ret == 0);
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got create_thread_lock\n"));
#endif
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on lock\n"));
    /* keep threads from starting while the world is stopped. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got lock\n"));
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    /* Phase 1, make sure that all threads are: 1) have noted the need to interrupt; or 2) in gc-safe code */

    for(p=all_threads; p; p=p->next) {
        gc_assert(p->os_thread != 0);
        FSHOW_SIGNAL((stderr,"/gc_stop_the_world: thread=%lu, state=%x\n",
                      p->os_thread, thread_state(p)));
        if(p!=th) {
	    if (thread_state(p)!=STATE_RUNNING)
		continue;
            FSHOW_SIGNAL((stderr,"/gc_stop_the_world: suspending thread %lu\n",
                          p->os_thread));
            /* We already hold all_thread_lock, P can become DEAD but
             * cannot exit, ergo it's safe to use pthread_kill. */
            status=pthread_kill(p->os_thread,SIG_STOP_FOR_GC);
            if (status==ESRCH) {
                /* This thread has exited. */
                gc_assert(thread_state(p)==STATE_DEAD);
            } else if (status) {
                lose("cannot send suspend thread=%lu: %d, %s\n",
                     p->os_thread,status,strerror(status));
            }
        }
    }

    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:signals sent\n"));
    for(p=all_threads;p;p=p->next) {
        if (p!=th) {
            FSHOW_SIGNAL
                ((stderr,
                  "/gc_stop_the_world: waiting for thread=%lu: state=%x\n",
                  p->os_thread, thread_state(p)));
            wait_for_thread_state_change(p, STATE_RUNNING);
            if (p->state == STATE_RUNNING)
                lose("/gc_stop_the_world: unexpected state");
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:end\n"));
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int lock_ret;
    int count = 0;
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will get consed on the front of
     * all_threads, but it won't have been stopped so won't need
     * restarting */
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:begin\n"));

    for(p=all_threads;p;p=p->next) {
        gc_assert(p->os_thread!=0);
        if (p!=th) {
	    ++count;
            lispobj state = thread_state(p);
            if (state != STATE_DEAD) {
                if(state != STATE_SUSPENDED) {
                    lose("gc_start_the_world: wrong thread state is %d\n",
                         fixnum_value(state));
                }
                FSHOW_SIGNAL((stderr, "/gc_start_the_world: resuming %lu\n",
                              p->os_thread));
		set_thread_state(p, STATE_RUNNING);
            }  
        }
    }
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);
#ifdef LOCK_CREATE_THREAD
    lock_ret = pthread_mutex_unlock(&create_thread_lock);
    gc_assert(lock_ret == 0);
#endif
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:end\n"));
}
#endif

#endif	/* another LISP_FEATURE_SB_GC_SAFEPOINT ifdeffery. FIXME CLEANUP. */


int
thread_yield()
{
    return sched_yield();
}

/* If the thread id given does not belong to a running thread (it has
 * exited or never even existed) pthread_kill _may_ fail with ESRCH,
 * but it is also allowed to just segfault, see
 * <http://udrepper.livejournal.com/16844.html>.
 *
 * Relying on thread ids can easily backfire since ids are recycled
 * (NPTL recycles them extremely fast) so a signal can be sent to
 * another process if the one it was sent to exited.
 *
 * We send signals in two places: signal_interrupt_thread sends a
 * signal that's harmless if delivered to another thread, but
 * SIG_STOP_FOR_GC is fatal.
 *
 * For these reasons, we must make sure that the thread is still alive
 * when the pthread_kill is called and return if the thread is
 * exiting. */

int
kill_safely(os_thread_t os_thread, int signal)
{
    FSHOW_SIGNAL((stderr,"/kill_safely: %lu, %d\n", os_thread, signal));
#if defined(LISP_FEATURE_WIN32) && !defined(LISP_FEATURE_SB_THREAD)
    return 0;
#else
    {
#ifdef LISP_FEATURE_SB_THREAD
        sigset_t oldset;
        struct thread *thread;
        /* Frequent special case: resignalling to self.  The idea is
           that leave_region safepoint will acknowledge the signal, so
           there is no need to take locks, roll thread to safepoint
           etc. */
        if (os_thread == pthread_self()) {
          pthread_kill(os_thread, signal);
          return 0;
        }
        /* pthread_kill is not async signal safe and we don't want to be
         * interrupted while holding the lock. */
        block_deferrable_signals(0, &oldset);
        pthread_mutex_lock(&all_threads_lock);
        for (thread = all_threads; thread; thread = thread->next) {
	    if (thread->os_thread == os_thread) {
		/* We found the target (well, maybe just a coincided
		   thread id -- it's harmless). */
		int status = pthread_kill(os_thread, signal);
		if (status)
		    lose("kill_safely: pthread_kill failed with %d\n", status);
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
		wake_thread(thread);
#endif
		break;
	    }
        }
        pthread_mutex_unlock(&all_threads_lock);
        thread_sigmask(SIG_SETMASK,&oldset,0);
        if (thread)
          return 0;
        else
          return -1;
#else
        int status;
        if (os_thread != 0)
            lose("kill_safely: who do you want to kill? %d?\n", os_thread);
        /* Dubious (as in don't know why it works) workaround for the
         * signal sometimes not being generated on darwin. */
#ifdef LISP_FEATURE_DARWIN
        {
            sigset_t oldset;
            sigprocmask(SIG_BLOCK, &deferrable_sigset, &oldset);
            status = raise(signal);
            sigprocmask(SIG_SETMASK,&oldset,0);
        }
#else
        status = raise(signal);
#endif
        if (status == 0) {
            return 0;
        } else {
            lose("cannot raise signal %d, %d %s\n",
                 signal, status, strerror(errno));
        }
#endif
    }
#endif
}
