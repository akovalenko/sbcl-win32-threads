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
extern lispobj call_into_lisp_first_time(lispobj fun, lispobj *args, int nargs);
#endif

static void
link_thread(struct thread *th)
{
    if (all_threads) all_threads->prev=th;
    th->next=all_threads;
    th->prev=0;
    COMPILER_BARRIER;
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

static int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    lispobj *args = NULL;
#endif
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
    link_thread(th);
    th->os_thread=thread_self();
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_hard_guard_page(1, NULL);
    protect_binding_stack_hard_guard_page(1, NULL);
    protect_alien_stack_hard_guard_page(1, NULL);
    protect_control_stack_guard_page(1, NULL);
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);
#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    return call_into_lisp_first_time(function,args,0);
#else
    return funcall0(function);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD
#define THREAD_STATE_LOCK_SIZE \
    (sizeof(pthread_mutex_t))+(sizeof(pthread_cond_t))
#else
#define THREAD_STATE_LOCK_SIZE 0
#endif

#define THREAD_STRUCT_SIZE (thread_control_stack_size + BINDING_STACK_SIZE + \
                            ALIEN_STACK_SIZE +                               \
                            THREAD_STATE_LOCK_SIZE +                         \
                            dynamic_values_bytes +                           \
                            32 * SIGSTKSZ +                                  \
                            THREAD_ALIGNMENT_BYTES)

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
static inline void lock_suspend_info(const char * file, int line);
static inline void unlock_suspend_info(const char * file, int line);

static inline int maybe_wake_gc_begin();
static inline void maybe_wake_gc_end(int oldword);

pthread_mutex_t resurrected_lock = PTHREAD_MUTEX_INITIALIZER;
struct thread *resurrected_thread;

unsigned int resurrectable_waiters = 0;
unsigned int max_resurrectable_waiters = 4;

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
    lispobj initial_state[TLS_SIZE];
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
    memcpy(initial_state, th, sizeof initial_state);
 resurrect:
    /* Experimental: allow create_thread reuse threads that are about
       to die */
    
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    gc_enter_foreign_call(&function,0);
    odxprint(safepoints, "New thread to be linked: %p\n", th);
    lock_suspend_info(__FILE__,__LINE__);
    if (suspend_info.suspend)
	th->state = STATE_SUSPENDED;
    unlock_suspend_info(__FILE__,__LINE__);
#endif

    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    link_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);


    odxprint(safepoints, "...Linked: %p\n", th);
    gc_assert(lock_ret == 0);

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    gc_leave_foreign_call();
#endif

    result = funcall0(function);

    /* Block GC */

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
    gc_enter_foreign_call(&function,0);
    bind_variable(IN_SAFEPOINT,T,th);	 /* so it won't attempt GC */
    SetSymbolValue(GC_PENDING,T,th);	 /* to check result */
    SetSymbolValue(GC_SAFE,T,th);	 /* discommend unmap_gc_page */
    /* May go into full suspend; returns without
       csp_around_foreign_call */
    gc_leave_foreign_call();
    /* If safepoint had to wait for parallel GC, that GC updated page
       tables already. */
    if (SymbolValue(GC_PENDING,th)==T) {
	SetSymbolValue(GC_PENDING,NIL,th);
	gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
    }
    unbind(th);
    /* After gc_safepoint, csp_around_foreign_call is cleared (and we
       had no context); thus gc_stop_the_world (if any) will mark us
       as GC-blocker. 

       For now, only phase 1 of stopping the world takes
       all_threads_lock; it relies on (1) each new thread going into
       full suspend() in the beginning of initial funcall, and (2)
       dying threads ending with safepoint (which makes them
       phase1-blockers).

       At this point, if there is any gc_stop_the_world in progress,
       it marked this thread with STATE_GC_BLOCKER. We wake such gc
       after unlinking. */

    odxprint(safepoints, "Dying thread to be unlinked: %p\n", th);
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);    
    unlink_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    odxprint(safepoints, "Dying thread unlinked: %p\n", th);

    /* If we are a GC-blocker, wake GC */
    {
	int maybe_wake_gc = 0;
	pthread_mutex_lock(th->state_lock);
	if (th->state == STATE_PHASE1_BLOCKER||
	    th->state == STATE_PHASE2_BLOCKER||
	    th->state == STATE_INTERRUPT_BLOCKER) {
	    maybe_wake_gc = 1;
	    /* If GC waits for th, it's going to close its alloc
	       region => we may just go on. */
	}
	th->state = STATE_DEAD;
	pthread_cond_broadcast(th->state_cond);
	pthread_mutex_unlock(th->state_lock);
	
	if (maybe_wake_gc) {
	    lock_suspend_info(__FILE__,__LINE__);
	    int gcwake = maybe_wake_gc_begin();
	    unlock_suspend_info(__FILE__,__LINE__);
	    maybe_wake_gc_end(gcwake);
	}
    }

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

    unsigned int fpu_mode = th->in_lisp_fpu_mode;
    int os_data_size = offsetof(struct thread, binding_stack_start);

    
    memcpy(os_data_size + (void*)th,
	   os_data_size + (void*)initial_state,
	   (sizeof initial_state) - os_data_size);
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    th->in_lisp_fpu_mode = fpu_mode;

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
    pthread_cond_destroy(th->state_cond);

    os_invalidate_free((os_vm_address_t)th->interrupt_data,
                  (sizeof (struct interrupt_data)));

#if defined(LISP_FEATURE_WIN32)
    for (i = 0; i<
	     (int) (sizeof(th->private_events.events)/
		    sizeof(th->private_events.events[0])); ++i) {
      CloseHandle(th->private_events.events[i]);
    }
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
    aligned_spaces = (void *)((((unsigned long)(char *)spaces)
                               + THREAD_ALIGNMENT_BYTES-1)
                              &~(unsigned long)(THREAD_ALIGNMENT_BYTES-1));
    per_thread=(union per_thread_data *)
        (aligned_spaces+
         thread_control_stack_size+
         BINDING_STACK_SIZE+
         ALIEN_STACK_SIZE +
         THREAD_STATE_LOCK_SIZE);

#ifdef LISP_FEATURE_SB_THREAD
    os_validate_recommit(per_thread, dynamic_values_bytes);
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
        (lispobj*)((void*)th->control_stack_start+thread_control_stack_size);
    th->control_stack_end = th->binding_stack_start;
    th->control_stack_guard_page_protected = T;
    th->alien_stack_start=
        (lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    set_binding_stack_pointer(th,th->binding_stack_start);
    th->this=th;
    th->os_thread=0;
#ifdef LISP_FEATURE_SB_THREAD
    th->os_attr=malloc(sizeof(pthread_attr_t));
    th->state_lock=(pthread_mutex_t *)((void *)th->alien_stack_start +
                                       ALIEN_STACK_SIZE);
    pthread_mutex_init(th->state_lock, NULL);
    th->state_cond=(pthread_cond_t *)((void *)th->state_lock +
                                      (sizeof(pthread_mutex_t)));
    pthread_cond_init(th->state_cond, NULL);
#endif
    th->state=STATE_RUNNING;
#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD
    th->alien_stack_pointer=((void *)th->alien_stack_start
                             + ALIEN_STACK_SIZE-N_WORD_BYTES);
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
    bind_variable(GC_SAFE,GC_SAFE,th); /* Phase1-safe */
    bind_variable(IN_SAFEPOINT,NIL,th);
    bind_variable(DISABLE_SAFEPOINTS,NIL,th);
#endif

#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    access_control_stack_pointer(th)=th->control_stack_start;
#endif

    th->interrupt_data = (struct interrupt_data *)
        os_validate(0,(sizeof (struct interrupt_data)));
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
    th->gc_safepoint_context = 0;
    th->csp_around_foreign_call = 0;
    th->pc_around_foreign_call = 0;
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

#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)

struct threads_suspend_info suspend_info = {
    0, PTHREAD_MUTEX_INITIALIZER, PTHREAD_MUTEX_INITIALIZER,
    SUSPEND_REASON_NONE, 0, NULL, NULL, 0, 0
};

const char * t_nil_str(lispobj value)
{
        if (value == T) return "T";
        if (value == NIL) return "NIL";
        return "?";
}

static inline void lock_suspend_info(const char * file, int line)
{
  odprintf("locking suspend_info.lock (%s:%d)", file, line);
  pthread_mutex_lock(&suspend_info.lock);
  odprintf("locked suspend_info.lock (%s:%d)", file, line);
}

static inline void unlock_suspend_info(const char * file, int line)
{
  odprintf("unlocking suspend_info.lock (%s:%d)", file, line);
  pthread_mutex_unlock(&suspend_info.lock);
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
    SetEvent(thread->private_events.events[1]);
}

boolean wake_needed(struct thread * thread)
{
  if ((!thread->os_thread->pending_signal_set)||
      (SymbolTlValue(STOP_FOR_GC_PENDING, thread) != NIL)||
      (thread->csp_around_foreign_call)||
      (SymbolTlValue(INTERRUPT_PENDING, thread) == T)||
      (thread->state != STATE_RUNNING)) {
      return 0;
  }
  return 1;
}

void wake_thread(struct thread * thread)
{
    struct thread * p;
    struct thread * self = arch_os_get_current_thread();
    boolean nonio_wake_required = 0; 

    wake_thread_io(thread);


    pthread_mutex_lock(thread->state_lock);
    nonio_wake_required = wake_needed(thread);    
    pthread_mutex_unlock(thread->state_lock);

    /* if there was no need to GC-page-based wake, there is still no
       need to do it after IO wake */

    nonio_wake_required = nonio_wake_required &&
	SymbolTlValue(STOP_FOR_GC_PENDING,self)==NIL &&
	SymbolTlValue(GC_PENDING,self)==NIL &&
	SymbolTlValue(STOP_FOR_GC_PENDING,thread)==NIL &&
	SymbolTlValue(GC_PENDING,thread)==NIL &&
	SymbolTlValue(INTERRUPT_PENDING,thread)==NIL;

    pthread_mutex_unlock(&all_threads_lock);

    if (nonio_wake_required)
	wake_the_world();

    /* pointless, but we have already too many functions with
       asymmetric locking here :( */
    pthread_mutex_lock(&all_threads_lock);
}


static inline void safepoint_cycle_state(lispobj state)
{
  struct thread * self = arch_os_get_current_thread();
  int gcwake = 0;
  pthread_mutex_lock(self->state_lock);
  if (self->state == STATE_PHASE1_BLOCKER ||
      self->state == STATE_PHASE2_BLOCKER ||
      self->state == STATE_INTERRUPT_BLOCKER) {
      gcwake = maybe_wake_gc_begin();
  }
  unlock_suspend_info(__FILE__, __LINE__);
  if (self->state != state) {
      self->state = state;
      pthread_cond_broadcast(self->state_cond);
      if (gcwake) {
          pthread_mutex_unlock(self->state_lock);
          maybe_wake_gc_end(gcwake);
	  if (state == STATE_RUNNING) {
	      return;
	  }
          pthread_mutex_lock(self->state_lock);
      }
  }
  if (state != STATE_RUNNING) {
      while (self->state == state)
	  pthread_cond_wait(self->state_cond, self->state_lock);
  }
  pthread_mutex_unlock(self->state_lock);
}


static inline void suspend()
{
  struct thread * self = arch_os_get_current_thread();
  if (!self->gc_safepoint_context && !self->csp_around_foreign_call)
      lose("Attempting to do full suspend w/o csp or ctx");
  SetSymbolValue(STOP_FOR_GC_PENDING, NIL, self);
  safepoint_cycle_state(STATE_SUSPENDED);
  if ((SymbolTlValue(IN_SAFEPOINT,self)==NIL||
       SymbolTlValue(GC_SAFE,self)==T) &&
      SymbolTlValue(GC_PENDING,self)==T)
      SetSymbolValue(GC_PENDING, NIL, self);
}

static inline void suspend_briefly()
{
  if (suspend_info.phase == 1 || suspend_info.reason == SUSPEND_REASON_INTERRUPT) {
    safepoint_cycle_state(STATE_SUSPENDED_BRIEFLY);
  } else {
    unlock_suspend_info(__FILE__, __LINE__);
  }
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

static inline int thread_may_suspend_for_gc()
{
  // Thread may gc if all of these are true:
  // 1) SIG_STOP_FOR_GC is unblocked
  // 2) GC_INHIBIT is NIL
  // Kovalenko: the next one is dubious
  // 3) INTERRUPTS_ENABLED is not-NIL
  // 4) !pseudo_atomic

  struct thread * self = arch_os_get_current_thread();

  if (SymbolValue(GC_INHIBIT, self) != NIL) {
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
int check_pending_interrupts()
{
  struct thread * p = arch_os_get_current_thread();
  pthread_t pself = p->os_thread;
  sigset_t oldset;
  if (pself->pending_signal_set) {
      if (InterlockedExchange(&pself->pending_signal_set,0)) {
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

  BEGIN_GC_UNSAFE_CODE;
  funcall0(StaticSymbolFunction(RUN_INTERRUPTION));
  END_GC_UNSAFE_CODE;

  pself->blocked_signal_set = oldset;
  return 1;
}

// returns 0 if skipped, 1 otherwise
int check_pending_gc()
{
    struct thread * self = arch_os_get_current_thread();
    int done = 0;
    sigset_t sigset = 0;
    /* Take off recursive protection as soon as GC_PENDING becomes !T */
    if ((SymbolValue(IN_SAFEPOINT,self) == T) &&
        (SymbolValue(GC_PENDING,self) != T)) {
        SetSymbolValue(IN_SAFEPOINT,NIL,self);
    }
    if (thread_may_gc() && (SymbolValue(IN_SAFEPOINT, self) == NIL)) {
        if ((SymbolTlValue(GC_PENDING, self) == T)) {
            bind_variable(IN_SAFEPOINT,T,self);
            block_deferrable_signals(NULL,&sigset);
	    lispobj gc_happened;
	    BEGIN_GC_UNSAFE_CODE;
	    gc_happened = funcall0(StaticSymbolFunction(SUB_GC));
	    END_GC_UNSAFE_CODE;
		
	    int concurrency = fixnum_value(self->tls_cookie);
	    self->tls_cookie = 0;
            unbind_variable(IN_SAFEPOINT,self);
            thread_sigmask(SIG_SETMASK,&sigset,NULL);

            if (gc_happened == T) {
		if (SymbolValue(INTERRUPTS_ENABLED,self) == T ||
		    SymbolValue(ALLOW_WITH_INTERRUPTS,self) == T) {
		    BEGIN_GC_UNSAFE_CODE;
		    funcall0(StaticSymbolFunction(POST_GC));
		    END_GC_UNSAFE_CODE;
		}
                done = 1;
            }
            if (concurrency>0 &&
		os_number_of_processors <= 1) {
		while(concurrency-- &&
		      !suspend_info.suspend)
		    thread_yield();
            }
        }
    }
    return done;
}

void gc_safepoint() { return; }

static inline
void gc_abandon_pending(struct thread *th)
{
    if (SymbolTlValue(GC_PENDING,th)==T &&
	(SymbolTlValue(IN_SAFEPOINT,th)!=T ||
	 SymbolTlValue(GC_SAFE,th)==T))
	SetSymbolValue(GC_PENDING,NIL,th);
}

/* Get the thread into appropriate suspend state depending on the
   current gc/interrupt action in progress. Doesn't wait (see
   gc_accept_thread_state() which does).

   Useful for threads whose state wasn't modified by a thread
   responsible for the action. Currently it's (1) a thread without
   pending interrupt signal, for interrupt's wake_the_world(), and (2)
   a thread linking into all_threads after gc_stop_the_world released
   all_threads_lock. */

static inline
void gc_suspend_willfully(struct thread *th)
{
    boolean full_suspend_possible = (SymbolTlValue(GC_INHIBIT,th)!=T);
    if (suspend_info.suspend && suspend_info.gc_thread!=th) {
	lock_suspend_info(__FILE__,__LINE__);
	if (suspend_info.suspend) {
	    switch (suspend_info.reason) {
	    case SUSPEND_REASON_GCING:
		gc_assert(full_suspend_possible);
		th->state = STATE_SUSPENDED;
		break;
	    case SUSPEND_REASON_GC:
		th->state = full_suspend_possible ?
		    STATE_SUSPENDED : STATE_SUSPENDED_BRIEFLY;
		break;
	    case SUSPEND_REASON_INTERRUPT:
		th->state = STATE_SUSPENDED_BRIEFLY;
		break;
	    case SUSPEND_REASON_NONE:
		break;
	    }
	}
	unlock_suspend_info(__FILE__,__LINE__);
    }
}

/* Get the thread into appropriate suspend state if it's in
   appropriate blocker state; return true if it left the blocker
   state.
   
   Supposed to be used either by the thread itself or by
   gc_stop_the_world() on other threads.

   Requires state_lock to be taken.
 */
static inline
boolean gc_adjust_thread_state(struct thread *th)
{
    struct thread* self = arch_os_get_current_thread();
    boolean external = (self != th);
    boolean full_suspend_possible = (SymbolTlValue(GC_INHIBIT,th)!=T);
    boolean result = 0;

    /* External state adjustment from blocker to suspend requires
       thread being in foreign call */
    if (external) {
	if (th->gc_safepoint_context == (void*)-1)
	    return 0;
	if (!th->csp_around_foreign_call)
	    return 0;
    }

    /* For X86_MEMORY_MODEL variant */
    COMPILER_BARRIER;
    if (external && (th->gc_safepoint_context == (void*)-1))
	return 0;
    COMPILER_BARRIER;

    switch (th->state) {
    case STATE_PHASE1_BLOCKER:
	result = 1;
	th->state = full_suspend_possible ?
	    STATE_SUSPENDED : STATE_SUSPENDED_BRIEFLY;
	if (full_suspend_possible) {
	    SetSymbolValue(STOP_FOR_GC_PENDING,NIL,th);
	} else {
	    SetSymbolValue(STOP_FOR_GC_PENDING,T,th);
	}
	gc_abandon_pending(th);
	break;
    case STATE_PHASE2_BLOCKER:
	result = full_suspend_possible;
	if (result) {
	    th->state = STATE_SUSPENDED;
	    SetSymbolValue(STOP_FOR_GC_PENDING,NIL,th);
	} else {
	    SetSymbolValue(STOP_FOR_GC_PENDING,T,th);
	}
	gc_abandon_pending(th);
	break;
    case STATE_INTERRUPT_BLOCKER:
	th->state = STATE_SUSPENDED_BRIEFLY;
	result = 1;
	break;
    case STATE_RUNNING:
	gc_suspend_willfully(th);
	break;
    }
    return result;
}

#if defined(LISP_FEATURE_X86)||defined(LISP_FEATURE_X86_64)
#define X86_MEMORY_MODEL
#endif


/* Applies GC wake if appropriate. */
static inline
void gc_wake_wakeable()
{
    int gcwake = 0;
    lock_suspend_info(__FILE__,__LINE__);
    gcwake = maybe_wake_gc_begin();
    unlock_suspend_info(__FILE__,__LINE__);
    if (gcwake)
	maybe_wake_gc_end(gcwake);
}


/* If thread is in suspend state, wait for its state change.
   Called in target thread.

   @arg wakep - whether to check `blockers' counter and wake GC
   enqueued on this thread

   @return whether we had to wait
*/
static inline
boolean gc_accept_thread_state(boolean wakep)
{
    struct thread* self = arch_os_get_current_thread();
    lispobj oldstate = self->state;
    boolean waitp = 0;

    switch (oldstate) {
    case STATE_SUSPENDED:
	waitp = 1;
	break;
    case STATE_SUSPENDED_BRIEFLY:
	waitp = 1;
	break;
    }
    if (waitp) {
	if (wakep) {
	    /* 1. wakep requires waitp now (only suspend states are
	       waking -- when replacing blocker states)
	       2. we unlock around GC wake to lower contention for state_lock
	       (not needed after phase 2, btw)
	    */
	    
	    pthread_mutex_unlock(self->state_lock);
	    gc_wake_wakeable();
	    pthread_mutex_lock(self->state_lock);
	}
	/* Wait while GC starts the thread.  As we unlock before
	   waking GC, it could have happened already.. */
	while (self->state == oldstate)
	    pthread_cond_wait(self->state_cond, self->state_lock);
    }
    return waitp;
}



#ifdef X86_MEMORY_MODEL


static inline void
full_serialize()
{
    __asm__ __volatile__ ( "mov $1,%%eax; cpuid"
			   : : : "%eax","%edx","%ecx","%ebx", "memory");
}

/* GC will now rely on serializing instruction to ensure that there is
   no pending writes on other CPUs. This way we make common flow of
   control inexpensive.
*/

void gc_enter_foreign_call(lispobj* csp, lispobj* pc)
{
    struct thread* self = arch_os_get_current_thread();
    boolean maybe_wake = 0;

    self->pc_around_foreign_call = pc;
    COMPILER_BARRIER;
    self->csp_around_foreign_call = csp;
    COMPILER_BARRIER;
    if (!suspend_info.suspend)
	goto finish;
    if (self->state == STATE_PHASE2_BLOCKER &&
	SymbolTlValue(GC_INHIBIT,self)==T)
	goto finish;

    pthread_mutex_lock(self->state_lock);
    self->csp_around_foreign_call = csp;
    self->pc_around_foreign_call = pc;
    maybe_wake = gc_adjust_thread_state(self);
    pthread_mutex_unlock(self->state_lock);

    if (maybe_wake)
	gc_wake_wakeable();
    
 finish:
    SetLastError(self->foreign_context_lasterror);
    /* Proceed to foreign code, while possibly being in suspended
       state (!) or phase2-blocker. Foreign code shouldn't touch memory
       movable by GC; when foreign call is exited, it should wait for
       thread state change if it's suspended (briefly or fully) 
    */
}

void gc_leave_foreign_call()
{
   struct thread* self = arch_os_get_current_thread();
   int maybe_wake = 0;

   /* Common sequence */
   self->foreign_context_lasterror = GetLastError();
   
   /* Interrupt signal pending */
   if (self->os_thread->pending_signal_set)
       goto full_locking;

   /* GC already in progress. */
   if (suspend_info.suspend)
       goto full_locking;
   
   COMPILER_BARRIER;

   /* Using ctx field as a flag. GC which is in progress won't notice,
      but gc_stop_the_world will now enqueue after this thread as
      gc-blocker. */
   
   self->gc_safepoint_context = (void*)-1;
   COMPILER_BARRIER;

   if (suspend_info.suspend)
       goto full_locking;

   COMPILER_BARRIER;
   self->csp_around_foreign_call = NULL;

   COMPILER_BARRIER;
   self->gc_safepoint_context = NULL;
   return;

 full_locking:

   self->gc_safepoint_context = NULL; /* Temporary protection removed */
   COMPILER_BARRIER;

   if (self->state == STATE_PHASE2_BLOCKER &&
       SymbolTlValue(GC_INHIBIT,self)==T) {
       /* if we run foreign call without GCing, and phase 2 is
	  enqueued after this thread, we are safe */
       self->csp_around_foreign_call = NULL;
       self->pc_around_foreign_call = NULL;
       return;
   }


   pthread_mutex_lock(self->state_lock);
   gc_accept_thread_state(gc_adjust_thread_state(self));
   /* Then unpublish context data */
   self->csp_around_foreign_call = NULL;
   self->pc_around_foreign_call = NULL;
   self->gc_safepoint_context = NULL;
   pthread_mutex_unlock(self->state_lock);
   while (check_pending_gc()||check_pending_interrupts());
}


#else

void gc_enter_foreign_call(lispobj* csp, lispobj* pc)
{
    struct thread* self = arch_os_get_current_thread();
    boolean maybe_wake = 0;

    /* Shorthand: when GC phase 2 is enqueued on this thread, there's
       no one to modify (or concurrently examine) its state.
       Typical example are foreign calls in WITHOUT-GCING body.
     */
    if (self->state == STATE_PHASE2_BLOCKER &&
	SymbolTlValue(GC_INHIBIT,self)==T) {
	self->csp_around_foreign_call = csp;
	self->pc_around_foreign_call = pc;
	goto finish;
    }
    
    pthread_mutex_lock(self->state_lock);
    self->csp_around_foreign_call = csp;
    self->pc_around_foreign_call = pc;
    maybe_wake = gc_adjust_thread_state(self);
    pthread_mutex_unlock(self->state_lock);

    if (maybe_wake)
	gc_wake_wakeable();
    
 finish:
    SetLastError(self->foreign_context_lasterror);
    /* Proceed to foreign code, while possibly being in suspended
       state (!) or phase2-blocker. Foreign code shouldn't touch memory
       movable by GC; when foreign call is exited, it should wait for
       thread state change if it's suspended (briefly or fully) 
    */
}

void gc_leave_foreign_call()
{
    struct thread* self = arch_os_get_current_thread();

    boolean waitp = 0;
    self->foreign_context_lasterror = GetLastError();

    /* shorthand: no locking (and no pending interrupt/gc check)
       when GC phase 2 is enqueued after this thread. */
    if (self->state == STATE_PHASE2_BLOCKER &&
	SymbolTlValue(GC_INHIBIT,self)==T) {
	self->csp_around_foreign_call = NULL;
	self->pc_around_foreign_call = NULL;
	return;
    }
    pthread_mutex_lock(self->state_lock);
    /* when foreing csp is published, we are never waited upon for
       something that can be satisfied on foreign call exit.
       That's why there's no gc_wake_wakeable here. */
    
    gc_accept_thread_state(0);

    /* Then unpublish context data */
    self->csp_around_foreign_call = NULL;
    self->pc_around_foreign_call = NULL;

    /* unlock is a memory barrier (release semantics) */
    pthread_mutex_unlock(self->state_lock);
    while (check_pending_gc()||check_pending_interrupts());
}

#endif

/* Should be called by a thread (possibly) trapped on GPA or
   receive-pending-interrupt.  */
void gc_maybe_stop_with_context(os_context_t *ctx, boolean gc_page_access)
{
    struct thread* self = arch_os_get_current_thread();
    int gcwake = 0;
    boolean maybe_wake = 0;
    lispobj oldstate;
    boolean pai = get_pseudo_atomic_interrupted(self);

    /* we should not be here in pseudo-atomic */
    gc_assert(!get_pseudo_atomic_atomic(self));
    /* but could be trapping on pai */
    if (get_pseudo_atomic_interrupted(self))
	clear_pseudo_atomic_interrupted(self);

    if (self->csp_around_foreign_call) {
	BEGIN_GC_UNSAFE_CODE;
	END_GC_UNSAFE_CODE;
	return;
    }


 again:
    pthread_mutex_lock(self->state_lock);
    /* possibly convert blocker state into suspend state */
    maybe_wake = gc_adjust_thread_state(self);
    /* context for conservation */
    self->gc_safepoint_context = ctx;
    /* maybe wake GC and wait for restart */
    gc_accept_thread_state(maybe_wake);
    /* no context now */
    self->gc_safepoint_context = NULL;
    pthread_mutex_unlock(self->state_lock);
    /* here our own willingness to GC (or interrupt handling) may take
       over. NB if concurrent GC was in progress, there is already no
       GC_PENDING. */
    while(check_pending_gc() || check_pending_interrupts());

    if (SymbolTlValue(STOP_FOR_GC_PENDING,self)==NIL
	&& self->state != STATE_RUNNING) {
	goto again;
    }
}

lispobj fn_by_pc(unsigned int pc)
{
  lispobj obj = (lispobj)search_read_only_space((void*)pc);
  if (!obj)
    obj = (lispobj)search_static_space((void*)pc);
  if (!obj)
    obj = (lispobj)search_dynamic_space((void*)pc);
  return obj;
}

int pc_in_lisp_code(unsigned int pc)
{
  return
    search_read_only_space((void*)pc) != NULL ||
    search_static_space((void*)pc) != NULL ||
    search_dynamic_space((void*)pc) != NULL;
}

int thread_get_pc(struct thread *th)
{
  CONTEXT ctx;
  pthread_np_get_thread_context(th->os_thread, &ctx);
  return ctx.Eip;
}

int thread_get_pc_susp(struct thread *th)
{
  CONTEXT ctx;
  pthread_np_suspend(th->os_thread);
  pthread_np_get_thread_context(th->os_thread, &ctx);
  pthread_np_resume(th->os_thread);
  return ctx.Eip;
}

int thread_in_lisp_code(struct thread *th)
{
  return pc_in_lisp_code(thread_get_pc(th));
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

void log_gc_state(const char * msg)
{
  odprintf(msg);
}

struct suspend_phase suspend_gc_phase1_nounmap;
struct suspend_phase suspend_gc_phase1_unmap;
struct suspend_phase suspend_gc_phase2;
struct suspend_phase suspend_gc_gcing;
struct suspend_phase suspend_gc_start;

struct suspend_phase suspend_gc_phase1_nounmap = {
    .suspend = 1,
    .reason = SUSPEND_REASON_GC,
    .phase = 1,
    .next = &suspend_gc_phase2,
};

struct suspend_phase suspend_gc_phase1_unmap = {
    .suspend = 1,
    .reason = SUSPEND_REASON_GC,
    .phase = 1,
    .next = &suspend_gc_phase2,
};

struct suspend_phase suspend_gc_phase2 = {
    .suspend = 1,
    .reason = SUSPEND_REASON_GC,
    .phase = 2,
    .next = &suspend_gc_gcing,
};

struct suspend_phase suspend_gc_gcing = {
    .suspend = 1,
    .reason = SUSPEND_REASON_GCING,
    .phase = 2,
    .next = &suspend_gc_start,
};

struct suspend_phase suspend_gc_start = {
    .suspend = 0,
    .reason = SUSPEND_REASON_NONE,
    .phase = 0,
    .next = NULL,
};


typedef boolean (*thread_predicate)(struct thread* candidate);

static inline boolean
thread_needs_gc_signal(struct thread *p)
{
    return
	p->state == STATE_RUNNING;
}

static inline boolean
thread_needs_interrupt_signal(struct thread *p)
{
    return
	p->state == STATE_RUNNING &&
	(p->os_thread->pending_signal_set) &&
	SymbolTlValue(GC_PENDING,p)!=T &&
	SymbolTlValue(STOP_FOR_GC_PENDING,p)!=T;
}

static inline boolean
thread_needs_gc_page_signal(struct thread *p)
{
    return
	thread_needs_gc_signal(p) &&
	SymbolTlValue(GC_SAFE,p)!=T &&
	SymbolTlValue(GC_PENDING,p)!=T &&
	SymbolTlValue(STOP_FOR_GC_PENDING,p)!=T;
}

static inline boolean
thread_blocks_gcing(struct thread *p)
{
    return
	(p->state != STATE_SUSPENDED) &&
	(p->state != STATE_DEAD);
}

static inline boolean
thread_live_other(struct thread *p)
{
    
    return
	p != arch_os_get_current_thread() &&
	p->state != STATE_DEAD &&
	p->state != STATE_RUNNING;
}

static inline boolean
move_thread_state(struct thread *p,
		  lispobj state,
		  thread_predicate pred_nolock,
		  thread_predicate pred_lock,
		  boolean signal_cond)
{
    boolean done = 0;
    if (pred_nolock && !pred_nolock(p)) {
	return 0;
    }
    pthread_mutex_lock(p->state_lock);
    if (p->state != STATE_DEAD && (!pred_lock || pred_lock(p))) {
	p->state = state;
	/* If thread is in foreign call, we may take it into suspend
	   state ourselves */

	if (!gc_adjust_thread_state(p)) {
	    /* not externally adjustable (blocker => suspend) */
	    if (signal_cond)
		pthread_cond_broadcast(p->state_cond);
	    done = 1;
	}
    }
    pthread_mutex_unlock(p->state_lock);
    /* If done, thread is in STATE_PHASE1_BLOCKER or STATE_PHASE2_BLOCKER. */
    return done;
}

static inline int pack_suspend_data(int phase, int reason, int aux)
{
    return (phase << 16)|(reason<<8)|aux;
}

static inline int suspend_getword_nolock()
{
    return pack_suspend_data(suspend_info.phase,
			     suspend_info.reason,
			     suspend_info.used_gc_page);
}

static inline void suspend_flushword_nolock(int word)
{
    if (word) {
	suspend_info.phase = (word >> 16);
	suspend_info.reason = (word >>8)&0xFF;
	suspend_info.used_gc_page = word & 0xFF;
	suspend_info.suspend = 1;
    } else {
	suspend_info.suspend = 0;
    }
}

static inline void suspend_flushword(int word)
{
    lock_suspend_info(__FILE__, __LINE__);
    suspend_flushword_nolock(word);
    unlock_suspend_info(__FILE__, __LINE__);
}

static inline void suspend_leave(int word)
{
    if ((word>>16)==1 &&
	((word>>8)&0xFF)==SUSPEND_REASON_GC &&
	((word&0xFF)==1)) {

	odxprint(safepoints,
		 "GCer %p resumed by %p, leaving %p with map_gc_page()",
		 suspend_info.gc_thread,
		 arch_os_get_current_thread(),
		 word);
	
	map_gc_page();
    }
    if ((word>>16)==1 &&
	((word>>8)&0xFF)==SUSPEND_REASON_INTERRUPT &&
	((word&0xFF)==1)) {
	odxprint(safepoints,
		 "INTR %p resumed by %p, leaving %p with map_gc_page()",
		 suspend_info.gc_thread,
		 arch_os_get_current_thread(),
		 word);
	map_gc_page();
    }
}

static inline int suspend_next(int word)
{
    if ((word>>16)==2)
	return pack_suspend_data(2, SUSPEND_REASON_GCING, word & 0xFF);
    else
	return word + (1<<16);
}

/* scenarios:
   1. no known blockers => doing suspend_leave with old state,
   lock suspend_info, doing suspend_flushword_nolock with next state
   [in gc_stop_the_world itself].

   2. known blockers => lock suspend_info, sum blockers.
   2.1. no known blockers remain
        => flush next state, unlock suspend_info, leave old state.
   2.2. known blockers remain
        => enqueue on blockers with safepoint_cycle_state().
	2.2.1. when last blocker retracts
	       => takes old state, evals next state, flushes next state,
	          ... leaves old state ... wakes GCer.
*/

static inline void gc_roll_or_wait(int known_blockers,
				   int phase, int reason, int flag)
{
    int oldword = pack_suspend_data(phase,reason,flag),
	newword = suspend_next(oldword);
    odxprint(safepoints,
	     "gc_roll_or_wait: phase=%d reason=%d flag=%d "
	     "blockers=%d packed %p next %p\n",
	     phase, reason, flag, known_blockers, oldword, newword);
    if (!known_blockers) {
	suspend_leave(oldword);	/* action (no lock) */
	suspend_flushword(newword);
    } else {
	lock_suspend_info(__FILE__, __LINE__);
	known_blockers += suspend_info.blockers;
	suspend_info.blockers = known_blockers;
	odxprint(safepoints,
		 "gc_roll_or_wait: [locked] phase=%d reason=%d flag=%d blockers=%d\n",
		 phase, reason, flag, known_blockers);
	if (!known_blockers) {
	    suspend_flushword_nolock(newword);
	    unlock_suspend_info(__FILE__, __LINE__);
	    suspend_leave(oldword);
	} else {
	    suspend_flushword_nolock(oldword);
	    safepoint_cycle_state(STATE_SUSPENDED_BRIEFLY);
	    /* last blocker => right state */
	}
    }
}

static inline int maybe_wake_gc_begin()
{
    /* 1. Expect suspend_info locked
       2. Return non-zero if it's last blocker
       3. Namely, return the oldword
       4. decrease blockers count */
    odxprint(safepoints,
	     "GCer %p Blocker %p maybe wake? \n",
	     suspend_info.gc_thread, arch_os_get_current_thread());

    if (!--suspend_info.blockers) {
	int oldword = suspend_getword_nolock();
	int newword = suspend_next(oldword);
	odxprint(safepoints,
		 "GCer %p Blocker %p decided to wake: tran %p -> %p \n",
		 suspend_info.gc_thread, arch_os_get_current_thread(), oldword, newword);
	suspend_flushword_nolock(newword);
	return oldword;
    } else {
	return 0;
    }
}

static inline void maybe_wake_gc_end(int oldword)
{
    if (oldword) {
	suspend_leave(oldword);
	set_thread_state(suspend_info.gc_thread,
			 STATE_RUNNING);
    }
}

#define X86_MEMORY_MODEL_IS_TRICKY

void gc_stop_the_world()
{
    struct thread *p, *th = arch_os_get_current_thread();
    boolean gc_page_signalling = 0, yielded = 0;
    int gc_blockers = 0;
    const boolean silently = 0, loudly = 1;
    
    pthread_mutex_lock(&suspend_info.world_lock);
    suspend_info.gc_thread = th;
    suspend_info.blockers = 0;
    suspend_flushword(pack_suspend_data(1,SUSPEND_REASON_GC,0));
    pthread_mutex_lock(&all_threads_lock);


#ifdef X86_MEMORY_MODEL_IS_TRICKY
    full_serialize();		/* Anyone sees suspend, _and_ we see
    				   anyone's memory in its full glory */
#endif

    for_each_thread(p) {
	if (p==th) continue;
	const char *oldss = dyndebug_safepoints ?
	    get_thread_state_as_string(p) : "[?]";
	if (move_thread_state(p,STATE_PHASE1_BLOCKER,
			      NULL,
			      thread_needs_gc_signal,
			      silently)) {
	    odxprint(safepoints,
		     "GCer %p Phase1-blocker %p CSP %p CTX %p State %s => %s\n",
		     th, p, p->csp_around_foreign_call, p->gc_safepoint_context,
		     oldss, get_thread_state_as_string(p));
	    ++gc_blockers;
	    gc_page_signalling = 1;
	}
    }
    pthread_mutex_unlock(&all_threads_lock);

    if (gc_page_signalling)
	unmap_gc_page();

    gc_roll_or_wait(gc_blockers, 1, SUSPEND_REASON_GC, gc_page_signalling);

    gc_blockers = 0;
    for_each_thread(p) {
	if (p==th) continue;
	void * oldctx = p->gc_safepoint_context;
	const char *oldss = dyndebug_safepoints ?
	    get_thread_state_as_string(p) : "[?]";
	
	if (move_thread_state(p,STATE_PHASE2_BLOCKER,
			      NULL,
			      thread_blocks_gcing,
			      loudly)) {
	    odxprint(safepoints,
		     "GCer %p Phase2-blocker %p CSP %p CTX %p [was %p] "
		     "INH %s State %s => %s\n",
		     th, p, p->csp_around_foreign_call, p->gc_safepoint_context,
		     oldctx, t_nil_str(SymbolValue(GC_INHIBIT,p)),
		     oldss, get_thread_state_as_string(p));
	    ++ gc_blockers;
	}
    }
    gc_roll_or_wait(gc_blockers, 2, SUSPEND_REASON_GC, gc_page_signalling);
    odxprint(safepoints,
	     "GCer %p stopped the world\n", th);
}

void gc_start_the_world()
{
    boolean loudly=1;
    int nthreads = 0, nallthreads = 0;
    struct thread *p, *th = arch_os_get_current_thread();
    odxprint(safepoints,
	     "GCer %p starting the world\n", th);
    suspend_flushword(0);
    pthread_mutex_lock(&all_threads_lock);

    for_each_thread(p) {
	++nallthreads;
	if (p->state != STATE_DEAD && p->state != STATE_RUNNING)
	    ++nthreads;
    }
    th->tls_cookie = make_fixnum(nallthreads>8 ? 7 : nallthreads-1);
    if (nthreads) {
	struct thread* wake_threads[nthreads];
	int i = 0;
	for_each_thread(p) {
	    if (p->state != STATE_DEAD && p->state != STATE_RUNNING)
		wake_threads[i++] = p;
	}
	nthreads = i;
	pthread_mutex_unlock(&all_threads_lock);
	for (i=0;i<nthreads; ++i) {
	    move_thread_state(wake_threads[i],
			      STATE_RUNNING,
			      NULL, NULL, loudly);
	} 
    } else {
	pthread_mutex_unlock(&all_threads_lock);
    }
    pthread_mutex_unlock(&suspend_info.world_lock);
    odxprint(safepoints,
	     "GCer %p started the world [ctx %p csp %p]\n", th,
	     th->gc_safepoint_context, th->csp_around_foreign_call);
}


void wake_the_world()
{
    /* We want threads with pending interrupt signal (and not
       INTERRUPT_PENDING, STOP_FOR_GC_PENDING, GC_PENDING, ...)  being
       in a non-consing Lisp loop, to acknowledge the interrupt.

    */
    struct thread *p, *th = arch_os_get_current_thread();
    boolean gc_page_signalling = 0, yielded = 0;
    int gc_blockers = 0;
    const boolean silently = 0, loudly = 1;
    
    if (pthread_mutex_trylock(&suspend_info.world_lock)) {
	if (suspend_info.suspend)
	    return;
	/* We are probably waiting on GC which wakes the world */
	pthread_mutex_lock(&suspend_info.world_lock);
    }

    suspend_info.gc_thread = th;
    suspend_info.blockers = 0;
    suspend_flushword(pack_suspend_data(1,SUSPEND_REASON_INTERRUPT,0));

#ifdef X86_MEMORY_MODEL_IS_TRICKY
    full_serialize();		/* Anyone sees suspend, _and_ we see
    				   anyone's memory in its full glory */
#endif
    
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
	if (p==th) continue;
	const char *oldss = dyndebug_safepoints ?
	    get_thread_state_as_string(p) : "[?]";
	if (move_thread_state(p,STATE_INTERRUPT_BLOCKER,
			      thread_needs_interrupt_signal,
			      thread_needs_interrupt_signal,
			      silently)) {
	    odxprint(safepoints,
		     "INTR %p Phase1-blocker %p CSP %p CTX %p State %s => %s\n",
		     th, p, p->csp_around_foreign_call, p->gc_safepoint_context,
		     oldss, get_thread_state_as_string(p));
	    ++gc_blockers;
	    gc_page_signalling = 1;
	}
    }
    pthread_mutex_unlock(&all_threads_lock);
    if (gc_page_signalling)
	unmap_gc_page();
    gc_roll_or_wait(gc_blockers, 1, SUSPEND_REASON_INTERRUPT, gc_page_signalling);
    gc_start_the_world();
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
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
            wake_thread(thread);
#endif	    
            if (status)
                lose("kill_safely: pthread_kill failed with %d\n", status);
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
