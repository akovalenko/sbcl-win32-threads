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

#ifndef LISP_FEATURE_SB_GC_SAFEPOINT
/* Only access thread state with blockables blocked. */
lispobj
thread_state(struct thread *thread)
{
    lispobj state;
    sigset_t old;
    block_blockable_signals(NULL, &old);
    os_sem_wait(thread->state_sem, "thread_state");
    state = thread->state;
    os_sem_post(thread->state_sem, "thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return state;
}

void
set_thread_state(struct thread *thread, lispobj state)
{
    int i, waitcount = 0;
    sigset_t old;
    block_blockable_signals(NULL, &old);
    os_sem_wait(thread->state_sem, "set_thread_state");
    if (thread->state != state) {
        if ((STATE_STOPPED==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_running_waitcount;
            thread->state_not_running_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_running_sem, "set_thread_state (not running)");
        }
        if ((STATE_RUNNING==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_stopped_waitcount;
            thread->state_not_stopped_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_stopped_sem, "set_thread_state (not stopped)");
        }
        thread->state = state;
    }
    os_sem_post(thread->state_sem, "set_thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
}

void
wait_for_thread_state_change(struct thread *thread, lispobj state)
{
    sigset_t old;
    os_sem_t *wait_sem;
    block_blockable_signals(NULL, &old);
  start:
    os_sem_wait(thread->state_sem, "wait_for_thread_state_change");
    if (thread->state == state) {
        switch (state) {
        case STATE_RUNNING:
            wait_sem = thread->state_not_running_sem;
            thread->state_not_running_waitcount++;
            break;
        case STATE_STOPPED:
            wait_sem = thread->state_not_stopped_sem;
            thread->state_not_stopped_waitcount++;
            break;
        default:
            lose("Invalid state in wait_for_thread_state_change: "OBJ_FMTX"\n", state);
        }
    } else {
        wait_sem = NULL;
    }
    os_sem_post(thread->state_sem, "wait_for_thread_state_change");
    if (wait_sem) {
        os_sem_wait(wait_sem, "wait_for_thread_state_change");
        goto start;
    }
    thread_sigmask(SIG_SETMASK, &old, NULL);
}
#endif /* !safepoint */
#endif

static int run_lisp_function(lispobj function)
{
#if defined(LISP_FEATURE_X86) ||                        \
    (defined(LISP_FEATURE_X86_64)                       \
     && !defined(LISP_FEATURE_WIN32))

    static int first_time = 1;
    if (first_time) {
        lispobj *args = NULL;
        first_time = 0;
        #if defined(LISP_FEATURE_WIN32)
        arch_os_get_current_thread()->control_stack_end =
            __builtin_frame_address(0);
        #endif
        return call_into_lisp_first_time(function,args,0);
    }
#endif
    return funcall0(function);
}

static int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
    unsigned int i;
    lispobj *dynamic_values = (void*)th;
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
    /* Win32 is too active in doing something strange to thread's stack
       memory. Maybe there is no way to implement _our_ guard pages for control
       stack on that platorm, after all.

       (don't forget) _resetstkoflw -- call when */
    protect_control_stack_hard_guard_page(1, NULL);
    protect_control_stack_guard_page(1, NULL);
#endif /* LISP_FEATURE_WIN32 */
    protect_binding_stack_hard_guard_page(1, NULL);
    protect_alien_stack_hard_guard_page(1, NULL);
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);

    return run_lisp_function(function);
}

#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
#define THREAD_CSP_PAGE_SIZE BACKEND_PAGE_BYTES
#else
#define THREAD_CSP_PAGE_SIZE 0
#endif
#define THREAD_STATE_LOCK_SIZE \
    ((sizeof(os_sem_t))+(sizeof(os_sem_t))+(sizeof(os_sem_t)))
#else
#define THREAD_STATE_LOCK_SIZE 0
#define THREAD_CSP_PAGE_SIZE 0
#endif


#define THREAD_STRUCT_SIZE (thread_control_stack_size + BINDING_STACK_SIZE + \
                            ALIEN_STACK_SIZE +                          \
                            THREAD_STATE_LOCK_SIZE +                    \
                            dynamic_values_bytes +                      \
                            32 * SIGSTKSZ +                             \
                            THREAD_ALIGNMENT_BYTES +                    \
                            THREAD_CSP_PAGE_SIZE)

#define FIRST_TLS_INDEX                                                 \
    (ALIGN_UP(MAX_INTERRUPTS+                                           \
              (sizeof(struct thread)/sizeof(lispobj)), 1024)            \
     -                                                                  \
     (THREAD_STATE_LOCK_SIZE)/sizeof(lispobj))                          \



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


typedef enum {
    GC_NONE=0,
    GC_FLIGHT,
    GC_MESSAGE,
    GC_INVOKED,
    GC_QUIET,
    GC_SETTLED,
    GC_COLLECT,
    GC_NPHASES
}  gc_phase_t;

/* Planned state progressions:
 * 
 * none -> flight: unmap_gc_page(). No blockers (GC_NONE can be left at any
 * moment).
 * 
 * flight -> message: happens when a master thread enters its trap. The only
 * blocker for flight mode is the master thread itself (GC_FLIGHT can't be left
 * until the master thread traps).
 *
 * message -> invoked: happens after each (other) thread is notified, i.e. it
 * will eventually stop (already stopped). map_gc_page(). Each thread with empty
 * CSP disagrees to leave GC_MESSAGE phase. 
 *
 * invoked -> collect: happens when every gc-inhibitor comes to completion
 * (that's normally pending interrupt trap). NB gc_stop_the_world, if it happens
 * in non-master thread, "takes over" as a master, also deregistering itself as
 * a blocker (i.e. it's ready to leave GC_INVOKED, but now it objects to leaving
 * GC_COLLECT; this "usurpation" doesn't require any change to GC_COLLECT
 * counter: for the counter, it's immaterial _which_ thread is waiting).
 * 
 * collect -> none: happens at gc_start_the_world (that should always happen in
 * the master). Any thread waiting until GC end now continues. */

struct gc_state {
    /* Flag: conditions are initialized */
    boolean initialized;

    /* Per-process lock for gc_state */
    pthread_mutex_t lock;

    /* Conditions: one per phase */
    pthread_cond_t phase_cond[GC_NPHASES];
    
    /* For each [current or future] phase, a number of threads not yet ready to
     * leave it */
    int phase_wait[GC_NPHASES];

    /* Master thread controlling the topmost stop/gc/start sequence */
    struct thread* master;
    struct thread* collector;

    /* Current GC phase */
    gc_phase_t phase;
};

static struct gc_state gc_state = {
    .lock = PTHREAD_MUTEX_INITIALIZER,
    .phase = GC_NONE,
};


/* set_thread_csp_access -- alter page permissions for not-in-Lisp
   flag (Lisp Stack Top) of the thread `p'. The flag may be modified
   if `writable' is true.

   Return true if there is a non-null value in the flag.

   When a thread enters C code or leaves it, a per-thread location is
   modified. That machine word serves as a not-in-Lisp flag; for
   convenience, when in C, it's filled with a topmost stack location
   that may contain Lisp data. When thread is in Lisp, the word
   contains NULL.

   GENCGC uses each thread's flag value for conservative garbage collection.

   There is a full VM page reserved for this word; page permissions
   are switched to read-only for race-free examine + wait + use
   scenarios. */

static inline boolean set_thread_csp_access(struct thread* p, boolean writable)
{
    os_protect(p->csp_around_foreign_call,sizeof(lispobj),
               writable? (OS_VM_PROT_READ|OS_VM_PROT_WRITE)
               : (OS_VM_PROT_READ));
    return !!*p->csp_around_foreign_call;
}

static inline void gc_state_lock()
{
    odxprint(safepoints,"GC state [%p] to be locked",gc_state.lock);
    gc_assert(0==pthread_mutex_lock(&gc_state.lock));
    if (gc_state.master) {
        fprintf(stderr,"GC state lock glitch [%p] in thread %p phase %d\n",
                gc_state.master,arch_os_get_current_thread(),gc_state.phase);
        odxprint(safepoints,"GC state lock glitch [%p]",gc_state.master);
    }
    gc_assert(!gc_state.master);
    gc_state.master = arch_os_get_current_thread();
    if (!gc_state.initialized) {
        int i;
        for (i=GC_NONE; i<GC_NPHASES; ++i)
            pthread_cond_init(&gc_state.phase_cond[i],NULL);
        gc_state.initialized = 1;
    }
    odxprint(safepoints,"GC state [%p] locked in phase %d",gc_state.lock, gc_state.phase);
}

static inline void gc_state_unlock()
{
    odxprint(safepoints,"GC state to be unlocked in phase %d",gc_state.phase);
    gc_assert(arch_os_get_current_thread()==gc_state.master);
    gc_state.master = NULL;
    gc_assert(0==pthread_mutex_unlock(&gc_state.lock));
    odxprint(safepoints,"%s","GC state unlocked");
}

static inline void gc_state_wait(gc_phase_t phase)
{
    struct thread* self = arch_os_get_current_thread();
    odxprint(safepoints,"Waiting for %d -> %d [%d holders]",
             gc_state.phase,phase,gc_state.phase_wait[gc_state.phase]);
    gc_assert(gc_state.master == self);
    gc_state.master = NULL;
    while(gc_state.phase != phase && !(phase == GC_QUIET && (gc_state.phase > GC_QUIET)))
        pthread_cond_wait(&gc_state.phase_cond[phase],&gc_state.lock);
    gc_assert(gc_state.master == NULL);
    gc_state.master = self;
}

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
    int i,j;
#if defined(LISP_FEATURE_WIN32)
    struct lisp_exception_frame exception_frame;
    wos_install_interrupt_handlers(&exception_frame);
#endif
    FSHOW((stderr,"/creating thread %lu\n", thread_self()));
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
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_guard_page(1, NULL);
#endif
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);
    /* Since GC can only know about this thread from the all_threads
     * list and we're just adding this thread to it, there is no
     * danger of deadlocking even with SIG_STOP_FOR_GC blocked (which
     * it is not). */

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    *th->csp_around_foreign_call = (lispobj)&function;
    odxprint(safepoints, "New thread to be linked: %p\n", th);
#endif
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    link_thread(th);
    
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    
    odxprint(safepoints, "...Linked: %p\n", th);
    gc_assert(lock_ret == 0);

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
    gc_state_lock();
    gc_state_wait(GC_NONE);
    gc_state_unlock();
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
#endif  /* safepoints */

    if(th->tls_cookie>=0) arch_os_thread_cleanup(th);
#ifndef LISP_FEATURE_SB_GC_SAFEPOINT
    os_sem_destroy(th->state_sem);
    os_sem_destroy(th->state_not_running_sem);
    os_sem_destroy(th->state_not_stopped_sem);
#endif

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
    mach_port_move_member(current_mach_task,
                          THREAD_STRUCT_TO_EXCEPTION_PORT(th),
                          MACH_PORT_NULL);
    mach_port_deallocate(current_mach_task,
                         THREAD_STRUCT_TO_EXCEPTION_PORT(th));
    mach_port_destroy(current_mach_task,
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

#ifdef LISP_FEATURE_SB_THREAD
/* FIXME: should be MAX_INTERRUPTS -1 ? */
const unsigned int tls_index_start = FIRST_TLS_INDEX;

/* test if an address is within thread-local space */
boolean is_thread_local_addr(struct thread* th, os_vm_address_t addr)
{
    ptrdiff_t diff = ((char*)th->os_address)-(char*)addr;
    return diff > (ptrdiff_t)0 && diff < (ptrdiff_t)THREAD_STRUCT_SIZE;
}

boolean is_some_thread_local_addr(os_vm_address_t addr)
{
    boolean result = 0;
    struct thread *th;
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(th) {
        if(is_thread_local_addr(th,addr)) {
            result = 1;
            break;
        }
        }
    pthread_mutex_unlock(&all_threads_lock);
    return result;
}

#endif

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
            SetSymbolValue(FREE_TLS_INDEX,tls_index_start << WORD_SHIFT,0);
            SetSymbolValue(TLS_INDEX_LOCK,make_fixnum(0),0);
        }
#define STATIC_TLS_INIT(sym,field) \
  ((struct symbol *)(sym-OTHER_POINTER_LOWTAG))->tls_index= \
  (THREAD_SLOT_OFFSET_WORDS(field) << WORD_SHIFT)

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
#ifndef LISP_FEATURE_SB_GC_SAFEPOINT
    th->state_sem=(os_sem_t *)((void *)th->alien_stack_start + ALIEN_STACK_SIZE);
    th->state_not_running_sem=(os_sem_t *)
        ((void *)th->state_sem + (sizeof(os_sem_t)));
    th->state_not_stopped_sem=(os_sem_t *)
        ((void *)th->state_not_running_sem + (sizeof(os_sem_t)));
    th->state_not_running_waitcount = 0;
    th->state_not_stopped_waitcount = 0;
    os_sem_init(th->state_sem, 1);
    os_sem_init(th->state_not_running_sem, 0);
    os_sem_init(th->state_not_stopped_sem, 0);
#endif
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

static inline gc_phase_t gc_phase_next(gc_phase_t old) {
    return (old+1) % GC_NPHASES;
}

static inline gc_phase_t thread_gc_phase(struct thread* p)
{
    boolean inhibit = (SymbolTlValue(GC_INHIBIT,p)==T)||
        (SymbolTlValue(IN_WITHOUT_GCING,p)==IN_WITHOUT_GCING);
    
    boolean inprogress =
        (SymbolTlValue(GC_PENDING,p)!=T&& SymbolTlValue(GC_PENDING,p)!=NIL);

    return
        inprogress ? (gc_state.collector && (gc_state.collector != p)
                      ? GC_NONE : GC_QUIET)
        : (inhibit ? GC_INVOKED : GC_NONE);
}

static inline void thread_gc_promote(struct thread* p, gc_phase_t cur, gc_phase_t old) {
    if (old != GC_NONE)
        gc_state.phase_wait[old]--;
    if (cur != GC_NONE) {
        gc_state.phase_wait[cur]++;
    }
    if (cur != GC_NONE)
        SetTlSymbolValue(STOP_FOR_GC_PENDING,T,p);
}

static inline void gc_notify_early()
{
    struct thread *self = arch_os_get_current_thread(), *p;
    gc_phase_t phase;
    odxprint(safepoints,"%s","global notification");
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (p==self)
            continue;
        odxprint(safepoints,"notifying thread %p csp %p",p,*p->csp_around_foreign_call);
        if (!set_thread_csp_access(p,0)) {
            thread_gc_promote(p, gc_state.phase, GC_NONE);
        } else {
            thread_gc_promote(p, thread_gc_phase(p), GC_NONE);
        }
    }
    pthread_mutex_unlock(&all_threads_lock);
}

static inline void gc_notify_final()
{
    struct thread *self = arch_os_get_current_thread(), *p;
    gc_phase_t phase;
    odxprint(safepoints,"%s","global notification");
    gc_state.phase_wait[gc_state.phase]=0;
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (p == gc_state.collector)
            continue;
        odxprint(safepoints,"notifying thread %p csp %p",p,*p->csp_around_foreign_call);
        if (!set_thread_csp_access(p,0)) {
            thread_gc_promote(p, gc_state.phase, GC_NONE);
        }
    }
    pthread_mutex_unlock(&all_threads_lock);
}

static inline void gc_done()
{
    struct thread *self = arch_os_get_current_thread(), *p;
    boolean inhibit = (SymbolTlValue(GC_INHIBIT,self)==T);

    odxprint(safepoints,"%s","global denotification");
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (inhibit && (SymbolTlValue(GC_PENDING,p)==T))
            SetTlSymbolValue(GC_PENDING,NIL,p);
        set_thread_csp_access(p,1);
    }
    pthread_mutex_unlock(&all_threads_lock);
}

static inline void gc_handle_phase()
{
    odxprint(safepoints,"Entering phase %d",gc_state.phase);
    switch (gc_state.phase) {
    case GC_FLIGHT:
        unmap_gc_page();
        break;
    case GC_MESSAGE:
        gc_notify_early();
        break;
    case GC_INVOKED:
        map_gc_page();
        break;
    case GC_SETTLED:
        gc_notify_final();
        unmap_gc_page();
        break;
    case GC_COLLECT:
        map_gc_page();
        break;
    case GC_NONE:
        gc_done();
        break;
    default:
        break;
    }
}


/* become ready to leave the <old> phase, but unready to leave the <new> phase;
 * `old' can be GC_NONE, it means this thread weren't blocking any state.  `cur'
 * can be GC_NONE, it means this thread wouldn't block GC_NONE, but still wait
 * for it. */
static inline void gc_advance(gc_phase_t cur, gc_phase_t old) {
    odxprint(safepoints,"GC advance request %d -> %d in phase %d",old,cur,gc_state.phase);
    if (cur == old)
        return;
    if (cur == gc_state.phase)
        return;
    if (old < gc_state.phase)
        old = GC_NONE;
    if (old != GC_NONE) {
        gc_state.phase_wait[old]--;
        odxprint(safepoints,"%d holders of phase %d without me",gc_state.phase_wait[old],old);
    }
    if (cur != GC_NONE) {
        gc_state.phase_wait[cur]++;
        odxprint(safepoints,"%d holders of phase %d with me",gc_state.phase_wait[cur],cur);
    }
    /* roll forth as long as there's no waiters */
    while (gc_state.phase_wait[gc_state.phase]==0
           && gc_state.phase != cur) {
        gc_state.phase = gc_phase_next(gc_state.phase);
        odxprint(safepoints,"no blockers, direct advance to %d",gc_state.phase);
        gc_handle_phase();
        pthread_cond_broadcast(&gc_state.phase_cond[gc_state.phase]);
    }
    odxprint(safepoints,"going to wait for %d threads",gc_state.phase_wait[gc_state.phase]);
    gc_state_wait(cur);
}

void thread_register_gc_trigger()
{
    odxprint(safepoints,"%s","GC triggered");
    gc_state_lock();
    if (gc_state.phase == GC_NONE) {
        gc_advance(GC_FLIGHT,GC_NONE);
    }
    gc_state_unlock();
}

void thread_in_lisp_raised(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();
    gc_phase_t phase;
    odxprint(safepoints,"%s","thread_in_lisp_raised");
    gc_state_lock();

    if (gc_state.phase == GC_FLIGHT &&
        SymbolTlValue(GC_PENDING,self)==T &&
        SymbolTlValue(GC_INHIBIT,self)!=T &&
        SymbolTlValue(IN_SAFEPOINT,self)!=T &&
        thread_gc_phase(self)==GC_NONE) {
        *self->csp_around_foreign_call = (word_t)ctxptr;
        gc_advance(GC_QUIET,GC_FLIGHT);
        set_thread_csp_access(self,1);
        if (gc_state.collector) {
            gc_advance(GC_NONE,GC_QUIET);
        } else {
            *self->csp_around_foreign_call = 0;
            SetTlSymbolValue(GC_PENDING,T,self);
        }
        gc_state_unlock();
        check_pending_gc();
        while(check_pending_interrupts(ctxptr));
        return;
    }
    if (gc_state.phase == GC_FLIGHT) {
        gc_state_wait(GC_MESSAGE);
    }
    phase = thread_gc_phase(self);
    if (phase == GC_NONE) {
        SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
        set_thread_csp_access(self,1);
        *self->csp_around_foreign_call = (word_t)ctxptr;
        if (gc_state.phase <= GC_SETTLED)
            gc_advance(phase,gc_state.phase);
        else
            gc_state_wait(phase);
        *self->csp_around_foreign_call = 0;
        gc_state_unlock();
        check_pending_gc();
        while(check_pending_interrupts(ctxptr));
    } else {
        gc_advance(phase,gc_state.phase);
        SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
        gc_state_unlock();
    }
}

void thread_in_safety_transition(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();

    odxprint(safepoints,"%s","GC safety transition");
    gc_state_lock();
    if (set_thread_csp_access(self,1)) {
        gc_state_wait(thread_gc_phase(self));
        gc_state_unlock();
        while(check_pending_interrupts(ctxptr));
    } else {
        gc_phase_t phase = thread_gc_phase(self);
        if (phase == GC_NONE) {
            SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
            *self->csp_around_foreign_call = (word_t)ctxptr;
            if (gc_state.phase <= GC_SETTLED)
                gc_advance(phase,gc_state.phase);
            else
                gc_state_wait(phase);
            *self->csp_around_foreign_call = 0;
        } else {
            gc_advance(phase,gc_state.phase);
            if (phase == GC_INVOKED)
                SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
        }
        gc_state_unlock();
    }
}

void thread_interrupted(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();

    odxprint(safepoints,"%s","pending interrupt trap");
    gc_state_lock();
    if (gc_state.phase != GC_NONE) {
        if (set_thread_csp_access(self,1)) {
            gc_state_unlock();
            thread_in_safety_transition(ctxptr);
        } else {
            gc_state_unlock();
            thread_in_lisp_raised(ctxptr);
        }
    } else {
        gc_state_unlock();
    }
    check_pending_gc();
    while(check_pending_interrupts(ctxptr));
}

void gc_stop_the_world()
{
    struct thread *self = arch_os_get_current_thread();
    odxprint(safepoints,"%s","stop the world");
    gc_state_lock();
    gc_state.collector = self;
    gc_state.phase_wait[GC_QUIET]++;
    
    switch(gc_state.phase) {
    case GC_NONE:
        gc_advance(GC_QUIET,gc_state.phase);
    case GC_FLIGHT:
    case GC_MESSAGE:
    case GC_INVOKED:
        gc_state_wait(GC_QUIET);
    case GC_QUIET:
        gc_state.phase_wait[GC_QUIET]=1;
        gc_advance(GC_COLLECT,GC_QUIET);
        break;
    case GC_COLLECT:
        break;
    default:
        lose("Stopping the world in unexpected state %d",gc_state.phase);
        break;
    }
    set_thread_csp_access(self,1);
    gc_state_unlock();
    SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
}

void gc_start_the_world()
{
    odxprint(safepoints,"%s","start the world");
    gc_state_lock();
    gc_state.collector = NULL;
    SetTlSymbolValue(IN_WITHOUT_GCING,IN_WITHOUT_GCING,
                     arch_os_get_current_thread());
    gc_advance(GC_NONE,GC_COLLECT);
    gc_state_unlock();
}


/* wake_thread(thread) -- ensure an interrupt delivery to
   `thread'. */
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

    wake_thread_io(thread);
    pthread_mutex_unlock(&all_threads_lock);

    gc_state_lock();
    if (gc_state.phase == GC_NONE) {
        gc_advance(GC_INVOKED,GC_NONE);
        gc_advance(GC_NONE,GC_INVOKED);
    }
    gc_state_unlock();

    pthread_mutex_lock(&all_threads_lock);
    return;
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
                if(state != STATE_STOPPED) {
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

#endif  /* another LISP_FEATURE_SB_GC_SAFEPOINT ifdeffery. FIXME CLEANUP. */


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
#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
          check_pending_interrupts(NULL);
#endif
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
