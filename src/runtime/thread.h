#if !defined(_INCLUDE_THREAD_H_)
#define _INCLUDE_THREAD_H_

#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include "sbcl.h"
#include "globals.h"
#include "runtime.h"
#include "os.h"
#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
#endif
#ifdef LISP_FEATURE_WIN32
#include "win32-thread-private-events.h"
#endif
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

#include "genesis/thread.h"
#include "genesis/fdefn.h"
#include "interrupt.h"

#define STATE_RUNNING MAKE_FIXNUM(1)
#define STATE_STOPPED MAKE_FIXNUM(2)
#define STATE_DEAD MAKE_FIXNUM(3)

#ifdef LISP_FEATURE_SB_THREAD
lispobj thread_state(struct thread *thread);
void set_thread_state(struct thread *thread, lispobj state);
void wait_for_thread_state_change(struct thread *thread, lispobj state);

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT

enum threads_suspend_reason {
    SUSPEND_REASON_NONE,
    SUSPEND_REASON_GC,
    SUSPEND_REASON_INTERRUPT,
    SUSPEND_REASON_GCING
};

struct threads_suspend_info {
    int suspend;
    pthread_mutex_t world_lock;
    pthread_mutex_t lock;
    enum threads_suspend_reason reason;
    int phase;
    struct thread * gc_thread;
    struct thread * interrupted_thread;
    int blockers;
    int used_gc_page;
};

struct suspend_phase {
    int suspend;
    enum threads_suspend_reason reason;
    int phase;
    struct suspend_phase *next;
};


extern struct threads_suspend_info suspend_info;


struct gcing_safety {
    lispobj csp_around_foreign_call;
    lispobj* pc_around_foreign_call;
};

#endif
extern pthread_key_t lisp_thread;
#endif

extern int thread_yield();

extern int kill_safely(os_thread_t os_thread, int signal);

#define THREAD_SLOT_OFFSET_WORDS(c) \
 (offsetof(struct thread,c)/(sizeof (struct thread *)))

union per_thread_data {
    struct thread thread;
    lispobj dynamic_values[1];  /* actually more like 4000 or so */
};

extern struct thread *all_threads;
extern int dynamic_values_bytes;

#if defined(LISP_FEATURE_DARWIN)
#define CONTROL_STACK_ALIGNMENT_BYTES 8192 /* darwin wants page-aligned stacks */
#define THREAD_ALIGNMENT_BYTES CONTROL_STACK_ALIGNMENT_BYTES
#else
#define THREAD_ALIGNMENT_BYTES BACKEND_PAGE_BYTES
#define CONTROL_STACK_ALIGNMENT_BYTES 16
#endif


#ifdef LISP_FEATURE_SB_THREAD
#define for_each_thread(th) for(th=all_threads;th;th=th->next)
#else
/* there's some possibility a SSC could notice this never actually
 * loops  */
#define for_each_thread(th) for(th=all_threads;th;th=0)
#endif

static inline lispobj *
SymbolValueAddress(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj *r = &(((union per_thread_data *)thread)
                       ->dynamic_values[(sym->tls_index) >> WORD_SHIFT]);
        if((*r)!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
#endif
    return &sym->value;
}

static inline lispobj
SymbolValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj r=
            ((union per_thread_data *)thread)
            ->dynamic_values[(sym->tls_index) >> WORD_SHIFT];
        if(r!=NO_TLS_VALUE_MARKER_WIDETAG) return r;
    }
#endif
    return sym->value;
}

static inline lispobj
SymbolTlValue(u64 tagged_symbol_pointer, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    return ((union per_thread_data *)thread)
        ->dynamic_values[(sym->tls_index) >> WORD_SHIFT];
#else
    return sym->value;
#endif
}

static inline void
SetSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread)
{
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
#ifdef LISP_FEATURE_SB_THREAD
    if(thread && sym->tls_index) {
        lispobj *pr= &(((union per_thread_data *)thread)
                       ->dynamic_values[(sym->tls_index) >> WORD_SHIFT]);
        if(*pr!=NO_TLS_VALUE_MARKER_WIDETAG) {
            *pr=val;
            return;
        }
    }
#endif
    sym->value = val;
}

static inline void
SetTlSymbolValue(u64 tagged_symbol_pointer,lispobj val, void *thread)
{
#ifdef LISP_FEATURE_SB_THREAD
    struct symbol *sym= (struct symbol *)
        (pointer_sized_uint_t)(tagged_symbol_pointer-OTHER_POINTER_LOWTAG);
    ((union per_thread_data *)thread)
        ->dynamic_values[(sym->tls_index) >> WORD_SHIFT]
        =val;
#else
    SetSymbolValue(tagged_symbol_pointer,val,thread) ;
#endif
}

/* This only works for static symbols. */
static inline lispobj
StaticSymbolFunction(lispobj sym)
{
    return ((struct fdefn *)native_pointer(SymbolValue(sym, 0)))->fun;
}

/* These are for use during GC, on the current thread, or on prenatal
 * threads only. */
#if defined(LISP_FEATURE_SB_THREAD)
#define get_binding_stack_pointer(thread)       \
    ((thread)->binding_stack_pointer)
#define set_binding_stack_pointer(thread,value) \
    ((thread)->binding_stack_pointer = (lispobj *)(value))
#define access_control_stack_pointer(thread) \
    ((thread)->control_stack_pointer)
#  if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
#define access_control_frame_pointer(thread) \
    ((thread)->control_frame_pointer)
#  endif
#elif defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#define get_binding_stack_pointer(thread)       \
    SymbolValue(BINDING_STACK_POINTER, thread)
#define set_binding_stack_pointer(thread,value) \
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj)(value), thread)
#define access_control_stack_pointer(thread)    \
    (current_control_stack_pointer)
#else
#define get_binding_stack_pointer(thread)       \
    (current_binding_stack_pointer)
#define set_binding_stack_pointer(thread,value) \
    (current_binding_stack_pointer = (lispobj *)(value))
#define access_control_stack_pointer(thread) \
    (current_control_stack_pointer)
#define access_control_frame_pointer(thread) \
    (current_control_frame_pointer)
#endif

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_GCC_TLS)
extern __thread struct thread *current_thread;
#endif

#if defined(LISP_FEATURE_WIN32)
static inline struct thread* arch_os_get_current_thread()
    __attribute__((__const__));
#endif
/* This is clearly per-arch and possibly even per-OS code, but we can't
 * put it somewhere sensible like x86-linux-os.c because it needs too
 * much stuff like struct thread and all_threads to be defined, which
 * usually aren't by that time.  So, it's here instead.  Sorry */

static inline struct thread *arch_os_get_current_thread(void)
{
#if defined(LISP_FEATURE_SB_THREAD)
#if defined(LISP_FEATURE_X86)
    register struct thread *me=0;
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
    __asm__ ("movl %%fs:0xE10+(4*63), %0" : "=r"(me) :);
    return me;
#endif
    if(all_threads) {
#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_RESTORE_FS_SEGMENT_REGISTER_FROM_TLS)
        sel_t sel;
        struct thread *th = pthread_getspecific(specials);
        sel.index = th->tls_cookie;
        sel.rpl = USER_PRIV;
        sel.ti = SEL_LDT;
        __asm__ __volatile__ ("movw %w0, %%fs" : : "r"(sel));
#elif defined(LISP_FEATURE_FREEBSD)
#ifdef LISP_FEATURE_GCC_TLS
        struct thread *th = current_thread;
#else
        struct thread *th = pthread_getspecific(specials);
#endif
#ifdef LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_TLS
        unsigned int sel = LSEL(th->tls_cookie, SEL_UPL);
        unsigned int fs = rfs();

        /* Load FS only if it's necessary.  Modifying a selector
         * causes privilege checking and it takes long time. */
        if (fs != sel)
            load_fs(sel);
#endif
        return th;
#endif
        __asm__ ("movl %%fs:%c1,%0" : "=r" (me)
                 : "i" (offsetof (struct thread,this)));
    }
    return me;
#else
#ifdef LISP_FEATURE_GCC_TLS
    return current_thread;
#else
    return pthread_getspecific(specials);
#endif
#endif /* x86 */
#else
     return all_threads;
#endif
}

#ifdef LISP_FEATURE_SB_GC_SAFEPOINT
void gc_safepoint();

void wake_the_world();
void gc_enter_foreign_call(lispobj* csp, lispobj* pc);
void gc_leave_foreign_call();
void gc_maybe_stop_with_context(os_context_t *ctx, boolean gc_page_access);

/* New generation */
void thread_in_safety_transition(os_context_t *ctx);
void thread_in_lisp_raised(os_context_t *ctx);
void thread_interrupted(os_context_t *ctx);



static inline
void push_gcing_safety(struct gcing_safety *into)
{
    struct thread* th = arch_os_get_current_thread();
    asm volatile ("");
    if ((into->csp_around_foreign_call =
         *th->csp_around_foreign_call)) {
        *th->csp_around_foreign_call = 0;
        asm volatile ("");
        into->pc_around_foreign_call = th->pc_around_foreign_call;
        th->pc_around_foreign_call = 0;
        asm volatile ("");
    } else {
        into->pc_around_foreign_call = 0;
    }
}

static inline
void pop_gcing_safety(struct gcing_safety *from)
{
    struct thread* th = arch_os_get_current_thread();
    if (from->csp_around_foreign_call) {
        asm volatile ("");
        *th->csp_around_foreign_call = from->csp_around_foreign_call;
        asm volatile ("");
        th->pc_around_foreign_call = from->pc_around_foreign_call;
        asm volatile ("");
    }
}

#define BEGIN_GC_UNSAFE_CODE                    \
    { struct gcing_safety sbcl__gc_safety;      \
    push_gcing_safety(&sbcl__gc_safety);

#define END_GC_UNSAFE_CODE                      \
    pop_gcing_safety(&sbcl__gc_safety);         \
    }

#endif


#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
extern kern_return_t mach_lisp_thread_init(struct thread *thread);
extern kern_return_t mach_lisp_thread_destroy(struct thread *thread);
#endif

extern boolean is_some_thread_local_addr(os_vm_address_t addr);
extern void create_initial_thread(lispobj);
extern void thread_register_gc_trigger();

#endif /* _INCLUDE_THREAD_H_ */
