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
#include <float.h>

#include <excpt.h>

#include "validate.h"
#include "thread.h"
#include "cpputil.h"

size_t os_vm_page_size;

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

int dyndebug_lazy_fpu = 0;
int dyndebug_lazy_fpu_careful = 0;
int dyndebug_skip_averlax = 0;
int dyndebug_survive_aver = 0;
int dyndebug_runtime_link = 0;
int dyndebug_safepoints = 0;

int dyndebug_to_filestream = 1;
int dyndebug_to_odstring = 0;

unsigned int dyndebug_charge_count = 0;
FILE* dyndebug_output = NULL;
#define this_thread (arch_os_get_current_thread())

static inline
void dyndebug_init()
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
}

/* wrappers for winapi calls that must be successful */
static inline
void* win_aver(void* value, char* comment, char* file, int line,
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

static inline
long sys_aver(long value, char* comment, char* file, int line,
              int justwarn)
{
    win_aver((void*)(value>=0),comment,file,line,justwarn);
    return value;
}

#define AVER(call)                                                      \
    ({ __typeof__(call) __attribute__((unused)) me =                    \
            (__typeof__(call))                                          \
            win_aver((void*)(call), #call, __FILE__, __LINE__, 0);      \
        me;})

#define AVERLAX(call)                                                   \
    ({ __typeof__(call) __attribute__((unused)) me =                    \
            (__typeof__(call))                                          \
            win_aver((void*)(call), #call, __FILE__, __LINE__, 1);      \
        me;})

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

#define CRT_AVER(booly)                                         \
    ({ __typeof__(booly) __attribute__((unused)) me = (booly);  \
        sys_aver((booly)?0:-1, #booly, __FILE__, __LINE__, 0);  \
        me;})

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

void odprintf_(const char * fmt, ...)
{
    char buf[1024];
    va_list args;
    int n;
    DWORD lastError = GetLastError();
    struct thread * self = arch_os_get_current_thread();
#if defined(LISP_FEATURE_SB_THREAD)
    if (self) {
        sprintf(buf, "[0x%p] %s, %s, %s, %s ", pthread_self(), t_nil_s(GC_SAFE), t_nil_s(GC_INHIBIT), t_nil_s(INTERRUPTS_ENABLED), t_nil_s(IN_SAFEPOINT));
    } else {
        sprintf(buf, "[0x%p] (arch_os_get_current_thread() is NULL) ", pthread_self());
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
    if (dyndebug_to_filestream)
	fprintf(dyndebug_output,"%s",buf);
    SetLastError(lastError);
}



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

void *UL_GetCurrentTeb() { return NtCurrentTeb(); };
void *UL_GetCurrentFrame() { return __builtin_frame_address(0); }

void *base_seh_frame;
void *real_uwp_seh_handler;

os_vm_address_t core_mmap_end;

static void *get_seh_frame(void)
{
    void* retval;
    asm volatile ("movl %%fs:0,%0": "=r" (retval));
    return retval;
}

static void set_seh_frame(void *frame)
{
    asm volatile ("movl %0,%%fs:0": : "r" (frame));
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
void alloc_gc_page()
{
    AVER(VirtualAlloc(GC_SAFEPOINT_PAGE_ADDR, 4,
                      MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE));
}

void map_gc_page()
{
    DWORD oldProt;
    AVER(VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, 4,
                        PAGE_READWRITE, &oldProt));
}

void unmap_gc_page()
{
    DWORD oldProt;
    AVER(VirtualProtect(GC_SAFEPOINT_PAGE_ADDR, 4,
                        PAGE_NOACCESS, &oldProt));
}

#endif

#if defined(LISP_FEATURE_SB_DYNAMIC_CORE)

u32 os_get_build_time_shared_libraries(u32 excl_maximum,
                                       void* opt_root,
                                       void** opt_store_handles,
                                       const char *opt_store_names[])
{
    void* base = opt_root ? opt_root : (void*)GetModuleHandle(NULL);
    void* base_magic_location =
        base + ((IMAGE_DOS_HEADER*)base)->e_lfanew;
    void* check_duplicates[excl_maximum];

    if ((*(u32*)base_magic_location)!=0x4550) {
        /* We don't need this DLL thingie _that_ much. If the world
         * has changed to a degree where PE magic isn't found, let's
         * silently return `no libraries detected'. */
        return 0;
    } else {
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
            MEMORY_BASIC_INFORMATION moduleMapping;
            if (dyndebug_runtime_link) {
                fprintf(dyndebug_output,"Now should know DLL: %s\n",
                        (char*)(base + image_import_descriptor->Name));
            }
            if (VirtualQuery
                (*(LPCVOID**)
                 ((LPCVOID)base + image_import_descriptor->FirstThunk),
                 &moduleMapping, sizeof (moduleMapping)))
            {
                for (j=0; j<nlibrary; ++j)
                {
                    if(check_duplicates[j] == moduleMapping.AllocationBase)
                        break;
                }
                if (j<nlibrary) continue;

                check_duplicates[nlibrary] = moduleMapping.AllocationBase;
                if (opt_store_handles) {
                    opt_store_handles[nlibrary] = moduleMapping.AllocationBase;
                }
                if (opt_store_names) {
                    opt_store_names[nlibrary] = (const char *)
                        (base + image_import_descriptor->Name);
                }
                if (dyndebug_runtime_link) {
                    fprintf(stderr,"DLL detection: %u, base %p: %s\n",
                            nlibrary, moduleMapping.AllocationBase,
                            (char*)(base + image_import_descriptor->Name));
                }
                ++ nlibrary;
            }
        }
        return nlibrary;
    }
}


#define FOLLOW(obj,lowtagtype,ctype)            \
    (*(struct ctype*)(obj - lowtagtype##_LOWTAG))

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
void os_link_runtime()
{
    lispobj head;
    void *link_target = (void*)(intptr_t)LINKAGE_TABLE_SPACE_START;
    u32 nsymbol;
    lispobj symbol_name;
    char *namechars;
    char namecopy[256];
    u32 namelength;
    char *at;
    void *myself = GetModuleHandleW(NULL);
    HMODULE buildTimeImages[16] = {myself};
    boolean datap;
    u32 i;
    /* Somewhat arbitrary (and DLL topic itself is dirty on
       pre-WinXP).  What I want is to get module handles for the
       libraries that _are_ already loaded because of our build-time
       dependencies. What I don't what is to list some preselected set
       of libraries. */
    u32 nlibraries =
        1 + os_get_build_time_shared_libraries(15u, myself,
                                               1+(void**)buildTimeImages,
					       NULL);

    if (dyndebug_runtime_link) {
        for (i=0; i<nlibraries; ++i) {
            fprintf(dyndebug_output,"Got library %u/%u with base %p\n",
                    i,nlibraries, buildTimeImages[i]);
        }
    }

    for (head = SymbolValue(REQUIRED_RUNTIME_C_SYMBOLS,0);
         head!=NIL; head = cdr(head))
    {
        int linked = 0;
	lispobj item = car(head);

	if (PTR_IS_ALIGNED(link_target,os_vm_page_size)) {
	    os_validate_recommit(link_target,os_vm_page_size);
	}
        symbol_name = car(item);
	datap = (NIL!=(cdr(item)));
	
        namechars = (void*)(intptr_t)FOTHERPTR(symbol_name,vector).data;
        namelength = fixnum_value(FOTHERPTR(symbol_name,vector).length);
        AVERLAX(namelength<(sizeof(namecopy))) ||
            (namelength = (sizeof(namecopy))-1);
        memcpy(namecopy, namechars, namelength);
        namecopy[namelength] = 0;

        /* no stdcall mangling when ll/gpa */
        if ((at = strrchr(namecopy,'@'))) { (*at) = 0; }

        for (i=0u; i<nlibraries; ++i) {
            void* result = GetProcAddress(buildTimeImages[i], namecopy);
            if (result) {
                if (dyndebug_runtime_link) {
                    fprintf(dyndebug_output,
                            "Core requests linking [0x%p] <= [%s]: => [0x%p]\n",
                            link_target, namecopy, result);
                }
		if (datap) 
		    arch_write_linkage_table_ref(link_target,result);
		else
		    arch_write_linkage_table_jmp(link_target,result);
                break;
            }
        }
        if (i == nlibraries)
        {
            if (dyndebug_runtime_link) {
                fprintf(stderr,"Core requests linking [0x%p] <= [%s]: failed\n",
			link_target, namecopy);
            }
	    if (datap)
		arch_write_linkage_table_ref(link_target,undefined_alien_address);
	    else
		arch_write_linkage_table_jmp(link_target,undefined_alien_function);
		
        }
	link_target = (void*)(((uintptr_t)link_target)+LINKAGE_TABLE_ENTRY_SIZE);
    }
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

static pthread_mutex_t fiber_dead_mutex = PTHREAD_MUTEX_INITIALIZER;

/* Event handle. Should be auto-reset, as only one thread is
   waiting. May be NULL when event shouldn't be signalled (target
   thread is going to recheck before waiting). */
static HANDLE fiber_dead_wakeup;

struct fiber_dead_record {
    pthread_t companion_pthread;
    struct fiber_dead_record* next;
};

static struct fiber_dead_record *first_dead, *last_dead;

/* To be called from foreign thread TLS destructor */
static void fiber_is_dead(void* companion_pthread)
{
    struct fiber_dead_record* next;
    next = malloc(sizeof(*next));
    next->next = NULL;
    next->companion_pthread = companion_pthread;
    pthread_mutex_lock(&fiber_dead_mutex);
    if (!last_dead) {
        first_dead = last_dead = next;
    } else {
        last_dead->next = next;
        last_dead = next;
    }
    if (fiber_dead_wakeup) {
        SetEvent(fiber_dead_wakeup);
    }
    pthread_mutex_unlock(&fiber_dead_mutex);
}

/* Simplified condition for handling obsolete foreign threads. TODO:
   use either futexes or pthread */

#define FIBER_DEAD_LOCK 0
#define FIBER_DEAD_UNLOCK 1
#define FIBER_DEAD_WAIT 2

void fiber_dead_synchronization(int op)
{
    struct thread * self = arch_os_get_current_thread();
    switch(op) {
    case FIBER_DEAD_LOCK:
        pthread_mutex_lock(&fiber_dead_mutex);
        break;
    case FIBER_DEAD_UNLOCK:
        pthread_mutex_unlock(&fiber_dead_mutex);
        break;
    case FIBER_DEAD_WAIT:
        fiber_dead_wakeup = self->private_events.events[0];
        ResetEvent(fiber_dead_wakeup);
        pthread_mutex_unlock(&fiber_dead_mutex);
        WaitForMultipleObjects(2, self->private_events.events, FALSE, INFINITE);
        pthread_mutex_lock(&fiber_dead_mutex);
        fiber_dead_wakeup = NULL;
        break;
    }
}

pthread_t fiber_dead_get()
{
    pthread_t result = NULL;
    if (first_dead) {
        result = first_dead->companion_pthread;
        if (first_dead == last_dead) {
            first_dead = last_dead = NULL;
        } else {
            struct fiber_dead_record* prev = first_dead;
            first_dead = first_dead->next;
            free(prev);
        }
    }
    return result;
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

    /* FPU stack should be empty if our caller follows C conventions;
     * however, we have to reload the control word anyway, so cleaning
     * the stack won't add much overhead (with modern OSes and TS bit
     * in CR0, difference between using FPU and not using it at all is
     * _much_ more noticeable than difference between nine and one
     * instruction).

     * There are some known situations where C convention is violated,
     * at least by Wine -- e.g. any SEH handler may be entered with
     * unpredictable content in FPU stack.

     * Last but not least, it would be immoral to expect something
     * from our caller that we don't provide for callouts; our
     * :sb-auto-fpu-switch will make things right on demand, but if
     * the foreign calling thread uses the same approach, it will
     * surely interfere with our own SEH handler. */
    
    asm("fnclex;");
    asm("ffree %st(7);");
    asm("ffree %st(6);");
    asm("ffree %st(5);");
    asm("ffree %st(4);");
    asm("ffree %st(3);");
    asm("ffree %st(2);");
    asm("ffree %st(1);");
    asm("ffree %st(0);");


    x87_fldcw(self->saved_c_fpu_mode>>16);

    BEGIN_GC_UNSAFE_CODE;
    funcall3(SymbolValue(ENTER_ALIEN_CALLBACK,self),
             LISPOBJ(args[0]),LISPOBJ(args[1]),LISPOBJ(args[2]));
    END_GC_UNSAFE_CODE;

    /* Even if our return type is float or double, it's loaded into
     * fr0 by alien callback assembler wrapper, which expects FPU
     * stack to be empty at his epilogue (initially/in the upstream it
     * used to call funcall3(), which is a non-floating-result wrapper
     * for call_into_lisp with a normal C ABI).
     *
     * With :sb-auto-fpu-switch, stack would be emptied automatically
     * when the assembler wrapper loads a result (our exception
     * handler would perceive assembler wrapper as C code;
     * consequently, it would adjust FPU stack as for C
     * calls). However, we can't rely on it in foreign threads: our
     * exception handler isn't installed as a topmost one and won't
     * have a chance to run. Fibers that we use are non-FPU-aware at
     * all (a flag to specify FPU state preservation appeared
     * somewhere around win2003 - too late); if we allowed foreign
     * callback to return with a full ("lispy") FPU stack, we'd mess
     * up both the fiber and the foreign thread. */

    establish_c_fpu_world();
    info_ptr->done = 1;
    return;
}

void fff_generic_callback(lispobj arg0,lispobj arg1, lispobj arg2)
{
    struct thread* th = arch_os_get_current_thread();
    pthread_t companion_fiber;
    if (th) {
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
    pthread_setspecific(save_lisp_tls_key,TlsGetValue(OUR_TLS_INDEX));
    establish_c_fpu_world();
}

static void restore_lisp_tls()
{
    TlsSetValue(OUR_TLS_INDEX, pthread_getspecific(save_lisp_tls_key));
}


#endif  /* LISP_FEATURE_SB_THREAD */

int os_number_of_processors = 1;

void os_init(char *argv[], char *envp[])
{
    SYSTEM_INFO system_info;
    GetSystemInfo(&system_info);
    os_vm_page_size = system_info.dwPageSize;
    fast_bzero_pointer = fast_bzero_detect;
    os_number_of_processors = system_info.dwNumberOfProcessors;
    dyndebug_init();

    base_seh_frame = get_seh_frame();
    accept_post_mortem_startup();
#if defined(LISP_FEATURE_SB_THREAD)
    pthread_key_create(&save_lisp_tls_key,NULL);
    /* lisp_fiber_key with destructor.  After foreign thread
       exits, its companion Lisp fiber is enqueued for destruction
       too. */
    pthread_key_create(&lisp_fiber_key,fiber_is_dead);
    /* Now INVALID fpu trap should be enabled from the very start */
    _controlfp(~_EM_INVALID,_MCW_EM);
    pthread_save_context_hook = save_lisp_tls;
    pthread_restore_context_hook = restore_lisp_tls;
    pthread_cond_init(&fiber_factory_condition,NULL);
    pthread_mutex_init(&fiber_factory_lock,NULL);
    pthread_mutex_init(&fiber_dead_mutex,NULL);
    alloc_gc_page();
#endif
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

    if (!addr) {
        /* the simple case first */
        return
            AVERLAX(VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE));
    }

    RECURSIVE_REDUCE_TO_ONE_SPACE_ADDR(os_validate,addr,len);

    if (addr_in_mmapped_core(addr))
        return addr;

    if (!AVERLAX(VirtualQuery(addr, &mem_info, sizeof mem_info)))
        return 0;

    if ((mem_info.State == MEM_RESERVE) && (mem_info.RegionSize >=len)) {
        /* It would be correct to return here. However, support for Wine
           would be beneficial, and Wine has a strange behavior in this
           department. It reports all memory below KERNEL32.DLL as
           reserved, but disallows MEM_COMMIT.

           Let's work around it: reserve the region we need for a second
           time. Second reservation is documented to fail on normal NT
           family, but it will succeed on Wine if this region is
           actually free.
        */
        VirtualAlloc(addr, len, MEM_RESERVE, PAGE_EXECUTE_READWRITE);
        /* If it is wine, second call succeeds, and now the region is
           really reserved. */
        return addr;
    }

    if (mem_info.State == MEM_RESERVE) {
        fprintf(stderr, "validation of reserved space too short.\n");
        fflush(stderr);
    }

    if(!AVERLAX(VirtualAlloc(addr, len, (mem_info.State == MEM_RESERVE)?
                             MEM_COMMIT: MEM_RESERVE, PAGE_EXECUTE_READWRITE)))
        return 0;

    return addr;
}

os_vm_address_t
os_validate_recommit(os_vm_address_t addr, os_vm_size_t len)
{
    return
        AVERLAX(VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE));
}

os_vm_address_t
os_allocate_lazily(os_vm_size_t len)
{
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
        fast_bzero_pointer(addr, len);
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

int win32_open_for_mmap(const char* fileName)
{
    HANDLE handle;
    if (strcmp(fileName,non_external_self_name)) {
	handle = CreateFileA(fileName,GENERIC_READ|GENERIC_EXECUTE,
			     FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,
			     OPEN_EXISTING,0,NULL);
    } else {
	WCHAR mywpath[MAX_PATH+1];
	DWORD gmfnResult = GetModuleFileNameW(NULL,mywpath,MAX_PATH+1);
	AVER(gmfnResult>0 && gmfnResult<(MAX_PATH+1));
	handle = CreateFileW(mywpath,GENERIC_READ|GENERIC_EXECUTE,
			     FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,
			     OPEN_EXISTING,0,NULL);
	
    }
    AVER(handle && (handle!=INVALID_HANDLE_VALUE));
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
    os_vm_size_t count;

    if (addr == (os_vm_address_t)DYNAMIC_SPACE_START)
    {
        if (IS_ALIGNED(offset,os_vm_mmap_unit_size)) {
            len = ALIGN_UP(len,os_vm_mmap_unit_size);
            HANDLE mapping =
                CreateFileMapping((HANDLE)_get_osfhandle(fd),NULL,
                                  PAGE_EXECUTE_WRITECOPY, 0, offset+len,
                                  NULL);
            if (!mapping) {
                mapping =
                    CreateFileMapping((HANDLE)_get_osfhandle(fd),NULL,
                                      PAGE_READONLY, 0, offset+len,
                                      NULL);
                if (mapping)
                    os_supports_executable_mapping = 0;
            } else {
                os_supports_executable_mapping = 1;
            }
            if (mapping) {
                /* Was validate()d, that's why we can't mmap with copy-on-write.
                   Well, let's turn it back... */
                AVERLAX(VirtualFree(addr, 0, MEM_RELEASE));
                AVER(MapViewOfFileEx(mapping, FILE_MAP_COPY
                                     |(os_supports_executable_mapping?
                                       FILE_MAP_EXECUTE: 0), 0,
                                     offset, len, addr));

                /* Reserve the rest of dynamic space... */
                os_validate(addr+len, dynamic_space_size-len);
                core_mmap_end = addr + len;
                os_mmap_protect_modes = os_supports_executable_mapping ?
                    os_mmap_exec_modes : os_mmap_noexec_modes;
                return addr;
            }
        }
    }
    AVER(VirtualAlloc(addr, len, MEM_COMMIT, PAGE_EXECUTE_READWRITE)||
         VirtualAlloc(addr, len, MEM_RESERVE|MEM_COMMIT, PAGE_EXECUTE_READWRITE));

    CRT_AVER_NONNEGATIVE(lseek(fd, offset, SEEK_SET));

    count = read(fd, addr, len);
    CRT_AVER( count == len );

    return addr;
}


void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    DWORD old_prot;

    RECURSIVE_REDUCE_TO_ONE_SPACE_VOID(os_protect,address,length,,prot);

    if (addr_in_mmapped_core(address)) {
        AVER(VirtualProtect(address, length, os_mmap_protect_modes[prot],
                            &old_prot));
    } else {
        AVER(VirtualProtect(address, length, os_protect_modes[prot], &old_prot)||
             (VirtualAlloc(address, length, MEM_COMMIT,os_protect_modes[prot]) &&
              VirtualProtect(address, length, os_protect_modes[prot], &old_prot)));
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



/* FIXME: Now that FOO_END, rather than FOO_SIZE, is the fundamental
 * description of a space, we could probably punt this and just do
 * (FOO_START <= x && x < FOO_END) everywhere it's called. */
static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*)((long)sbeg);
    char* end = (char*)((long)sbeg) + slen;
    char* adr = (char*)a;
    return (adr >= beg && adr < end);
}

boolean
is_linkage_table_addr(os_vm_address_t addr)
{
    return in_range_p(addr, LINKAGE_TABLE_SPACE_START, LINKAGE_TABLE_SPACE_END);
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    struct thread *th;
    if(in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
       in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
       in_range_p(addr, DYNAMIC_SPACE_START  , dynamic_space_size))
        return 1;
    for_each_thread(th) {
        if(((os_vm_address_t)th->control_stack_start <= addr) && (addr < (os_vm_address_t)th->control_stack_end))
            return 1;
        if(in_range_p(addr, (unsigned long)th->binding_stack_start, BINDING_STACK_SIZE))
            return 1;
    }
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

 static inline
 int fpu_world_cruel_p()
 {
     struct thread* th = arch_os_get_current_thread();
     return th && ((th->in_lisp_fpu_mode)&0xFF00);
 }

 static inline
 int fpu_world_lispy_p()
 {
     struct thread* th = arch_os_get_current_thread();
     return th && (th->in_lisp_fpu_mode&0xFF);
 }

 /* In SBCL code, Lisp may fldenv or something, so he will own FPU
  * but never enter our SEH handler... */
 static inline
 int fpu_world_unknown_p()
 {
     struct thread* th = arch_os_get_current_thread();
     return th && dyndebug_lazy_fpu_careful && (th->in_lisp_fpu_mode&0xFF0000);
 }

 void establish_c_fpu_world()
 {
     struct thread* th = arch_os_get_current_thread();
     if (fpu_world_unknown_p()) {
         x87_fldcw(th->saved_c_fpu_mode>>16);
	 asm("ffree %st(7);");
	 asm("ffree %st(6);");
	 asm("ffree %st(5);");
	 asm("ffree %st(4);");
	 asm("ffree %st(3);");
	 asm("ffree %st(2);");
	 asm("ffree %st(1);");
	 asm("ffree %st(0);");
	 asm("fwait");
	 th->in_lisp_fpu_mode = 0;
     }
     if (fpu_world_lispy_p()) {
         unsigned int mode;
	 asm("fnstcw %0": "=m"(mode));
         th->saved_lisp_fpu_mode = mode <<16;
         x87_fldcw(th->saved_c_fpu_mode>>16);
	 asm("fstp %st(0); fstp %st(0)");
	 asm("fstp %st(0); fstp %st(0)");
	 asm("fstp %st(0); fstp %st(0)");
	 asm("fstp %st(0); fstp %st(0)");
	 th->in_lisp_fpu_mode = 0;
     }
     AVERLAX(!fpu_world_unknown_p());
     AVERLAX(!fpu_world_lispy_p());
 }



 void c_level_backtrace(const char* header, int depth)
 {
     void* frame;
     int n = 0;
     void** lastseh;

     for (lastseh = get_seh_frame(); lastseh && (lastseh!=(void*)0xFFFFFFFF);
	  lastseh = *lastseh);

     fprintf(dyndebug_output, "Backtrace: %s (thread %p)\n", header, this_thread);
     for (frame = __builtin_frame_address(0); frame; frame=*(void**)frame)
     {
	 if ((n++)>depth)
	     return;
	 fprintf(dyndebug_output, "[#%02d]: ebp = 0x%p, ret = 0x%p\n",n,
		 frame, ((void**)frame)[1]);
     }
 }

 /* Called when FPU world is in indefinite state (in exception handler,
    for instance), to establish safely one of the definite ones.

    While a lot of C code is neutral to FPU, Lisp definitely shouldn't
    run with half-full stack.

    To recover lazily (when we enter Lisp) we introduce a special bit
    in in_lisp_fpu_mode.

    Target FPU world of recover is always the world that was current
    before it was broken. */

 static inline
 void recover_fpu_world()
 {
     if (fpu_world_cruel_p()) {
	 int lispyp = fpu_world_lispy_p();
	 x87_env nice_fpu_environment = {
	     .control = (lispyp ?
			 this_thread->saved_lisp_fpu_mode :
			 this_thread->saved_c_fpu_mode)>>16,
	     .status = 0,
	     .tags = (lispyp ? 0x5555 : 0xFFFF)
	 };

	 if (dyndebug_lazy_fpu) {
	     fprintf(dyndebug_output,
		     "Recovering indeterminate FPU state to lispyp=%d\n",
		     lispyp);
	     c_level_backtrace("Recovering FPU state..",15);
	 }
	 asm("fnclex;fldenv %0;fwait" : :"m"(nice_fpu_environment));
	 (arch_os_get_current_thread())->in_lisp_fpu_mode &= 0xFF;
     }
 }

 static
 void check_fpu_state(const char* format,...)
 {
     if (dyndebug_lazy_fpu && !fpu_world_cruel_p()) {
	 boolean lispyp = fpu_world_lispy_p();
	 x87_env fpu_environment = x87_fnstenv();
	 boolean have_valid_regs = (fpu_environment.tags!=0xFFFFu);
	 boolean have_empty_regs =
	     (0!=((fpu_environment.tags & 0x5555u) &
		  ((fpu_environment.tags & 0xAAAAu)>>1)));
	 if ((lispyp && have_empty_regs)||
	     (!lispyp && have_valid_regs)) {
	     if (format) {
		 va_list header;
		 va_start(header,format);
		 vfprintf(dyndebug_output,format,header);
	     }
	     fprintf(dyndebug_output,"\n"
		     "Strange FPU state detected:\n"
		     "Expected %s\n"
		     "Actual tag word is %04x\n",
		     lispyp? "Lisp mode (no empty regs)" : "C mode (all regs empty)",
		     fpu_environment.tags);
	     c_level_backtrace("Unexpected FPU state", 10);
	 }
	 x87_fldenv(fpu_environment);
     }
 }

 boolean sb_control_fpu_normally_untouched_by_non_float_calls = 1;
 boolean sb_control_update_sb_stat_counters = 1;
 LONG sb_stat_fpu_modeswitch_cycling_counter = 0L;

 #define WCTX_PUSH(wctx,value)                                           \
     do { wctx->Esp-=4;                                                  \
	 *((intptr_t*)(intptr_t) wctx->Esp)=(intptr_t)value; } while(0)

 #define WCTX_PUSHA(wctx)                        \
     do {                                        \
     intptr_t orig_esp = wctx->Esp;              \
     WCTX_PUSH(wctx,wctx->Eax);                  \
     WCTX_PUSH(wctx,wctx->Ecx);                  \
     WCTX_PUSH(wctx,wctx->Edx);                  \
     WCTX_PUSH(wctx,wctx->Ebx);                  \
     WCTX_PUSH(wctx,orig_esp);                   \
     WCTX_PUSH(wctx,wctx->Ebp);                  \
     WCTX_PUSH(wctx,wctx->Esi);                  \
     WCTX_PUSH(wctx,wctx->Edi);                  \
     } while(0)



 typedef void (*callback_pointer)();

 /**
  * Modify context in order to make it run run_function after the
  * signal or exception handler returns. After run_function is
  * complete, it returns to original code.
  *
  * @param context CONTEXT of a thread where the call should be
  * injected
  *
  * @param run_function function whose call should be injected
  *
  * @param frame_data_size size of a data block allocated for the
  * duration of function call (dynamic extent).
  *
  * @param arg1
  * @param arg2
  *
  * `run-function' can receive up to 3 parameters. The first one is
  * (automagically) a pointer to frame_data_size-sized memory block.
  * The rest two parameters are arbitrary; `run_function' can be
  * declared without them and work flawlessly, but the caller of
  * inject_function_call should better give some values for arg1 and
  * arg2 anyway.
  *
  * Temporary memory block is allocated below inject_function_call
  * frame on the same stack, which is a very dangerous technique: not
  * only the frame data may become trash if somebody uses the stack up
  * to this size, but an execution sequence of the function could be
  * broken as well.
  *
  * @return pointer to a memory block of size `frame_data_size', that
  * will be deallocated after `run_function' returns.
  */
 static inline
 void* inject_function_call(CONTEXT *context,
			    callback_pointer run_function,
			    size_t frame_data_size,
			    void* arg1, void* arg2)
 {
     void* frame_data = ((void*)&context) -
	 (0x2000 + frame_data_size);

     WCTX_PUSH(context, context->Eip); /* call ZZZZZ */
     WCTX_PUSH(context, context->Ebp); /* push %ebp */
     context->Ebp = context->Esp;      /* mov %esp, %ebp */
     context->Esp = (DWORD)frame_data;
     WCTX_PUSH(context, context->EFlags);
     WCTX_PUSHA(context);
     WCTX_PUSH(context,arg2);
     WCTX_PUSH(context,arg1);
     WCTX_PUSH(context,frame_data); /* FloatSave copy */
     extern void post_signal_tramp();
     WCTX_PUSH(context,post_signal_tramp); /* "return" address */
     context->Eip = (intptr_t)run_function;
     return frame_data;
 }

 static inline
 void inject_npx_recovery(CONTEXT* context)
 {
     const size_t data_size = sizeof(context->FloatSave);

     memcpy(inject_function_call(context, fpu_restore, data_size, 0, 0),
	    &context->FloatSave, data_size);
 }

 extern void fpu_insns_modrm(), fpu_insns_high();

 static inline
 void maybe_inject_fpu_instruction_restart(CONTEXT* context)
 {
     /* if (!(context->FloatSave.StatusWord & 0x4000)) */
     /* 	return ; */
     uint8_t* fpu_eip = (void*)(uintptr_t)context->FloatSave.ErrorOffset;

     DWORD* temp_data = (((void*)&fpu_eip)-0x2000);

     uint8_t fpu_prefix_D8toDF = (0xFFu & (context->FloatSave.ErrorSelector>>24))|0xD8;
     uint8_t fpu_next_byte = 0xFFu & (context->FloatSave.ErrorSelector>>16);
     uint8_t fpu_prefix_offset = fpu_prefix_D8toDF ^ 0xD8;
     void* fpu_insn_base;
     uint16_t fpu_insn_index;

     if (!(context->FloatSave.ErrorSelector &0xFFFF0000)) {
	 if (context->ExtendedRegisters[7] ||
	     context->ExtendedRegisters[6]) {
	     fpu_prefix_D8toDF = context->ExtendedRegisters[7]^0xD8;
	     fpu_next_byte = context->ExtendedRegisters[6];
	 } else {
	     fpu_prefix_D8toDF = fpu_eip[0];
	     fpu_next_byte = fpu_eip[1];
	 }
	 fpu_prefix_offset = fpu_prefix_D8toDF ^ 0xD8;
     }


     AVER(fpu_prefix_offset<8);
     context->FloatSave.ErrorSelector = 0xFFFF0000;

     switch (fpu_next_byte & 0xC0) {
     case 0xC0:
	 /* no memory operand; 64 instructions per prefix, 8 bytes
	  * per instruction, starting at fpu_insns_high. */
	 fpu_insn_base = &fpu_insns_high;
	 fpu_insn_index = (fpu_prefix_offset * 64) + (fpu_next_byte ^ 0xC0);
	 break;
     default:
	 fpu_insn_base = &fpu_insns_modrm;
	 fpu_insn_index = (fpu_prefix_offset * 8) + ((fpu_next_byte & 070)>>3);
	 break;
     }
     temp_data[0] = context->Ebp;
     temp_data[1] = context->Eax;
     temp_data[2] = context->Eip;

     context->Eax = context->FloatSave.DataOffset;
     context->Eip = (DWORD)(uintptr_t)(fpu_insn_base + 8*fpu_insn_index);
     context->Ebp = (DWORD)(uintptr_t)temp_data;

 }

 static inline
 void maybe_set_xmm_state(CONTEXT* context, boolean for_lisp_p)
 {
     unsigned short tags_x87 = context->FloatSave.TagWord & 0xFFFFu;
     unsigned short control_x87 = context->FloatSave.ControlWord & 0xFFFFu;
     unsigned short status_x87 = context->FloatSave.StatusWord & 0xFFFFu;

     if (context->ContextFlags & CONTEXT_EXTENDED_REGISTERS) {
	 memset(context->ExtendedRegisters+32,0,8*8);
	 context->ExtendedRegisters[4] = for_lisp_p ? 0xFF : 0x00;
	 context->ExtendedRegisters[0] = control_x87 & 0xFFu;
	 context->ExtendedRegisters[1] = control_x87>>8;
	 context->ExtendedRegisters[2] = status_x87 & 0xFFu;
	 context->ExtendedRegisters[3] = status_x87>>8;
     }
 }

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
    DWORD lastError;
    DWORD code = exception_record->ExceptionCode;
    EXCEPTION_DISPOSITION disposition = ExceptionContinueExecution;
    void* fault_address = (void*)exception_record->ExceptionInformation[1];
    os_context_t ctx, *oldctx;
    
    struct thread* self = arch_os_get_current_thread();
    int contextual_fpu_state = self ? self->in_lisp_fpu_mode : 0;

    lastError = GetLastError();
    
    if (dyndebug_lazy_fpu)
    {
	DWORD tags = context->FloatSave.TagWord;
	if (!fpu_world_lispy_p() && ((tags&0xFFFF)!=0xFFFF)) {
	    fprintf(dyndebug_output,
		    "Exception handler/ strange NPX for C world\n"
		    "....with tags 0x%04lx, status 0x%04lx, control 0x%04lx\n",
		    context->FloatSave.TagWord,
		    context->FloatSave.StatusWord,
		    context->FloatSave.ControlWord);
	    c_level_backtrace("Exception handler: strange NPX state",10);
	}
	if (fpu_world_lispy_p() && ((tags&0x5555) & ((tags&0xAAAA)>>1)))
	{
	    fprintf(dyndebug_output,
		    "Exception handler/ strange NPX for Lisp world\n"
		    "....with tags 0x%04lx, status 0x%04lx, control 0x%04lx\n",
		    context->FloatSave.TagWord,
		    context->FloatSave.StatusWord,
		    context->FloatSave.ControlWord);
	    c_level_backtrace("Exception handler: strange NPX state",10);
	}
    }
    if (code == EXCEPTION_FLT_STACK_CHECK) {
	int stack_empty = (!(context->FloatSave.StatusWord &(1<<9)));

	/* We used to expect such exceptions in non-lispy and
	 * ambigious states only. With FPU insn restart support,
	 * exceptions originating in C may be handled too. */

	AVERLAX(!fpu_world_cruel_p()); /* Let's complain if a Lisp
					* code was exposed to
					* interrupt at ambigious
					* time */
	if (stack_empty && fpu_world_lispy_p()) {
	    fprintf(dyndebug_output,
		    "Exception handler: stack EMPTY while it should be FULL.\n"
		    "....tags 0x%04lx, status 0x%04lx, control 0x%04lx\n",
		    context->FloatSave.TagWord,
		    context->FloatSave.StatusWord,
		    context->FloatSave.ControlWord);
	    c_level_backtrace("Exception handler: strange NPX state",10);



	    /* FPU stack must be in SBCL-compliant state, having no
	     * empty regs, but somehow it became empty! BAD thing. */
	    goto complain;
	}

	if (!stack_empty && !fpu_world_lispy_p()) {
	    /* FPU stack must be in C-compliant state, having no
	     * valid regs, but somehow it has overflown! BAD thing. */


	    fprintf(dyndebug_output,
		    "Exception handler: stack FULL while it should be EMPTY.\n"
		    "....tags 0x%04lx, status 0x%04lx, control 0x%04lx\n",
		    context->FloatSave.TagWord,
		    context->FloatSave.StatusWord,
		    context->FloatSave.ControlWord);
	    c_level_backtrace("Exception handler: strange NPX state",10);

	    goto complain;
	}

	/* We have legitimate FPU stack exception that is to be
	 * handled silently.  */
	if (dyndebug_lazy_fpu) {
	    fprintf(dyndebug_output,
		    "Exception handler: FPU stack %s (%s compliance expected).\n"
		    "....tags 0x%04lx, status 0x%04lx, control 0x%04lx\n"
		    "....DataOffset %08lx, ErrorOffset %08lx\n"
		    "....DataSelector %08lx, ErrorSelector %08lx\n"
		    "....Cr0Npx %08lx, XMM opcode [6]=%02x, [7]=%02x\n"
		    "....Main CPU EIP %08lx, ESP %08lx\n",
		    stack_empty ? "EMPTY" : "FULL",
		    fpu_world_lispy_p() ? "Lisp" : "C",
		    context->FloatSave.TagWord,
		    context->FloatSave.StatusWord,
		    context->FloatSave.ControlWord,
		    context->FloatSave.DataOffset,
		    context->FloatSave.ErrorOffset,
		    context->FloatSave.DataSelector,
		    context->FloatSave.ErrorSelector,
		    context->FloatSave.Cr0NpxState,
		    context->ExtendedRegisters[6],
		    context->ExtendedRegisters[7],
		    context->Eip, context->Esp);
	    c_level_backtrace("....see where it happened.",10);
	}
	if (sb_control_update_sb_stat_counters) {
	    InterlockedIncrement(&sb_stat_fpu_modeswitch_cycling_counter);
	}

	*((contextual_fpu_state ?
	   &self->saved_lisp_fpu_mode :
	   &self->saved_c_fpu_mode)) = (context->FloatSave.ControlWord & 0xFFFF)<<16;

	contextual_fpu_state = contextual_fpu_state^4; /* inverted */

	context->FloatSave.ControlWord =
	    *(contextual_fpu_state ?
	      &self->saved_lisp_fpu_mode :
	      &self->saved_c_fpu_mode)>>16;


	context->FloatSave.StatusWord &= ~(0x3941);

	context->FloatSave.TagWord = contextual_fpu_state ? 0x5555 : 0xFFFF;
	memset(context->FloatSave.RegisterArea,0,
	       sizeof(context->FloatSave.RegisterArea));
	maybe_set_xmm_state(context,contextual_fpu_state);
	maybe_inject_fpu_instruction_restart(context);
	goto finish;
    }

    /* If FPU world is not marked as cruel, we mark it (we don't
     * really know where we interrupted). */

    if (self&&((self->in_lisp_fpu_mode &&
		((context->FloatSave.TagWord & 0x5555)&
		 ((context->FloatSave.TagWord & 0xAAAA)>>1)))
	       ||(!self->in_lisp_fpu_mode &&
		  ((context->FloatSave.TagWord & 0xFFFF)!=0xFFFF)))) {
	self->in_lisp_fpu_mode |= 0xFF00; /* Hello cruel world */
    }

    ctx.win32_context = context;
    ctx.sigmask = self ? self->os_thread->blocked_signal_set : 0;

    /* For EXCEPTION_ACCESS_VIOLATION only. */
    odprintf("handle exception, EIP = 0x%p, code = 0x%p (addr = 0x%p)",
	     context->Eip, exception_record->ExceptionCode, fault_address);

    if (single_stepping &&
	exception_record->ExceptionCode == EXCEPTION_SINGLE_STEP) {
	/* We are doing a displaced instruction. At least function
	 * end breakpoints uses this. */

	BEGIN_GC_UNSAFE_CODE;	/* todo is it really gc-unsafe? */
	restore_breakpoint_from_single_step(&ctx);
	END_GC_UNSAFE_CODE;
	goto finish;
    }
    if (IS_TRAP_EXCEPTION(exception_record, ctx)) {
	unsigned trap;
	/* Unlike some other operating systems, Win32 leaves EIP
	 * pointing to the breakpoint instruction. */
	ctx.win32_context->Eip += TRAP_CODE_WIDTH;
	/* Now EIP points just after the INT3 byte and aims at the
	 * 'kind' value (eg trap_Cerror). */
	trap = *(unsigned char *)(*os_context_pc_addr(&ctx));
	 
	/* Before any other trap handler: gc_safepoint ensures that
	   inner alloc_sap for passing the context won't trap on
	   pseudo-atomic. */
	if (trap == trap_PendingInterrupt) {
	    /* Done everything needed for this trap, except EIP
	       adjustment */
	    gc_maybe_stop_with_context(&ctx, 0);
	    arch_skip_instruction(&ctx);
	    goto finish;
	}
	/* This is just for info in case the monitor wants to print an
	 * approximation. */
	access_control_stack_pointer(self) =
	    (lispobj *)*os_context_sp_addr(&ctx);

	block_blockable_signals(0,&ctx.sigmask);

	BEGIN_GC_UNSAFE_CODE;
	handle_trap(&ctx, trap);
	END_GC_UNSAFE_CODE;
	
	thread_sigmask(SIG_SETMASK,&ctx.sigmask,NULL);

	goto finish;
    }
    else if (exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
	if (fault_address == GC_SAFEPOINT_PAGE_ADDR) {
	    /* Pick off GC-related memory fault next. */
	    gc_maybe_stop_with_context(&ctx, 1);
	    goto finish;
	}
	int index = find_page_index(fault_address);
	if (index != -1) {
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
	
	AVER(VirtualAlloc(PTR_ALIGN_DOWN(fault_address,os_vm_page_size),
			  os_vm_page_size,
			  MEM_COMMIT, PAGE_EXECUTE_READWRITE)
	     ||(fprintf(stderr,"Unable to recommit addr %p eip %p\n",
			fault_address, context->Eip) &&
		(c_level_backtrace("BT",5),
		 fake_foreign_function_call(&ctx),
		 lose("Lispy backtrace"),
		 0)));

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

    fprintf(stderr, "Exception Code: 0x%lx.\n", exception_record->ExceptionCode);
    fprintf(stderr, "Faulting IP: 0x%lx.\n", (DWORD)exception_record->ExceptionAddress);
    if (exception_record->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
	MEMORY_BASIC_INFORMATION mem_info;

	if (VirtualQuery(fault_address, &mem_info, sizeof mem_info)) {
	    fprintf(stderr, "page status: 0x%lx.\n", mem_info.State);
	}

	fprintf(stderr, "Was writing: %ld, where: 0x%lx.\n",
		exception_record->ExceptionInformation[0],
		(DWORD)fault_address);
    }

    fflush(stderr);

    fake_foreign_function_call(&ctx);
    lose("Exception too early in cold init, cannot continue.");

    /* FIXME: WTF? How are we supposed to end up here? */
#if defined(LISP_FEATURE_SB_THREAD)
    pthread_sigmask(SIG_SETMASK, &ctx.sigmask, NULL);
    gc_contextual_safepoint(&ctx);
#endif

    /* Common return point. */
finish:
    if (self)
	self->in_lisp_fpu_mode = contextual_fpu_state;
    SetLastError(lastError);
    return disposition;
}

/* When Lisp dumps core, it's sometimes useful to make it
   CreateProcess instead of dying. */
struct at_exit_instructions {
    void* start_application;
    void* command_line;
    HANDLE std_handles[3];
} do_at_exit;

void post_mortem_function()
{
    PROCESS_INFORMATION pinfo;
    STARTUPINFO si = {.cb = sizeof (STARTUPINFO)};
    HANDLE myHandle;
    char myHandleStr[32];

    DuplicateHandle(GetCurrentProcess(), GetCurrentProcess(),
                    GetCurrentProcess(), &myHandle,
                    0,
                    TRUE,
                    DUPLICATE_SAME_ACCESS);
    snprintf(myHandleStr,32,"%u",(unsigned)myHandle);
    si.hStdInput = do_at_exit.std_handles[0];
    si.hStdOutput = do_at_exit.std_handles[1];
    si.hStdError = do_at_exit.std_handles[2];

    SetStdHandle(STD_INPUT_HANDLE,INVALID_HANDLE_VALUE);
    SetStdHandle(STD_OUTPUT_HANDLE,INVALID_HANDLE_VALUE);
    SetStdHandle(STD_ERROR_HANDLE,INVALID_HANDLE_VALUE);
    si.dwFlags = STARTF_USESTDHANDLES;
    if (!do_at_exit.start_application)
        return;
    SetEnvironmentVariable("SBCL_STARTING_ITS_HEIR",myHandleStr);

    CreateProcess(do_at_exit.start_application,
                  do_at_exit.command_line,
                  NULL,                 /* lpProcessAttributes */
                  NULL,                 /* lpThreadAttributes */
                  TRUE,                 /* bInheritHandles */
                  0,                    /* dwCreationFlags */
                  NULL,                 /* lpEnvironment */
                  NULL,                 /* lpCurrentDirectory */
                  &si,                  /* startupinfo */
                  &pinfo);              /* lpProcessInformation */
    CloseHandle(&si.hStdInput);
    CloseHandle(&si.hStdOutput);
    CloseHandle(&si.hStdError);
}

void start_before_dying(void* app, void* cmdline) {
    char* copy_app = malloc(strlen(app)+1);
    char* copy_cmdline = malloc(strlen(cmdline)+1);
    DWORD handle_kinds[] =
        {STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE};
    int i;

    strcpy(copy_app,app);
    strcpy(copy_cmdline,cmdline);
    if (do_at_exit.command_line) {
        free(do_at_exit.start_application);
        free(do_at_exit.command_line);
    } else {
        atexit(post_mortem_function);
    }
    for (i=0; i<=2; ++i)
        DuplicateHandle(GetCurrentProcess(),GetStdHandle(handle_kinds[i]),
                        GetCurrentProcess(),&do_at_exit.std_handles[i],
                        0, TRUE, DUPLICATE_SAME_ACCESS);
    do_at_exit.start_application = copy_app;
    do_at_exit.command_line = copy_cmdline;
}

void accept_post_mortem_startup()
{
    char parentHandleStr[32];
    HANDLE parentHandle;
    if (!GetEnvironmentVariable("SBCL_STARTING_ITS_HEIR",parentHandleStr,32))
        return;
    SetEnvironmentVariable("SBCL_STARTING_ITS_HEIR",NULL);
    parentHandle = (HANDLE)strtoul(parentHandleStr, NULL, 0);
    WaitForSingleObject(parentHandle,INFINITE);
}

void
wos_install_interrupt_handlers(struct lisp_exception_frame *handler)
{
    handler->next_frame = get_seh_frame();
    handler->handler = (void*)exception_handler_wrapper;
    set_seh_frame(handler);
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

struct console_input_data {
    pthread_mutex_t lock;
    HANDLE handle;
    int readers;
} console_input_data = {PTHREAD_MUTEX_INITIALIZER, INVALID_HANDLE_VALUE, 0};

void win32_interrupt_console_input()
{
    HANDLE newInstance;
    AVER(0==pthread_mutex_lock(&console_input_data.lock));
    AVER(DuplicateHandle(GetCurrentProcess(),console_input_data.handle,
                         GetCurrentProcess(),&newInstance,
                         0, FALSE, DUPLICATE_CLOSE_SOURCE|DUPLICATE_SAME_ACCESS));
    console_input_data.handle = newInstance;
    AVER(0==pthread_mutex_unlock(&console_input_data.lock));
}

void win32_start_console_input(HANDLE original, HANDLE *target)
{
    /* AVER(0==pthread_mutex_lock(&console_input_data.lock)); */
    /* if (!console_input_data.readers) { */
    /*     AVER(DuplicateHandle(GetCurrentProcess(),original, */
    /*                          GetCurrentProcess(),&console_input_data.handle, */
    /*                          0, FALSE, DUPLICATE_SAME_ACCESS)); */
    /* } */
    /* *target = console_input_data.handle; */
    /* (this_thread)->waiting_console_handle = console_input_data.handle; */
    /* ++ console_input_data.readers; */
    /* AVER(0==pthread_mutex_unlock(&console_input_data.lock)); */
    *target = original;

}

void win32_end_console_input()
{
    /* AVER(0==pthread_mutex_lock(&console_input_data.lock)); */
    /* if (!(-- console_input_data.readers)) { */
    /*     CloseHandle(console_input_data.handle); */
    /* }; */
    /* (this_thread)->waiting_console_handle = INVALID_HANDLE_VALUE; */
    /* AVER(0==pthread_mutex_unlock(&console_input_data.lock)); */
}


int win32_write_unicode_console(HANDLE handle, void * buf, int count)
{
    DWORD written = 0;
    count &= ~1;
    if (WriteConsoleW(handle,buf,count>>1,&written,NULL)) {
        if (!written) {
            errno = EINTR;
            return -1;
        } else {
            return 2*written;
        }
    } else {
        errno = EIO;
        return -1;
    }
}

int win32_read_unicode_console(HANDLE handle, void* buf, int count)
{
    DWORD nread = 0;
    BOOL ok;
    HANDLE ownhandle;
    count &= ~1;
again:
    win32_start_console_input(handle,&ownhandle);
    if (WaitForSingleObject(this_thread->private_events.events[1],
			    0)!=WAIT_TIMEOUT) {
	errno = EINTR;
	return -1;
    }
    ok = ReadConsoleW(ownhandle,buf,count>>1,&nread,NULL);
    win32_end_console_input();
    if (ok) {
        if (!nread) {
            errno = EINTR;
            return -1;
        } else {
            DWORD nch,offset = 0;
            LPWSTR ubuf = buf;
            for (nch=0;nch<nread;++nch) {
                if (ubuf[nch]==13) {
                    ++offset;
                } else {
                    if (offset)
                        ubuf[nch-offset]=ubuf[nch];
                }
            }
            nread-=offset;
            if(!nread)
                goto again;
            return 2*nread;
        }
    } else {
        errno = EIO;
        return -1;
    }
}

static const LARGE_INTEGER zero_large_offset = {.QuadPart = 0LL};

int win32_unix_write(int fd, void * buf, int count)
{
    HANDLE handle;
    DWORD written_bytes;
    OVERLAPPED overlapped;
    struct thread * self = arch_os_get_current_thread();
    BOOL waitInGOR;
    LARGE_INTEGER file_position;
    BOOL seekable;

    odprintf("write(%d, 0x%p, %d)", fd, buf, count);
    handle =(HANDLE)_get_osfhandle(fd);
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
    if (WriteFile(handle, buf, count, &written_bytes,
                  &overlapped)) {
        odprintf("write(%d, 0x%p, %d) immeditately wrote %d bytes",
                 fd, buf, count, written_bytes);
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

int win32_unix_read(int fd, void * buf, int count)
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

    odprintf("read(%d, 0x%p, %d)", fd, buf, count);
    handle = (HANDLE)_get_osfhandle(fd);
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
    ok = ReadFile(handle,buf,count,&read_bytes, &overlapped);
    if (ok) {
        /* immediately */
        goto done_something;
    } else {
        errorCode = GetLastError();
        if (errorCode == ERROR_HANDLE_EOF || errorCode == ERROR_BROKEN_PIPE) {
            /* it is an `error' for positioned reads! oh wtf */
            goto done_something;
        }
        if (errorCode!=ERROR_IO_PENDING) {
            /* is it some _real_ error? */
	    fprintf(stderr,"LastError: %d\n",GetLastError());
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
                if (errorCode == ERROR_HANDLE_EOF) {
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

/* EOF */

/* Local Variables: */
/* c-file-style: "stroustrup" */
/* End: */
