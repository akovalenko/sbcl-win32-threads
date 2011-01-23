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

#ifndef SBCL_INCLUDED_WIN32_OS_H
#define SBCL_INCLUDED_WIN32_OS_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "target-arch-os.h"
#include "target-arch.h"

#ifdef LISP_FEATURE_SB_THREAD
#include "pthreads_win32.h"
#endif


typedef LPVOID os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

/* typedef void *siginfo_t; */

/* These are used as bitfields, but Win32 doesn't work that way, so we do a translation. */
#define OS_VM_PROT_READ    1
#define OS_VM_PROT_WRITE   2
#define OS_VM_PROT_EXECUTE 4

#define os_open_core(file,mode) win32_open_for_mmap(file)
#define HAVE_os_open_core

#define os_fopen_runtime(file,mode) win32_fopen_runtime()
#define HAVE_os_fopen_runtime

extern int os_number_of_processors;
#define HAVE_os_number_of_processors

extern int win32_open_for_mmap(const char* file);
extern FILE* win32_fopen_runtime();

#define OUR_TLS_INDEX 63
#define SIG_MEMORY_FAULT SIGSEGV

#define SIG_STOP_FOR_GC (SIGRTMIN+1)
#define SIG_DEQUEUE (SIGRTMIN+2)
#define SIG_THREAD_EXIT (SIGRTMIN+3)

#define FPU_STATE_SIZE 27

struct lisp_exception_frame {
    struct lisp_exception_frame *next_frame;
    void *handler;
    lispobj *bindstack_pointer;
};

void wos_install_interrupt_handlers(struct lisp_exception_frame *handler);
char *dirname(char *path);

#define HAVE_os_invalidate_free
#define HAVE_os_validate_recommit
#define HAVE_os_allocate_lazily

#define os_vm_mmap_unit_size 65536
#define HAVE_os_vm_mmap_unit_size

void os_invalidate_free(os_vm_address_t addr, os_vm_size_t len);
void* os_validate_recommit(os_vm_address_t addr, os_vm_size_t len);
os_vm_address_t os_allocate_lazily(os_vm_size_t len);
void accept_post_mortem_startup();

void win32_interrupt_console_input();
void os_link_runtime();
void establish_c_fpu_world();

/* Temporal per-thread storage for non-lisp values, that I constantly
   need on Win32 to run code snippets injected by SEH handler and
   continue into normal code (yes, :sb-auto-fpu-switch is hackish).

   If your platform runs signal handlers on normal stack, you may need
   such space as well, for equivalent purposes. Please consider using
   the same place: convenient, easy-to-find, and not affecting gencgc's
   conservatism. */

#define THREAD_ALIEN_RESERVE (64*N_WORD_BYTES)

#define get_thread_alien_reserve(th)		\
    ((void *)th->alien_stack_start		\
     + ALIEN_STACK_SIZE - THREAD_ALIEN_RESERVE)
    

extern int dyndebug_lazy_fpu;
extern int dyndebug_lazy_fpu_careful;
extern int dyndebug_skip_averlax;
extern int dyndebug_survive_aver;
extern int dyndebug_runtime_link;
extern int dyndebug_safepoints;

void odprintf_(const char * fmt, ...);

#define odxprint(topic,fmt,...)			\
    do if(dyndebug_##topic)			\
	   odprintf_(fmt,__VA_ARGS__);		\
    while(0)

#define bcopy(src,dest,n) memmove(dest,src,n)

#endif  /* SBCL_INCLUDED_WIN32_OS_H */
