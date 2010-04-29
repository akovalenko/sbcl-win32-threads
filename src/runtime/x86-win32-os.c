/*
 * The x86 Win32 incarnation of arch-dependent OS-dependent routines.
 * See also "win32-os.c".
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

#include <stdio.h>
#include <stddef.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "sbcl.h"

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include "thread.h"             /* dynamic_values_bytes */


#include "validate.h"
size_t os_vm_page_size;

int arch_os_thread_init(struct thread *thread)
{
    {
        void *top_exception_frame;
        void *cur_stack_end;
        void *cur_stack_start;
        MEMORY_BASIC_INFORMATION stack_memory;

        asm volatile ("movl %%fs:0,%0": "=r" (top_exception_frame));
        asm volatile ("movl %%fs:4,%0": "=r" (cur_stack_end));

        /* Can't pull stack start from fs:4 or fs:8 or whatever,
         * because that's only what currently has memory behind
         * it from being used, so do a quick VirtualQuery() and
         * grab the AllocationBase. -AB 2006/11/25
         */

        if (!VirtualQuery(&stack_memory, &stack_memory, sizeof(stack_memory))) {
            fprintf(stderr, "VirtualQuery: 0x%lx.\n", GetLastError());
            lose("Could not query stack memory information.");
        }
        cur_stack_start = stack_memory.AllocationBase;

        /* We use top_exception_frame rather than cur_stack_end to
         * elide the last few (boring) stack entries at the bottom of
         * the backtrace.
         */
        thread->control_stack_start = cur_stack_start;
        thread->control_stack_end = top_exception_frame;

#ifndef LISP_FEATURE_SB_THREAD
        /*
         * Theoretically, threaded SBCL binds directly against
         * the thread structure for these values. We don't do
         * threads yet, but we'll probably do the same. We do
         * need to reset these, though, because they were
         * initialized based on the wrong stack space.
         */
        SetSymbolValue(CONTROL_STACK_START,(lispobj)thread->control_stack_start,thread);
        SetSymbolValue(CONTROL_STACK_END,(lispobj)thread->control_stack_end,thread);
#endif
    }

#ifdef LISP_FEATURE_SB_THREAD
    __asm__ __volatile__ ("movl %0, %%fs:0x14" : : "r" (thread));
#endif

    return 1;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread *thread) {
    return 0;
}

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case reg_EAX: return &context->Eax;
    case reg_ECX: return &context->Ecx;
    case reg_EDX: return &context->Edx;
    case reg_EBX: return &context->Ebx;
    case reg_ESP: return &context->Esp;
    case reg_EBP: return &context->Ebp;
    case reg_ESI: return &context->Esi;
    case reg_EDI: return &context->Edi;
    default: return 0;
    }
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->Eip; /*  REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &context->Esp; /* REG_UESP */
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return &context->Ebp; /* REG_EBP */
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return ((((context->FloatSave.ControlWord) & 0xffff) ^ 0x3f) |
            (((context->FloatSave.StatusWord) & 0xffff) << 16));
}

void
os_restore_fp_control(os_context_t *context)
{
    asm ("fldcw %0" : : "m" (context->FloatSave.ControlWord));
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

extern void planted_trampoline();

void plant_call(HANDLE thread, planted_function_t fn)
{
        CONTEXT context;
        if (SuspendThread(thread) == -1)
        {
                fprintf(stderr, "Unable to suspend thread 0x%p\n", thread);
                return;
        }
        context.ContextFlags = CONTEXT_FULL;
        if (GetThreadContext(thread, &context) == 0)
        {
                fprintf(stderr, "Unable to get thread context for thread 0x%p\n", thread);
                ResumeThread(thread);
                return;
        }

        {
                //planting
                //calling convention is as follow:
                // PUSH unsaved registers
                // PUSH last arg
                // ...
                // PUSH first arg
                // PUSH return address           }
                // SET %EIP to first instruction } CALL does this
                // POP unsaved registers

                // we should save registers (TODO!)

                // we have no args, so don't push

                // pushing return address (the current EIP, which points to the next instruction)
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Eip;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = (int)(void*)fn;

                // PUSHAD
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Eax;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Ecx;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Edx;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Ebx;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Esp - 5 * 4;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Ebp;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Esi;
                context.Esp -= 4;
                *((int*)((void*)context.Esp - 0)) = context.Edi;
                context.Esp -= 4;
                // PUSHFD
                *((int*)((void*)context.Esp - 0)) = context.EFlags;
                // setting %EIP
                context.Eip = (int)(void*)planted_trampoline;
        }
        if (SetThreadContext(thread, &context) == 0)
        {
                fprintf(stderr, "Unable to set thread context for thread 0x%p\n", thread);
                ResumeThread(thread);
                return;
        }

        if (ResumeThread(thread) == -1)
        {
                fprintf(stderr, "Unable to resume thread 0x%p\n", thread);
                return;
        }
        //fprintf(stderr, "Function planted to thread %lu\n", (int)thread);
}
