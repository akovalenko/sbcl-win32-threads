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
#include <errno.h>
#include <string.h>

#include "sbcl.h"
#include "os.h"
#include "interr.h"

/* Except for os_zero, these routines are only called by Lisp code.
 * These routines may also be replaced by os-dependent versions
 * instead. See hpux-os.c for some useful restrictions on actual
 * usage. */

void
os_zero(os_vm_address_t addr, os_vm_size_t length)
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;

#ifdef DEBUG
    fprintf(stderr,";;; os_zero: addr: 0x%08x, len: 0x%08x\n",addr,length);
#endif

    block_start = os_round_up_to_page(addr);

    length -= block_start-addr;
    block_size = os_trunc_size_to_page(length);

    if (block_start > addr)
        memset((char *)addr, 0, block_start-addr);
    if (block_size < length)
        memset((char *)block_start+block_size, 0, length-block_size);

    if (block_size != 0) {
        /* Now deallocate and allocate the block so that it faults in
         * zero-filled. */

        os_invalidate(block_start, block_size);
        addr = os_validate(block_start, block_size);

        if (addr == NULL || addr != block_start)
            lose("os_zero: block moved! 0x%08x ==> 0x%08x\n",
                 block_start,
                 addr);
    }
}

os_vm_address_t
os_allocate(os_vm_size_t len)
{
    return os_validate((os_vm_address_t)NULL, len);
}

void
os_deallocate(os_vm_address_t addr, os_vm_size_t len)
{
    os_invalidate(addr,len);
}

int
os_get_errno(void)
{
    return errno;
}

#if defined(LISP_FEATURE_OS_PROVIDES_DLOPEN)
#include <dlfcn.h>

void* os_dlopen(char* name, int flags) {
	volatile void* ret = dlopen(name,flags);
	return ret;
}

#if defined(LISP_FEATURE_SB_DYNAMIC_CORE)
/* When this feature is enabled, the special category of /static/ foreign
 * symbols disappears. Foreign fixups are resolved to linkage table locations
 * during genesis, and for each of them a record is added to
 * REQUIRED_RUNTIME_C_SYMBOLS list, of the form (cons name datap).
 *
 * Name is a base-string of a symbol name, and non-nil datap marks data
 * references.
 *
 * Before any code in lisp image can be called, we have to resolve all
 * references to runtime foreign symbols that used to be static, adding linkage
 * table entry for each element of REQUIRED_RUNTIME_C_SYMBOLS.
 */

/* We start with a little greenspunning to make car, cdr and base-string data
 * accessible. */

#include "genesis/config.h"
#include "genesis/cons.h"
#include "genesis/vector.h"
#include "genesis/constants.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "thread.h"

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

void os_link_runtime()
{
    lispobj head;
    void *link_target = (void*)(intptr_t)LINKAGE_TABLE_SPACE_START;
    void *validated_end = link_target;
    lispobj symbol_name;
    char *namechars;
    boolean datap;
    void* result;

    for (head = SymbolValue(REQUIRED_RUNTIME_C_SYMBOLS,0);
         head!=NIL; head = cdr(head))
    {
        lispobj item = car(head);
        symbol_name = car(item);
        datap = (NIL!=(cdr(item)));
        namechars = (void*)(intptr_t)FOTHERPTR(symbol_name,vector).data;
        result = os_dlsym_default(namechars);
        if (link_target == validated_end) {
            validated_end += os_vm_page_size;
            os_validate_recommit(link_target,os_vm_page_size);
        }
        if (result) {
            if (datap)
                arch_write_linkage_table_ref(link_target,result);
            else
                arch_write_linkage_table_jmp(link_target,result);
        }
        link_target = (void*)(((uintptr_t)link_target)+LINKAGE_TABLE_ENTRY_SIZE);
    }
}
#endif  /* sb-dynamic-core */
#endif  /* os-provides-dlopen */
