#ifndef SBCL_INCLUDED_OS_DEFAULT_H
#define SBCL_INCLUDED_OS_DEFAULT_H

#ifndef OS_VM_MMAP_GRANULARITY_SHIFT
#define OS_VM_MMAP_GRANULARITY_SHIFT 1
#endif

#define OS_VM_MMAP_UNIT_SIZE (os_vm_page_size<<(OS_VM_MMAP_GRANULARITY_SHIFT))

#ifndef HAVE_os_open_core
#define os_open_core(filename,mode) open(filename,mode)
#endif

#ifndef HAVE_os_invalidate_free
#define os_invalidate_free os_invalidate
#endif

#ifndef HAVE_os_validate_commit
#define os_validate_commit os_validate
#endif

#ifndef HAVE_os_allocate_lazily
#define os_allocate_lazily os_validate
#endif

#endif /* SBCL_INCLUDED_OS_DEFAULT_H */
