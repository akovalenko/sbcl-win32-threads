#include "sbcl.h"

#if defined(LISP_FEATURE_SB_THREAD)
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0500
#endif
#include "pthreads_win32.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

int pthread_attr_init(pthread_attr_t *attr)
{
  attr->stack_size = 0;
  return 0;
}

int pthread_attr_destroy(pthread_attr_t *attr)
{
  return 0;
}

int pthread_attr_setstack(pthread_attr_t *attr, void *stackaddr, size_t stacksize)
{
  fprintf(stderr, "pthread_attr_setstack called\n");
  ExitProcess(1);
  return 0;
}

int pthread_attr_setstacksize(pthread_attr_t *attr, size_t stacksize)
{
  attr->stack_size = stacksize;
  return 0;
}

DWORD thread_self_tls_index;

typedef unsigned char boolean;

/* Some parts of pthread_np API provide access to Windows NT Fibers
   (cooperatively scheduled coroutines). Each fiber is wrapped in its
   own pthread.

   Fibers may be entered by different threads during their lifetime,
   i.e. they are orthogonal to threads.

   */
int pthread_np_is_my_other_fiber(pthread_t thread)
{
  pthread_t self = pthread_self();
  return self &&
    (self != thread) &&
    ((self->handle == thread->handle));
}

void pthread_np_suspend(pthread_t thread)
{
  CONTEXT context;
  if (pthread_np_is_my_other_fiber(thread)) {
    ++thread->suspend_count;
  } else {
    SuspendThread(thread->handle);
    context.ContextFlags = CONTEXT_FULL;
    GetThreadContext(thread->handle, &context);
  }
}

int pthread_np_get_thread_context(pthread_t thread, CONTEXT* context)
{
  context->ContextFlags = CONTEXT_FULL;
  if (thread->fiber_context) {
    *context = *thread->fiber_context;
    return 1;
  } else {
    return GetThreadContext(thread->handle, context) != 0;
  }
}

void pthread_np_resume(pthread_t thread)
{
  if (pthread_np_is_my_other_fiber(thread)) {
    if (thread->suspend_count)
      --thread->suspend_count;
  } else {
    ResumeThread(thread->handle);
  }
}

void pthread_np_request_interruption(pthread_t thread)
{
  if (thread->waiting_cond) {
    pthread_cond_broadcast(thread->waiting_cond);
  }
}

pthread_t pthread_self()
{
  return (pthread_t)TlsGetValue(thread_self_tls_index);
}

const char * state_to_str(pthread_thread_state state)
{
  switch (state) {
    case pthread_state_running: return "running";
    case pthread_state_finished: return "finished";
    case pthread_state_joined: return "joined";
  }
}

DWORD WINAPI Thread_Function(LPVOID param)
{
  pthread_t self = (pthread_t)param;
  void* arg = self->arg;
  pthread_fn fn = self->start_routine;
  void* fiber;
  int own_fiber;
  void* fiber_parent;
  HANDLE handle = self->handle;

  self->teb = NtCurrentTeb();
  TlsSetValue(thread_self_tls_index, self);
  self->retval = fn(arg);
  pthread_mutex_lock(&self->lock);
  self->state = pthread_state_finished;
  pthread_cond_broadcast(&self->cond);
  while (!self->detached && self->state != pthread_state_joined) {
    pthread_cond_wait(&self->cond, &self->lock);
  }
  pthread_mutex_unlock(&self->lock);

  pthread_mutex_destroy(&self->lock);
  pthread_cond_destroy(&self->cond);
  fiber = self->fiber;
  own_fiber = self->own_fiber;
  fiber_parent = self->fiber_parent;
  if (self->fiber_cleanup_pointer) {
    *self->fiber_cleanup_pointer = fiber;
  }
  if (self->cleanup_callback) {
    /* pthread itself is still valid, as is its handle */
    self->cleanup_callback(self->cleanup_context);
  }
  free(self);
  if (fiber_parent) {
    SwitchToFiber(fiber_parent);
    /* Should NEVER be reentered */
    return 0;
  }
  /* Close our thread handle. Fibers that have fiber_parent don't do
     it, but non-fiber threads and lone fibers without fiber_parent
     do. */
  CloseHandle(handle);
  /* If thread was converted to fiber during its lifetime, this fiber
     is now deleted. Fiber stack is freed, and thread running it
     exits, as in ExitThread. */
  if (fiber && own_fiber) {
    DeleteFiber(fiber);
  }
  return 0;
}

VOID CALLBACK Fiber_Function(LPVOID param)
{
  Thread_Function(param);
}

int pthread_create(pthread_t *thread, const pthread_attr_t *attr, void *(*start_routine) (void *), void *arg)
{
  pthread_t pth = (pthread_t)malloc(sizeof(pthread_thread));
  pthread_t self = pthread_self();
  int i;
  HANDLE createdThread = NULL;

  if (self->fiber_factory) {
    pth->fiber = CreateFiber (attr ? attr->stack_size : 0, Fiber_Function, pth);
    if (!pth->fiber) return 1;
    pth->teb = self->teb;
    pth->handle = self->handle;
    pth->suspend_count = 1;
    pth->fiber_parent = self->fiber;
  } else {
    createdThread = CreateThread(NULL, attr ? attr->stack_size : 0,
                                 Thread_Function, pth, CREATE_SUSPENDED, NULL);
    if (!createdThread) return 1;
    pth->fiber = NULL;
    pth->teb = NULL;
    pth->suspend_count = 0;
    pth->fiber_parent = NULL;
    pth->handle = createdThread;
  }
  pth->fiber_factory = 0;
  pth->start_routine = start_routine;
  pth->arg = arg;
  pth->waiting_cond = NULL;
  pth->fiber_context = NULL;
  pth->fiber_cleanup_pointer = NULL;
  pth->cleanup_callback = NULL;
  pth->cleanup_context = NULL;
  pth->fiber_callback = NULL;
  pth->fiber_context = NULL;
  if (self) {
    pth->blocked_signal_set = self->blocked_signal_set;
  } else {
    sigemptyset(&pth->blocked_signal_set);
  }
  for (i = 1; i < NSIG; ++i)
    pth->signal_is_pending[i] = 0;
  pth->state = pthread_state_running;
  pthread_mutex_init(&pth->lock, NULL);
  pthread_cond_init(&pth->cond, NULL);
  pth->detached = 0;
  if (thread) *thread = pth;
  pthread_np_resume(pth);
  if (pth->fiber) {
    if (pthread_np_switch_to_fiber(pth)<0)
      return 1;
  }
  return 0;
}

int pthread_equal(pthread_t thread1, pthread_t thread2)
{
  return thread1 == thread2;
}

int pthread_detach(pthread_t thread)
{
  int retval = 0;
  pthread_mutex_lock(&thread->lock);
  thread->detached = 1;
  pthread_cond_broadcast(&thread->cond);
  pthread_mutex_unlock(&thread->lock);
  return retval;
}

int pthread_join(pthread_t thread, void **retval)
{
  pthread_mutex_lock(&thread->lock);
  while (thread->state != pthread_state_finished) {
    pthread_cond_wait(&thread->cond, &thread->lock);
  }
  thread->state = pthread_state_joined;
  pthread_cond_broadcast(&thread->cond);
  if (retval)
    *retval = thread->retval;
  pthread_mutex_unlock(&thread->lock);
  return 0;
}

int pthread_key_create(pthread_key_t *key, void (*destructor)(void*))
{
  DWORD index;
  if (destructor) {
    fprintf(stderr, "destructor is specified for pthread_key_create\n");
    ExitProcess(1);
  }
  index = TlsAlloc();
  if (index == TLS_OUT_OF_INDEXES)
    return 1;
  *key = index;
  return 0;
}

void *pthread_getspecific(pthread_key_t key)
{
  return TlsGetValue(key);
}

int pthread_setspecific(pthread_key_t key, const void *value)
{
  return TlsSetValue(key, (LPVOID)value) != FALSE;
}

int pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
  pthread_t self = pthread_self();
  if (oldset)
    *oldset = self->blocked_signal_set;
  if (set) {
    const char * action;

    switch (how) {
      case SIG_BLOCK:
        action = "blocking";
        self->blocked_signal_set |= *set;
        break;
      case SIG_UNBLOCK:
        action = "unblocking";
        self->blocked_signal_set &= ~(*set);
        break;
      case SIG_SETMASK:
        action = "setting";
        self->blocked_signal_set = *set;
        break;
    }
    if (0)
    {
      char buf[100];
      sprintf(buf, "[0x%p] set signals mask to 0x%x by %s of 0x%x", self, self->blocked_signal_set, action, *set);
      OutputDebugString(buf);
    }
  }
  if (set)
  {
    int i;
    for (i = 1; i < NSIG; ++i) {
      if (!sigismember(&self->blocked_signal_set, i)) {
        unsigned int is_pending = InterlockedExchange(&self->signal_is_pending[i], 0);
        if (is_pending) {
          pthread_np_pending_signal_handler(i);
        }
      }
    }
  }
  return 0;
}

pthread_mutex_t mutex_init_lock;

int pthread_mutex_init(pthread_mutex_t * mutex, const pthread_mutexattr_t * attr)
{
  *mutex = (CRITICAL_SECTION*)malloc(sizeof(CRITICAL_SECTION));
  InitializeCriticalSection(*mutex);
  return 0;
}

int pthread_mutexattr_init(pthread_mutexattr_t* attr)
{
  return 0;
}
int pthread_mutexattr_destroy(pthread_mutexattr_t* attr)
{
  return 0;
}

int pthread_mutexattr_settype(pthread_mutexattr_t* attr,int mutex_type)
{
  return 0;
}

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
  if (*mutex != PTHREAD_MUTEX_INITIALIZER)
    DeleteCriticalSection(*mutex);
  return 0;
}

void pthread_np_add_pending_signal(pthread_t thread, int signum)
{
  const char * s = thread->signal_is_pending[signum] ? "pending" : "not pending";
  thread->signal_is_pending[signum] = 1;
}

void pthread_np_remove_pending_signal(pthread_t thread, int signum)
{
  const char * s = thread->signal_is_pending[signum] ? "pending" : "not pending";
  thread->signal_is_pending[signum] = 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      *mutex = (CRITICAL_SECTION*)malloc(sizeof(CRITICAL_SECTION));
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  EnterCriticalSection(*mutex);
  return 0;
}

int pthread_mutex_trylock(pthread_mutex_t *mutex)
{
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      *mutex = (CRITICAL_SECTION*)malloc(sizeof(CRITICAL_SECTION));
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if (TryEnterCriticalSection(*mutex))
    return 0;
  else
    return EBUSY;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
  LeaveCriticalSection(*mutex);
  return 0;
}

static HANDLE cv_default_event_get_fn()
{
  return CreateEvent(NULL, FALSE, FALSE, NULL);
}

static void cv_default_event_return_fn(HANDLE event)
{
  CloseHandle(event);
}

int pthread_cond_init(pthread_cond_t * cv, const pthread_condattr_t * attr)
{
  pthread_mutex_init(&cv->wakeup_lock, NULL);
  cv->first_wakeup = NULL;
  cv->last_wakeup = NULL;
  cv->alertable = 0;
  cv->get_fn = cv_default_event_get_fn;
  cv->return_fn = cv_default_event_return_fn;
  return 0;
}

int pthread_cond_destroy(pthread_cond_t *cv)
{
  pthread_mutex_destroy(&cv->wakeup_lock);
  return 0;
}

int pthread_cond_broadcast(pthread_cond_t *cv)
{
  int count = 0;
  pthread_mutex_lock(&cv->wakeup_lock);
  while (cv->first_wakeup)
  {
    struct thread_wakeup * w = cv->first_wakeup;
    HANDLE waitevent = w->event;
    cv->first_wakeup = w->next;
    SetEvent(waitevent);
    ++count;
  }
  cv->last_wakeup = NULL;
  pthread_mutex_unlock(&cv->wakeup_lock);
  return 0;
}

int pthread_cond_signal(pthread_cond_t *cv)
{
  struct thread_wakeup * w;
  pthread_mutex_lock(&cv->wakeup_lock);
  w = cv->first_wakeup;
  if (w) {
    HANDLE waitevent = w->event;
    cv->first_wakeup = w->next;
    if (!cv->first_wakeup)
      cv->last_wakeup = NULL;
    SetEvent(waitevent);
  }
  pthread_mutex_unlock(&cv->wakeup_lock);
  return 0;
}

void cv_wakeup_add(struct pthread_cond_t* cv, struct thread_wakeup* w)
{
  w->event = cv->get_fn();
  w->next = NULL;
  pthread_mutex_lock(&cv->wakeup_lock);
  if (cv->last_wakeup == w) {
    fprintf(stderr, "cv->last_wakeup == w\n");
    ExitProcess(0);
  }
  if (cv->last_wakeup != NULL)
  {
    cv->last_wakeup->next = w;
    cv->last_wakeup = w;
  }
  else
  {
    cv->first_wakeup = w;
    cv->last_wakeup = w;
  }
  pthread_mutex_unlock(&cv->wakeup_lock);
}

void cv_wakeup_remove(struct pthread_cond_t* cv, struct thread_wakeup* w)
{
  pthread_mutex_lock(&cv->wakeup_lock);
  {
    if (cv->first_wakeup == w) {
      cv->first_wakeup = w->next;
      if (cv->last_wakeup == w)
        cv->last_wakeup = NULL;
    } else {
      struct thread_wakeup * prev = cv->first_wakeup;
      while (prev && prev->next != w)
        prev = prev->next;
      if (!prev) {
        pthread_mutex_unlock(&cv->wakeup_lock);
        return;
      }
      prev->next = w->next;
      if (cv->last_wakeup == w)
        cv->last_wakeup = prev;
    }
  }
  pthread_mutex_unlock(&cv->wakeup_lock);
}

int pthread_cond_wait(pthread_cond_t * cv, pthread_mutex_t * cs)
{
  struct thread_wakeup w;
  cv_wakeup_add(cv, &w);
  if (cv->last_wakeup->next == cv->last_wakeup) {
    fprintf(stderr, "cv->last_wakeup->next == cv->last_wakeup\n");
    ExitProcess(0);
  }
  if (cv->last_wakeup->next != NULL) {
    fprintf(stderr, "cv->last_wakeup->next != NULL\n");
    ExitProcess(0);
  }
  pthread_self()->waiting_cond = cv;
  pthread_mutex_unlock(cs);
  if (cv->alertable) {
    while (WaitForSingleObjectEx(w.event, INFINITE, TRUE) == WAIT_IO_COMPLETION);
  } else {
    WaitForSingleObject(w.event, INFINITE);
  }
  pthread_self()->waiting_cond = NULL;
  cv->return_fn(w.event);
  pthread_mutex_lock(cs);
  return 0;
}

int pthread_cond_timedwait(pthread_cond_t * cv, pthread_mutex_t * cs, const struct timespec * abstime)
{
  DWORD rv;
  struct thread_wakeup w;
  cv_wakeup_add(cv, &w);
  if (cv->last_wakeup->next == cv->last_wakeup) {
    fprintf(stderr, "cv->last_wakeup->next == cv->last_wakeup\n");
    ExitProcess(0);
  }
  pthread_self()->waiting_cond = cv;
  pthread_mutex_unlock(cs);
  {
    struct timeval cur_tm;
    long sec, msec;
    gettimeofday(&cur_tm, NULL);
    sec = abstime->tv_sec - cur_tm.tv_sec;
    msec = sec * 1000 + abstime->tv_nsec / 1000000 - cur_tm.tv_usec / 1000;
    if (msec < 0)
      msec = 0;
    if (cv->alertable) {
      while ((rv = WaitForSingleObjectEx(w.event, msec, TRUE)) == WAIT_IO_COMPLETION);
    } else {
      rv = WaitForSingleObject(w.event, msec);
    }
  }
  pthread_self()->waiting_cond = NULL;
  if (rv == WAIT_TIMEOUT)
    cv_wakeup_remove(cv, &w);
  cv->return_fn(w.event);
  pthread_mutex_lock(cs);
  if (rv == WAIT_TIMEOUT)
    return ETIMEDOUT;
  else
    return 0;
}

int sched_yield()
{
  SwitchToThread();
  return 0;
}

void pthread_lock_structures()
{
  pthread_mutex_lock(&mutex_init_lock);
}

void pthread_unlock_structures()
{
  pthread_mutex_unlock(&mutex_init_lock);
}

void pthreads_win32_init()
{
  pthread_t pth = (pthread_t)malloc(sizeof(pthread_thread));
  thread_self_tls_index = TlsAlloc();
  pth->start_routine = NULL;
  pth->arg = NULL;
  pth->waiting_cond = NULL;
  pth->fiber_factory = 0;
  pth->fiber = NULL;
  pth->fiber_parent = NULL;
  pth->fiber_cleanup_pointer = NULL;
  pth->suspend_count = 0;
  pth->fiber_context = NULL;
  pth->cleanup_callback = NULL;
  pth->cleanup_context = NULL;
  pth->fiber_callback = NULL;
  pth->fiber_context = NULL;
  pth->teb = NtCurrentTeb();
  sigemptyset(&pth->blocked_signal_set);
  {
    int i;
    for (i = 1; i < NSIG; ++i)
      pth->signal_is_pending[i] = 0;
  }
  pthread_mutex_init(&mutex_init_lock, NULL);
  DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                  GetCurrentProcess(), &pth->handle, 0,
                  TRUE, DUPLICATE_SAME_ACCESS);
  TlsSetValue(thread_self_tls_index, pth);
}

VOID CALLBACK pthreads_win32_unnotice(void* parameter, BOOLEAN timerOrWait)
{
  pthread_t pth = parameter;
  if (pth->cleanup_callback)
    pth->cleanup_callback(pth->cleanup_context);
  CloseHandle(pth->handle);
  if (pth->fiber && pth->own_fiber) {
    DeleteFiber(pth->fiber);
  }
  UnregisterWait(pth->wait_handle);
  free(pth);
}

int pthread_np_notice_thread()
{
  if (!pthread_self()) {
    pthread_t pth = (pthread_t)malloc(sizeof(pthread_thread));
    pth->start_routine = NULL;
    pth->arg = NULL;
    pth->waiting_cond = NULL;
    pth->fiber_factory = 0;
    pth->fiber = NULL;
    pth->fiber_parent = NULL;
    pth->fiber_cleanup_pointer = NULL;
    pth->suspend_count = 0;
    pth->fiber_context = NULL;
    pth->teb = NtCurrentTeb();
    pth->cleanup_callback = NULL;
    pth->cleanup_context = NULL;
    pth->fiber_callback = NULL;
    pth->fiber_callback_context = NULL;

    sigemptyset(&pth->blocked_signal_set);
    {
      int i;
      for (i = 1; i < NSIG; ++i)
        pth->signal_is_pending[i] = 0;
    }
    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                    GetCurrentProcess(), &pth->handle, 0, TRUE,
                    DUPLICATE_SAME_ACCESS);
    TlsSetValue(thread_self_tls_index, pth);

    RegisterWaitForSingleObject(&pth->wait_handle,
                                pth->handle,
                                pthreads_win32_unnotice,
                                pth,
                                INFINITE,
                                WT_EXECUTEONLYONCE);
    return 1;
  } else {
    return 0;
  }
}

int pthread_np_convert_self_to_fiber()
{
  pthread_t pth = pthread_self();
  if (!pth)
    return 1;
  if (!pth->fiber) {
    void* fiber = GetCurrentFiber();
    /* Beware: undocumented (but widely used) method below to check if
       the thread is already converted. */
    if (fiber != NULL && fiber != (void*)0x1E00) {
      pth->fiber = fiber;
      pth->own_fiber = 0;
    } else {
      pth->fiber = ConvertThreadToFiber(pth);
      pth->own_fiber = 1;
    }
    if (!pth->fiber)
      return 1;
  }
  return 0;
}

int pthread_np_set_fiber_factory_mode(int on)
{
  pthread_t pth = pthread_self();
  if (on && pthread_np_convert_self_to_fiber()) {
    return 1;
  }
  pth->fiber_factory = on;
  return 0;
}

#define MAX_SAVED_TLS_SLOTS 64

DWORD tls_fiber_save_map[(MAX_SAVED_TLS_SLOTS+31)/32] = {0}; /* MAX 64 TLS slots */

#define FIBER_RUNNING 0
#define FIBER_EXITED (-1)

int pthread_np_switch_to_fiber(pthread_t pth)
{
  pthread_t self = pthread_self();
  CONTEXT ctx;
  LPVOID saved_tls[MAX_SAVED_TLS_SLOTS];
  LPVOID* real_tls = (0xE10 + self->teb);
  void* clean_fiber = NULL;
  int i;

 again:
  if (!pthread_np_is_my_other_fiber(pth))    /* Not my fiber */
    return -1;
  if (pth->suspend_count)                 /* Suspended */
    return -1;
  if (pthread_np_convert_self_to_fiber()) /* Can't become a fiber */
    return -1;

  pth->fiber_cleanup_pointer = &clean_fiber;
  for (i=0; i<MAX_SAVED_TLS_SLOTS; ++i) {
    if (tls_fiber_save_map[i/32]&(1u<<(i%32))) {
      saved_tls[i] = real_tls[i];
      /* If it's the first switch, thread expect zeroes in TLS.
       Otherwise, it will restore its own TLS and zeroing does no harm. */
      real_tls[i] = 0;
    }
  }
  pthread_np_get_thread_context(self,&ctx);
  self->fiber_context = &ctx;
  TlsSetValue(thread_self_tls_index, pth);
  SwitchToFiber(pth->fiber);
  /* When we return here... */
  TlsSetValue(thread_self_tls_index, self);
  /* I am probably an idiot: lost some hours on tracking this down.
     After switching to other fiber, TEB may be changed: that's the
     whole point of saving and restoring TLS slots.

     Therefore, let real_tls point at _new_ TEB TLS data. */
  real_tls = (0xE10 + self->teb);
  for (i=0; i<MAX_SAVED_TLS_SLOTS; ++i) {
    if (tls_fiber_save_map[i/32]&(1u<<(i%32))) {
      real_tls[i] = saved_tls[i];
    }
  }
  self->fiber_context = NULL;

  if (clean_fiber) {
    DeleteFiber(clean_fiber);
    i = 1;
  } else {
    pth->fiber_cleanup_pointer = NULL;
    i = 0;
  }
  if (self->fiber_callback) {
    pthread_t (*cb)(void*) = self->fiber_callback;
    void *ctx = self->fiber_callback_context;
    self->fiber_callback = NULL;
    self->fiber_callback_context = NULL;
    {
      pthread_t back = cb(ctx);
      if (back) {
        pth = back;
        goto again;
      }
    }
  }
  return i;
}

int pthread_np_run_in_fiber(pthread_t pth, pthread_t (*callback)(void*),
                            void* context)
{
  if (!pthread_np_is_my_other_fiber(pth))    /* Not my fiber */
    return -1;
  pth->fiber_callback = callback;
  pth->fiber_callback_context = context;
  return pthread_np_switch_to_fiber(pth);
}

void pthread_np_set_cleanup(pthread_t thread, void (*cleaner)(void*),void* context)
{
  thread->cleanup_callback = cleaner;
  thread->cleanup_context = context;
}

int pthread_np_fiber_save_tls(int slot, int enable)
{
  if (slot < 0 || slot >= MAX_SAVED_TLS_SLOTS)
    return 1;
  if (enable)
    tls_fiber_save_map[slot/32]|=(1u<<(slot%32));
  else
    tls_fiber_save_map[slot/32]&=~(1u<<(slot%32));
  return 0;
}

HANDLE pthread_np_get_handle(pthread_t pth)
{
  return pth->handle;
}

void* pthread_np_get_lowlevel_fiber(pthread_t pth)
{
  return pth->fiber;
}

int pthread_np_delete_lowlevel_fiber(void* fiber)
{
  DeleteFiber(fiber);
  return 0;
}

/* Current thread's non-current fibers may be donated to other
   fibers, possibly in other threads.

   Unnoticed threads (without pthread structure) may donate any
   fibers: it's assumed that they're cleaning up after orphaned
   pthreads. */

int pthread_np_donate_fiber(pthread_t fiber, pthread_t recipient)
{
  if (pthread_self() && !pthread_np_is_my_other_fiber(fiber))
    return -1;

  fiber->fiber_parent = recipient->fiber;
  fiber->teb = recipient->teb;
  fiber->handle = recipient->handle;
  return 0;
}

int sigemptyset(sigset_t *set)
{
  *set = 0;
  return 0;
}

int sigfillset(sigset_t *set)
{
  *set = 0xfffffffful;
  return 0;
}

int sigaddset(sigset_t *set, int signum)
{
  *set |= 1 << signum;
  return 0;
}

int sigdelset(sigset_t *set, int signum)
{
  *set &= ~(1 << signum);
  return 0;
}

int sigismember(const sigset_t *set, int signum)
{
  return (*set & (1 << signum)) != 0;
}
#endif
