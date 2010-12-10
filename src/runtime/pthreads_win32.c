#include "sbcl.h"

#if defined(LISP_FEATURE_SB_THREAD)
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0500
#endif
#define PTHREAD_INTERNALS
#include "pthreads_win32.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#ifdef PTHREAD_DEBUG_OUTPUT
#define pthshow(fmt,...)                        \
  do {                                          \
  fprintf(stderr,fmt "\n", __VA_ARGS__);        \
  fflush(stderr);                               \
  } while (0)
#else
#define pthshow(fmt,...) do {} while (0)
#endif

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


typedef unsigned char boolean;

/* TLS management internals */

static DWORD thread_self_tls_index;
static void (*tls_destructors[PTHREAD_KEYS_MAX])(void*);
static boolean tls_used[PTHREAD_KEYS_MAX];
static pthread_key_t tls_max_used_key;
static pthread_mutex_t thread_key_lock = PTHREAD_MUTEX_INITIALIZER;
static void tls_call_destructors();
static pthread_t tls_impersonate(pthread_t other) {
  pthread_t old = pthread_self();
  TlsSetValue(thread_self_tls_index,other);
  return old;
}

void pthread_np_get_my_context_subset(CONTEXT* ctx)
{
  ctx->ContextFlags = CONTEXT_FULL;
  ctx->Eip = pthread_np_get_my_context_subset;
  asm volatile ("mov %%eax,%0": "=m"(ctx->Eax));
  asm volatile ("mov %%ebx,%0": "=m"(ctx->Ebx));
  asm volatile ("mov %%ecx,%0": "=m"(ctx->Ecx));
  asm volatile ("mov %%edx,%0": "=m"(ctx->Edx));
  asm volatile ("mov %%esp,%0": "=m"(ctx->Esp));
  asm volatile ("mov %%ebp,%0": "=m"(ctx->Ebp));
  asm volatile ("mov %%esi,%0": "=m"(ctx->Esi));
  asm volatile ("mov %%edi,%0": "=m"(ctx->Edi));
}

static void do_nothing() {}
/* Fiber context hooks */
void (*pthread_save_context_hook)() = do_nothing;
void (*pthread_restore_context_hook)() = do_nothing;

/* Some parts of pthread_np API provide access to Windows NT Fibers
   (cooperatively scheduled coroutines). Each fiber is wrapped in its
   own pthread.

   Fibers may be entered by different threads during their lifetime,
   i.e. they are orthogonal to threads.

   Contrary to the raw NT Fibers API, we will distinguish two kinds of
   objects: fibers-created-as-fibers and any other thing (thread that
   is not a fiber, thread converted to fiber, system thread
   noticed). Consequently, though there is no "main fiber" in NT,
   there _is_ a main pthread for each (wrapped) system thread, living
   or dying with this system thread. It may be converted to fiber, but
   its "fiberness" is incidental, only to be able to switch into
   another fibers or create them.

   Any fiber that is currently running belongs to some thread
   (fiber-created-as-thread, to be exact). Call it FCAT group.

   [1] Entrance lock: prevent double entry.

   [2] Suspend for fibers -> "try locking entrance lock; if failed, do
   real thread suspend"

   [3] Resume for fibers -> two strategies depending on what [2] done.

   [4] Exit/death for fibers -> switch to its FCAT group.

   [2],[3],[4] doesn't apply to threads-converted-to-fibers: full
   stop/resume is always done on them (?).
*/

void pthread_np_suspend(pthread_t thread)
{
  CONTEXT context;
  pthread_mutex_lock(&thread->fiber_lock);
  /* If thread is about to run another fiber, fiber_context should be
     non-NULL. No point in really suspending it, as it won't be
     reentered without taking fiber_lock. */
  if (!thread->fiber_context.Esp) {
    /* For ANY fiber without saved context record, fiber_group should
       be present. */
    fprintf(stderr,"Had to really suspend thread #x%p\n",thread);
    fflush(stderr);
    SuspendThread(thread->fiber_group->handle);
    context.ContextFlags = CONTEXT_FULL;
    GetThreadContext(thread->fiber_group->handle, &context);
  }
}

int pthread_np_get_thread_context(pthread_t thread, CONTEXT* context)
{
  context->ContextFlags = CONTEXT_FULL;
  if (thread->fiber_context.Esp) {
    *context = thread->fiber_context;
    return 1;
  } else {
    return GetThreadContext(thread->fiber_group->handle, context) != 0;
  }
}

CONTEXT* pthread_np_publish_context(CONTEXT* maybe_save_old_one)
{
  pthread_t self = pthread_self();

  if (maybe_save_old_one && self->fiber_context.Esp)
    *maybe_save_old_one = self->fiber_context;
  pthread_np_get_my_context_subset(&self->fiber_context);
  self->fiber_context.Esp = __builtin_frame_address(0);
  self->fiber_context.Eip = __builtin_return_address(0);
  return &self->fiber_context;
}

void pthread_np_unpublish_context()
{
  pthread_t self = pthread_self();
  self->fiber_context.Esp = 0;
}

void pthread_np_resume(pthread_t thread)
{
  if (!thread->fiber_context.Esp) {
    ResumeThread(thread->fiber_group->handle);
  }
  pthread_mutex_unlock(&thread->fiber_lock);
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
  default: return "unknown";
  }
}

static void thread_or_fiber_function(pthread_t self)
{
  pthread_t prev = tls_impersonate(self);
  void* arg = self->arg;
  pthread_fn fn = self->start_routine;

  if (prev) {
    pthread_mutex_lock(&prev->fiber_lock);
    prev->fiber_group = NULL;
    /* Previous fiber, that started us, had assigned our
       fiber_group. Now we clear its fiber_group. */
    pthread_mutex_unlock(&prev->fiber_lock);
  }
  self->retval = fn(arg);
  pthread_mutex_lock(&self->lock);
  self->state = pthread_state_finished;
  pthread_cond_broadcast(&self->cond);
  while (!self->detached && self->state != pthread_state_joined) {
    if (self->created_as_fiber) {
      pthread_mutex_unlock(&self->lock);
      pthread_np_switch_to_fiber(self->fiber_group);
      pthread_mutex_lock(&self->lock);
    } else {
      pthread_cond_wait(&self->cond, &self->lock);
    }
  }
  pthread_mutex_unlock(&self->lock);
  pthread_mutex_destroy(&self->lock);
  pthread_mutex_destroy(&self->fiber_lock);
  pthread_cond_destroy(&self->cond);
  tls_call_destructors();
}

DWORD WINAPI Thread_Function(LPVOID param)
{
  pthread_t self = (pthread_t) param;

  self->teb = NtCurrentTeb();
  thread_or_fiber_function(param);
  CloseHandle(self->handle);
  {
    void* fiber = self->fiber;
    free(self);
    if (fiber) {
      DeleteFiber(GetCurrentFiber()); /* Exits */
    }
  }
  return 0;
}

static void fiber_destructor(void* fiber) { DeleteFiber(fiber); }

VOID CALLBACK Fiber_Function(LPVOID param)
{
  pthread_t self = (pthread_t) param;
  thread_or_fiber_function(param);
  {
    /* fiber_group is a main thread into which we are to call */
    pthread_t group = self->fiber_group;
    free(self);
    tls_impersonate(NULL);
    if (group) {
      /* `Call and continue' semantics */
      pthread_np_run_in_fiber(group, fiber_destructor, GetCurrentFiber());
    }
    DeleteFiber(GetCurrentFiber()); /* Exits */
  }
}

/* Signals */
struct sigaction signal_handlers[NSIG];

int sigaction(int signum, const struct sigaction* act, struct sigaction* oldact)
{
  struct sigaction newact = *act;
  if (oldact)
    *oldact = signal_handlers[signum];
  if (!(newact.sa_flags & SA_SIGINFO)) {
    newact.sa_sigaction = newact.sa_handler;
  }
  signal_handlers[signum] = newact;
  return 0;
}

int pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                   void *(*start_routine) (void *), void *arg)
{
  pthread_t pth = (pthread_t)calloc(sizeof(pthread_thread),1);
  pthread_t self = pthread_self();
  int i;
  HANDLE createdThread = NULL;

  if (self && self->fiber_factory) {
    pth->fiber = CreateFiber (attr ? attr->stack_size : 0, Fiber_Function, pth);
    if (!pth->fiber) return 1;
    pth->created_as_fiber = 1;
    /* Has no fiber-group until someone enters it (we will) */
  } else {
    createdThread = CreateThread(NULL, attr ? attr->stack_size : 0,
                                 Thread_Function, pth, CREATE_SUSPENDED, NULL);
    if (!createdThread) return 1;
    /* FCAT is its own fiber-group [initially] */
    pth->fiber_group = pth;
    pth->handle = createdThread;
  }
  pth->start_routine = start_routine;
  pth->arg = arg;
  if (self) {
    pth->blocked_signal_set = self->blocked_signal_set;
  } else {
    sigemptyset(&pth->blocked_signal_set);
  }
  for (i = 1; i < NSIG; ++i)
    pth->signal_is_pending[i] = 0;
  pth->state = pthread_state_running;
  pthread_mutex_init(&pth->lock, NULL);
  pthread_mutex_init(&pth->fiber_lock, NULL);
  pthread_cond_init(&pth->cond, NULL);
  pth->detached = 0;
  if (thread) *thread = pth;
  if (pth->fiber) {
    pthread_np_switch_to_fiber(pth);
  } else {
    /* Resume will unlock, so we lock here */
    pthread_mutex_lock(&pth->fiber_lock);
    pthread_np_resume(pth);
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
  int fiberp = thread->created_as_fiber;
  pthread_mutex_lock(&thread->lock);
  while (thread->state != pthread_state_finished) {
    if (fiberp) {
      /* just trying */
      pthread_mutex_unlock(&thread->lock);
      pthread_np_switch_to_fiber(thread);
      pthread_mutex_lock(&thread->lock);
    } else {
      pthread_cond_wait(&thread->cond, &thread->lock);
    }
  }
  thread->state = pthread_state_joined;
  pthread_cond_broadcast(&thread->cond);
  if (retval)
    *retval = thread->retval;
  pthread_mutex_unlock(&thread->lock);
  if (fiberp)
    pthread_np_switch_to_fiber(thread);
  return 0;
}

/* Manage our own TLS. */
int pthread_key_create(pthread_key_t *key, void (*destructor)(void*))
{
  pthread_key_t index;
  boolean success = 0;
  pthread_mutex_lock(&thread_key_lock);
  for (index = 0; index < PTHREAD_KEYS_MAX; ++index) {
    if (!tls_used[index]) {
      if (tls_max_used_key<index)
        tls_max_used_key = index;
      tls_destructors[index] = destructor;
      tls_used[index] = 1;
      success = 1;
      break;
    }
  }
  pthread_mutex_unlock(&thread_key_lock);

  if (success) {
    *key = index;
    return 0;
  } else {
    return 1;
  }
}

int pthread_key_delete(pthread_key_t key)
{
  /* tls_used flag is not a machine word. Let's lock, as there is no
     atomic guarantee even on x86.  */
  pthread_mutex_lock(&thread_key_lock);
  tls_destructors[key] = 0;
  /* No memory barrier here: application is responsible for proper
     call sequence, and having the key around at this point is an
     official UB.  */
  tls_used[key] = 0;
  pthread_mutex_unlock(&thread_key_lock);
  return 0;
}

static void tls_call_destructors()
{
  pthread_key_t key;
  int i;
  int called;

  for (i = 0; i<PTHREAD_DESTRUCTOR_ITERATIONS; ++i) {
    called = 0;
    for (key = 0; key<=tls_max_used_key; ++key) {
      void *cell = pthread_getspecific(key);
      pthread_setspecific(key,NULL);
      if (cell && tls_destructors[key]) {
        (tls_destructors[key])(cell);
        called = 1;
      }
    }
    if (!called)
      break;
  }
}

pthread_mutex_t once_mutex = PTHREAD_MUTEX_INITIALIZER;

int pthread_once(pthread_once_t *once_control, void (*init_routine)(void))
{
  if (PTHREAD_ONCE_INIT == *once_control) {
    pthread_mutex_lock(&once_mutex);
    if (PTHREAD_ONCE_INIT == *once_control) {
      init_routine();
      *once_control = 42;
    }
    pthread_mutex_unlock(&once_mutex);
  }
  return 0;
}

int pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset)
{
  pthread_t self = pthread_self();
  if (oldset)
    *oldset = self->blocked_signal_set;
  if (set) {
    switch (how) {
      case SIG_BLOCK:
        self->blocked_signal_set |= *set;
        break;
      case SIG_UNBLOCK:
        self->blocked_signal_set &= ~(*set);
        break;
      case SIG_SETMASK:
        self->blocked_signal_set = *set;
        break;
    }
  }
  return 0;
}

void pthread_np_pending_signal_handler(int i, void* ucontext_arg)
{
}

pthread_mutex_t mutex_init_lock;

int pthread_mutex_init(pthread_mutex_t * mutex, const pthread_mutexattr_t * attr)
{
  *mutex = (struct _pthread_mutex_info*)malloc(sizeof(struct _pthread_mutex_info));
  InitializeCriticalSection(&(*mutex)->cs);
  (*mutex)->file = " (free) ";
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
    DeleteCriticalSection(&(*mutex)->cs);
  return 0;
}

void pthread_np_add_pending_signal(pthread_t thread, int signum)
{
  const char * s = thread->signal_is_pending[signum] ? "pending" : "not pending";

  thread->signal_is_pending[signum] = 1;
}

int pthread_kill(pthread_t thread, int signum)
{
  pthread_np_add_pending_signal(thread,signum);
  return 0;
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
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  EnterCriticalSection(&(*mutex)->cs);
  (*mutex)->owner = pthread_self();
  return 0;
}

int pthread_mutex_trylock(pthread_mutex_t *mutex)
{
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if (TryEnterCriticalSection(&(*mutex)->cs)) {
    (*mutex)->owner = pthread_self();
    return 0;
  }
  else
    return EBUSY;
}

int pthread_mutex_lock_annotate_np(pthread_mutex_t *mutex, const char* file, int line)
{
  int contention = 0;
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
      pthshow("Mutex #x%p: automatic initialization; #x%p %s +%d",
              mutex, *mutex,
              file, line);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if ((*mutex)->owner) {
    pthshow("Mutex #x%p -> #x%p: contention; owned by #x%p, wanted by #x%p",
            mutex, *mutex,
            (*mutex)->owner,
            pthread_self());
    pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
            mutex, *mutex,
            (*mutex)->file,(*mutex)->line, file, line);
    contention = 1;
  }
  EnterCriticalSection(&(*mutex)->cs);
  if (contention) {
    pthshow("Mutex #x%p -> #x%p: contention end; left by #x%p, taken by #x%p",
            mutex, *mutex,
            (*mutex)->owner,
            pthread_self());
    pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
            mutex, *mutex,
            (*mutex)->file,(*mutex)->line, file, line);
  }
  (*mutex)->owner = pthread_self();
  (*mutex)->file = file;
  (*mutex)->line = line;
  return 0;
}

int pthread_mutex_trylock_annotate_np(pthread_mutex_t *mutex, const char* file, int line)
{
  int contention = 0;
  if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
    pthread_mutex_lock(&mutex_init_lock);
    if (*mutex == PTHREAD_MUTEX_INITIALIZER) {
      pthread_mutex_init(mutex, NULL);
    }
    pthread_mutex_unlock(&mutex_init_lock);
  }
  if ((*mutex)->owner) {
    pthshow("Mutex #x%p -> #x%p: tried contention; owned by #x%p, wanted by #x%p",
            mutex, *mutex,
            (*mutex)->owner,
            pthread_self());
    pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
            mutex, *mutex,
            (*mutex)->file,(*mutex)->line, file, line);
    contention = 1;
  }
  if (TryEnterCriticalSection(&(*mutex)->cs)) {
    if (contention) {
      pthshow("Mutex #x%p -> #x%p: contention end; left by #x%p, taken by #x%p",
              mutex, *mutex,
              (*mutex)->owner,
              pthread_self());
      pthshow("Mutex #x%p -> #x%p: contention notes: old %s +%d, new %s +%d",
              mutex, *mutex,
              (*mutex)->file,(*mutex)->line, file, line);
    }
    (*mutex)->owner = pthread_self();
    (*mutex)->file = file;
    (*mutex)->line = line;
    return 0;
  }
  else
    return EBUSY;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
  (*mutex)->owner = NULL;
  LeaveCriticalSection(&(*mutex)->cs);
  return 0;
}
static pthread_key_t cv_event_key;

static void cv_event_destroy(void* event)
{
  CloseHandle((HANDLE)event);
}

static HANDLE cv_default_event_get_fn()
{
  HANDLE event = pthread_getspecific(cv_event_key);
  if (!event) {
    event = CreateEvent(NULL, FALSE, FALSE, NULL);
    pthread_setspecific(cv_event_key, event);
  } else {
    ResetEvent(event);
  }
  return event;
}

static void cv_default_event_return_fn(HANDLE event)
{
}

static pthread_condattr_t cv_default_attr = {
  0,                            /* alertable */
  cv_default_event_get_fn,      /* get_fn */
  cv_default_event_return_fn    /* return_fn */
};

int pthread_cond_init(pthread_cond_t * cv, const pthread_condattr_t * attr)
{
  if (!attr)
    attr = &cv_default_attr;
  pthread_mutex_init(&cv->wakeup_lock, NULL);
  cv->first_wakeup = NULL;
  cv->last_wakeup = NULL;
  cv->alertable = attr->alertable;
  cv->get_fn = attr->get_fn;
  cv->return_fn = attr->return_fn;
  return 0;
}

int pthread_condattr_init(pthread_condattr_t *attr)
{
  *attr = cv_default_attr;
  return 0;
}

int pthread_condattr_destroy(pthread_condattr_t *attr)
{
  return 0;
}
int pthread_condattr_setevent_np(pthread_condattr_t *attr,
                                 cv_event_get_fn get_fn, cv_event_return_fn ret_fn)
{
  attr->get_fn = get_fn ? get_fn : cv_default_event_get_fn;
  attr->return_fn = ret_fn ? ret_fn : cv_default_event_return_fn;
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
  w->next = NULL;
  pthread_mutex_lock(&cv->wakeup_lock);
  w->event = cv->get_fn();
  if (cv->last_wakeup == w) {
    fprintf(stderr, "cv->last_wakeup == w\n");
    fflush(stderr);
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
    fflush(stderr);
    ExitProcess(0);
  }
  if (cv->last_wakeup->next != NULL) {
    fprintf(stderr, "cv->last_wakeup->next != NULL\n");
    fflush(stderr);
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
  if (rv == WAIT_TIMEOUT) {
    cv_wakeup_remove(cv, &w);
  }
  cv->return_fn(w.event);
  pthread_mutex_lock(cs);
  if (rv == WAIT_TIMEOUT)
    return ETIMEDOUT;
  else
    return 0;
}

int sched_yield()
{
  /* http://stackoverflow.com/questions/1383943/switchtothread-vs-sleep1
     SwitchToThread(); was here. Unsure what's better for us, just trying.. */
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

static int pthread_initialized = 0;

void pthreads_win32_init()
{
  if (!pthread_initialized) {
    thread_self_tls_index = TlsAlloc();
    pthread_mutex_init(&mutex_init_lock, NULL);
    pthread_np_notice_thread();
    pthread_key_create(&cv_event_key,cv_event_destroy);
    pthread_initialized = 1;
  }
}

static
VOID CALLBACK pthreads_win32_unnotice(void* parameter, BOOLEAN timerOrWait)
{
  pthread_t pth = parameter;
  pthread_t self = tls_impersonate(pth);

  tls_call_destructors();
  CloseHandle(pth->handle);
  /*
  if (pth->fiber && pth->own_fiber) {
    DeleteFiber(pth->fiber);
    } */
  UnregisterWait(pth->wait_handle);

  tls_impersonate(self);
  pthread_mutex_destroy(&pth->fiber_lock);
  pthread_mutex_destroy(&pth->lock);
  free(pth);
}

int pthread_np_notice_thread()
{
  if (!pthread_self()) {
    pthread_t pth = (pthread_t)calloc(sizeof(pthread_thread),1);
    pth->teb = NtCurrentTeb();
    pthread_mutex_init(&pth->fiber_lock,NULL);
    pthread_mutex_init(&pth->lock,NULL);
    pth->state = pthread_state_running;
    pth->fiber_group = pth;

    sigemptyset(&pth->blocked_signal_set);
    {
      int i;
      for (i = 1; i < NSIG; ++i)
        pth->signal_is_pending[i] = 0;
    }
    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
                    GetCurrentProcess(), &pth->handle, 0, TRUE,
                    DUPLICATE_SAME_ACCESS);
    tls_impersonate(pth);

    if (pthread_initialized) {
      RegisterWaitForSingleObject(&pth->wait_handle,
                                  pth->handle,
                                  pthreads_win32_unnotice,
                                  pth,
                                  INFINITE,
                                  WT_EXECUTEONLYONCE);
    }
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

int pthread_np_switch_to_fiber(pthread_t pth)
{
  pthread_t self = pthread_self();

 again:
  if (pth == self) {
    /* Switch to itself is a successful no-op.
       NB. SwitchToFiber(GetCurrentFiber()) is not(!). */
    return 0;
  }

  if (!pth->fiber) {
    /* Switch to not-a-fiber-at-all */
    return -1;
  }

  if (!pth->created_as_fiber) {
    /* Switch to main thread (group): fails if... */
    if (self && (self->fiber_group != pth)) {
      /* ...trying to switch from [under] one main thread into another */
      return -1;
    }
  }
  if (!self && pth->created_as_fiber) {
    /* Switch to free fiber from non-noticed thread */
    return -1;
  }

  if (self && pthread_np_convert_self_to_fiber()) {
    /* Current thread can't become a fiber (and run fibers) */
    return -1;
  }

  /* If target fiber is suspened, we wait here. */
  pthread_mutex_lock(&pth->fiber_lock);
  if (pth->fiber_group) {
    /* Reentering a running fiber */
    pthread_mutex_unlock(&pth->fiber_lock);
    /* Don't wait for a running fiber here, just fail. If an
       application wants to wait, it should use some separate
       synchronization. */
    return -1;
  }
  if (self) {
    /* From now on, any attempt to get context of self receives ctx
       content, and suspend/resume work with fiber reentrance lock. */
    pthread_np_publish_context(NULL);
    /* Target fiber group is like mine */
    pth->fiber_group = self->fiber_group;
  } else {
    /* Switch-from-null-self (always into thread, usually from
       terminating fiber) */
    pth->fiber_group = pth;
  }
  /* Target fiber now marked as busy */
  pthread_mutex_unlock(&pth->fiber_lock);

  if (self) {
    pthread_save_context_hook();
  }
  /* NB we don't set pthread TLS, let target fiber do it by itself. */
  SwitchToFiber(pth->fiber);

  /* When we return here... */
  pth = tls_impersonate(self);

  /* Now pth contains fiber that entered this one */
  pthread_restore_context_hook();

  if (pth) {
    pthread_mutex_lock(&pth->fiber_lock);
    if (pth->fiber_group == self->fiber_group) {
      pth->fiber_group = NULL;
    }
    pthread_mutex_unlock(&pth->fiber_lock);
  }
  /* Self surely is not NULL, or we'd never be here */

  pthread_mutex_lock(&self->fiber_lock);
  /* Thus we avoid racing with GC */
  pthread_np_unpublish_context();
  pthread_mutex_unlock(&self->fiber_lock);

  /* From now on, pthread_np_get_thread_context on self calls real
     GetThreadContext(), and suspend/resume work with real thread
     handle. */

  /* Implement call-in-fiber */
  if (self->fiber_callback) {
    void (*cb)(void*) = self->fiber_callback;
    void *ctx = self->fiber_callback_context;

    /* Nested callbacks and fiber switches are possible, so clean
       up a cb pointer here */
    self->fiber_callback = NULL;
    self->fiber_callback_context = NULL;
    cb(ctx);
    if (pth) {
      /* Return to caller without recursive
       pthread_np_switch_to_fiber.  This way, an "utility fiber"
       serving multiple callbacks won't grow its stack to infinity */
      goto again;
    }
    /* There is no `callback client' pretending to be returned
       into: it means callback shouldn't yield to caller. */
  }
  return 0; /* success */
}

int pthread_np_run_in_fiber(pthread_t pth, void (*callback)(void*),
                            void* context)
{
  pth->fiber_callback = callback;
  pth->fiber_callback_context = context;
  return pthread_np_switch_to_fiber(pth);
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
int sigpending(sigset_t *set)
{
  int i;
  sigemptyset(set);
  for (i=0; i<NSIG;++i) {
    if (pthread_self()->signal_is_pending[i]) {
      sigaddset(set, i);
    }
  }
  return 0;
}

#endif
