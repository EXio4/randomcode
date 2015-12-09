#pragma once

#include <pthread.h>

typedef struct thread {

    int running, stop;
    void *(*func)(void*);
    void *ret;

    int status;

    void *data;

// TODO: support windows threads

    pthread_t threadid;

} thread;

thread *thread_new(void *(*func)(void*), void *param);
int     thread_start(thread *self);
int     thread_stop(thread *self);
int     thread_running(thread *self);
void    thread_wait(thread *self);
void   *thread_ret(thread *self);


