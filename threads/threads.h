#pragma once

#include <pthread.h>

typedef struct thread {

    int running, stop;
    void *(*func)(void*);
    void *ret;

    int status;

    void* data;

// TODO: support windows threads

    pthread_t threadid;

} thread;

thread* thread_new(void *(*func)(void*), void* param);
int thread_start(thread* self);
int thread_stop(thread* self);

int thread_running(thread* self) {
    if (!self) return -1;
    return self->running;
}

int thread_kill(thread* self); // use only when you don't care about clean up or want to make everything DIE
