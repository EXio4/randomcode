#include <stdlib.h>
#include "threads.h"

void *thread_internal(void* param) {
    thread* self = (thread*)param;

    self->running = 1;

    self->ret = self->func(param);

    self->running = 0;

    return NULL;
}

thread* thread_new(void *(*func)(void*), void* param) {
    thread *self = malloc(sizeof(thread));
    if (!self) return NULL;

    self->data    = param;
    self->func    = func;
    self->running = 0;
    self->stop    = 0;
    self->status  = 0;
    self->ret     = NULL;

    return self;
}
int thread_start(thread* self) {
    int rc = -1;

    struct timespec req, rem;
    req.tv_sec  = 0;
    req.tv_nsec = 1000000;

    if (!self)         return -1;

    if (self->running) return 1;

    rc = pthread_create(&self->threadid, NULL, &thread_internal, self);

    while (!(self->running)) {
        nanosleep(&req,&rem);
    }

    return rc;
}

int thread_stop(thread* self) {
    if (!self) return -1;

    self->stop = 1;

    return 0;
}

void thread_wait(thread *self) {
    void *status;

    if (self->running) {
        pthread_join(self->threadid, &status);
    }
}
void *thread_ret(thread *self) {
    if (!self || self->running)
        return NULL;
    else
        return self->ret;

}

#ifdef TESTS

#include <stdio.h>
#include <string.h>

void* test(void* param) {
    thread* self = (thread*)param;

    struct timespec req, rem;
    req.tv_sec  = 0;
    req.tv_nsec = 2000000;

    int *i = malloc(sizeof(int));
    *i = 0;

    free(self->data);
    self->data=NULL;

    while(!self->stop) {
        (*i)++;
        nanosleep(&req,&rem);
    }

    (*i)++;

    return (void*)i;
}

int main(int argc, char** argv) {
    thread* my = thread_new(&test, strdup("hi"));

    thread_start(my);

    thread_stop(my);

    thread_wait(my);

    int x = *((int*)thread_ret(my));

    printf("Test ");
    if (x > 0 && my->data == NULL) {
        printf("passed\n");
    } else {
        printf("failed\n");
    }

    // should try to do something more "harder" here..

    return 0;
}

#endif
