#pragma once

typedef struct ltun_t {
    int fd;
    void *intf;
    char *name;
} ltun_t;

ltun_t* ltun_alloc(char *dev, int mtu, char *local, char *remote);
int ltun_read(ltun_t *self, char *buf, int len);
int ltun_write(ltun_t *self, char *buf, int len);
int ltun_close(ltun_t *self);
