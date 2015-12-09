#include <stdio.h>
#include <string.h>
#include <pcre.h>
#include "regex.h"

void *regex_compile(char *regex) {

    const char *error;
    int erroroffset;

    void *rgx = pcre_compile(regex, 0, &error, &erroroffset, NULL);

    return rgx;

}

int regex_match(char *str, void *rgx, char ***output, int maxmatch) {

    int rc = 0;
    int i;
    int mx = (maxmatch+1)*3;
    int *vc = malloc(sizeof(int)*mx);

    rc = pcre_exec(rgx, NULL, str, (int)strlen(str), 0, 0, vc, mx);

    if (rc == 0) {
        rc = maxmatch+1;
    }

    if (output) {

        char **out = malloc(maxmatch*sizeof(char*));

        for (i = 1; i < rc; i++) {
           char *s_start = str + vc[2*i];
           int s_length = vc[2*i+1] - vc[2*i];
           out[(i-1)] = malloc(sizeof(char)*s_length+1);
           snprintf(out[(i-1)], s_length+1, "%.*s", s_length, s_start);
        }

        *output = out;
    }

    return rc-1;
}

int regex_smatch(char *str, char *regex, char ***output, int maxmatch) {

    void* rgx = regex_compile(regex);

    if (!rgx) {
        return -1;
    }

    return regex_match(str, rgx, output, maxmatch);

}

#ifdef TESTS

int main(int argc, char **argv) {
    char **out;
    void *rgx = regex_compile(argv[1]);
    int i = 0;
    int x = regex_smatch(argv[2], argv[1], &out, 2);
    if (x < 0) x = 0;
    for (i = 0; i < x; i++) {
        printf("%d - ", i);
        printf("%s\n", out[i]);
        free(out[i]);
    }
    free(out);
    for (i = 0; i < 100000; i++) {
        x = regex_match(argv[2], rgx, NULL, 2);
    }
    printf("%d\n", x);
    return 0;
}

#endif
