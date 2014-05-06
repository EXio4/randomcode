#pragma once

void *regex_compile(char *regex);
int regex_match(char *str, void *rgx, char ***output, int maxmatch);
int regex_smatch(char *str, char *regex, char ***output, int maxmatch);
