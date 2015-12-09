#pragma once

int base64_encode(const char* message, int len, char** buffer);
int base64_decode(char* b64message, char** buffer);
