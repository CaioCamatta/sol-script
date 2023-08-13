#include "util/file.h"

#include <stdio.h>

// static char *readFile(const char *relativePath) {
//     FILE *f = fopen(relativePath, "rb");
//     fseek(f, 0, SEEK_END);
//     long fsize = ftell(f);
//     fseek(f, 0, SEEK_SET); /* same as rewind(f); */

//     char *string = malloc(fsize + 1);
//     fread(string, fsize, 1, f);
//     fclose(f);

//     string[fsize] = 0;
// }