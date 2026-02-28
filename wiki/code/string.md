string
======

## Replace

```
#include <stdio.h>
#include <string.h>
#include <stdlib.h> // Required for dynamic memory allocation (better for robust code)

// A function to replace the first occurrence of a substring.
// This example uses a static buffer; a production version would use malloc/free.
char *replace_substring(char *st, char *orig, char *repl) {
    static char buffer[4096]; // Buffer size is a limitation
    char *ch;

    // If the original substring isn't found, return the original string
    if (!(ch = strstr(st, orig)))
        return st;

    // Copy the part of the string BEFORE the match
    strncpy(buffer, st, ch - st);
    buffer[ch - st] = 0; // Null-terminate the buffer

    // Append the replacement string, then the part AFTER the match
    sprintf(buffer + (ch - st), "%s%s", repl, ch + strlen(orig));
    return buffer;
}
```
