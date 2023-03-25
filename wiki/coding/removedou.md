Remove Douplicate 
=================

## Remove douplicate with C

```C
#include <string.h>
#include <stdio.h>

int main(int argc, char* argv[])
{
    char a[] = "hello000";
    int n, i, j, k;

    printf("Before: %s\n", a);
    for (i=0; a[i]; i++)
    {
        for (j =i+1; a[j];)
        {
            if (a[i] == a[j])
            {
                for (k=j; a[k]; k++)
                    a[k] = a[k+1];
            }
            else
            {
                j++;
            }
        }
    }
    printf("After: %s\n", a);
    
    return 0;
}
```
