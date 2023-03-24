revert
======

## Revert string with STL

```C++
#include <iostream>
using namespace std;

string revert_str(string &str)
{
    string newstr;
    string::reverse_iterator it;
    for (it=str.rbegin(); it!=str.rend(); ++it)
    {
        newstr.append(1, *it);
    }
    return newstr;
}
```

## Revert string with C++

```C++
string revertString(const string& str)
{
    if (str.length() <= 1)
    {
        return str;
    }

    // str = 'abcdef'
    // str[0] = 'a'
    // str.substr(1) = 'bcdef'
    return revertString(str.substr(1) + str[0];
}

string revert(string a)
{
    int temp;
    int len = a.length();
    cout << len << endl;
    for (int i = 0; i < len/2; i++)
    {
        temp = a[i];
        a[i] = a[len - 1 - i];
        a[len - 1 - i] = temp;
    }

    return a;
}
```

## Revert string with C

```C
#include <stdio.h>
#include <string.h>
char* revert(char *str)
{
    int n = strlen(str);
    for (int i = 0; i < n/2; i++)
    {
        char c = str;
        str = str[n - i];
        str[n - i] = c;
    }

    return str;
}

void revert(char *str)
{
    int front, rear;
    rear = strlen(str) - 1;
    for (front = 0; front < rear; front++, rear--)
    {
        str[front] = str[front]^str[rear];
        str[rear] = str[front]^str[rear];
        str[front] = str[front]^str[rear];
    }
}

void revert(char *s, int size)
{
    char * p = s;
    char * q = s+size-1;
    while((q-p)>0){
        char tmp = *p;
        *p=*q;
        *q = tmp; 
        p++;
        q--;              
    }       
}       
```
