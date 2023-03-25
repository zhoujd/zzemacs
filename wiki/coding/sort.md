Sort
====

## Sort with C++ STL

```C++
 1 #include<iostream>
 2 #include<algorithm>
 3 using namespace std;
 4 bool cmp(int a, int b);
 5 void main() {
 7 　　int a[] = {45,12,34,77,90,11,2,4,5,55};
 8 　　sort(a, a+10, cmp);
 9 　　for(int i=0; i<10; i++)
10 　　　　cout<<a[i]<<" ";     
11 }
12
13 bool cmp(int a,int b) {
14 　　return a>b;
15 }
```
