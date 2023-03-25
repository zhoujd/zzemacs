Sort
====

## Sort with C++ STL

```C++
#include<iostream>
#include<algorithm>
using namespace std;
bool cmp(int a, int b);
void main() {
　　int a[] = {45,12,34,77,90,11,2,4,5,55};
　　sort(a, a+10, cmp);
　　for(int i=0; i<10; i++)
　　　　cout<<a[i]<<" ";     
}

bool cmp(int a,int b) {
　　return a>b;
}
```

## Bub sort with C

```C
#include <stdio.h>
//交换 a 和 b 的位置的函数
#define N 5
int a[N] = { 5,1,4,2,8 };
void swap(int *a, int *b);
//这是带输出的冒泡排序实现函数，从输出结果可以分析冒泡的具体实现流程
void BubSort_test();
//这是不带输出的冒泡排序实现函数，通过此函数，可直接对数组 a 中元素进行排序
void BubSort_pro();
int main()
{
    BubSort_test();
    return 0;
}
void swap(int *a, int *b) {
    int temp;
    temp = *a;
    *a = *b;
    *b = temp;
}
//这是带输出的冒泡排序实现函数，从输出结果，可以看到冒泡的具体实现流程
void BubSort_test() {
    for (int i = 0; i < N; i++) {
        //对待排序序列进行冒泡排序
        for (int j = 0; j + 1 < N - i; j++) {
            //相邻元素进行比较，当顺序不正确时，交换位置
            if (a[j] > a[j + 1]) {
                swap(&a[j], &a[j + 1]);
            }
        }
        //输出本轮冒泡排序之后的序列
        printf("第%d轮冒泡排序：", i + 1);
        for (int i = 0; i < N; i++) {
            printf("%d ", a[i]);
        }
        printf("\n");
    }
}
//这是不带输出的冒泡排序实现函数，通过此函数，可直接对数组 a 中元素进行排序
void BubSort_pro() {
    for (int i = 0; i < N; i++) {
        //对待排序序列进行冒泡排序
        for (int j = 0; j + 1 < N - i; j++) {
            //相邻元素进行比较，当顺序不正确时，交换位置
            if (a[j] > a[j + 1]) {
                swap(&a[j], &a[j + 1]);
            }
        }
    }
}
```

## Bubble sort with Python

```Python
def bubbleSort(arr):
    n = len(arr) 
    # 遍历所有数组元素
    for i in range(n):
        # Last i elements are already in place
        for j in range(0, n-i-1): 
            if arr[j] > arr[j+1] :
                arr[j], arr[j+1] = arr[j+1], arr[j]
 
arr = [64, 34, 25, 12, 22, 11, 90] 
bubbleSort(arr) 
print ("排序后的数组:")
for i in range(len(arr)):
    print ("%d" %arr[i]),
```
