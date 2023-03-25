Sort
====

## URLs

https://www.geeksforgeeks.org/bubble-sort/

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

## Bubble sort with C

```C

// C program for implementation of Bubble sort
#include <stdio.h>
 
void swap(int* xp, int* yp)
{
    int temp = *xp;
    *xp = *yp;
    *yp = temp;
}
 
// A function to implement bubble sort
void bubbleSort(int arr[], int n)
{
    int i, j;
    for (i = 0; i < n - 1; i++)
        // Last i elements are already in place
        for (j = 0; j < n - i - 1; j++)
            if (arr[j] > arr[j + 1])
                swap(&arr[j], &arr[j + 1]);
}
 
/* Function to print an array */
void printArray(int arr[], int size)
{
    int i;
    for (i = 0; i < size; i++)
        printf("%d ", arr[i]);
    printf("\n");
}
 
// Driver program to test above functions
int main()
{
    int arr[] = { 5, 1, 4, 2, 8 };
    int n = sizeof(arr) / sizeof(arr[0]);
    bubbleSort(arr, n);
    printf("Sorted array: \n");
    printArray(arr, n);
    return 0;
}
```

## Bubble sort with Python

```Python
# Python program for implementation of Bubble Sort
 
def bubbleSort(arr):
    n = len(arr)
 
    # Traverse through all array elements
    for i in range(n):
 
        # Last i elements are already in place
        for j in range(0, n-i-1):
 
            # traverse the array from 0 to n-i-1
            # Swap if the element found is greater
            # than the next element
            if arr[j] > arr[j+1]:
                arr[j], arr[j+1] = arr[j+1], arr[j]
 
 
# Driver code to test above
if __name__ == "__main__":
  arr = [5, 1, 4, 2, 8]
 
  bubbleSort(arr)
 
  print("Sorted array is:")
  for i in range(len(arr)):
      print("%d" % arr[i], end=" ")
```
