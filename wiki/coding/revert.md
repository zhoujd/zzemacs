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

## Revert list

```C++
ListNode* reverse(ListNode* head) {
    if (head == nullptr || head->next == nullptr) {
        return head;
    }
    ListNode* last = reverse(head->next);
    head->next->next = head;
    head->next = nullptr;
    return last;
}
```

```python
## return new head
def reverse(head: ListNode) -> ListNode:
    if not head or not head.next:
        return head
    last = reverse(head.next) 
    head.next.next = head 
    head.next = None 
    return last
```

```golang
func reverse(head *ListNode) *ListNode {
    if head == nil || head.next == nil {
        return head
    }
    last := reverse(head.next)
    head.next.next = head
    head.next = nil
    return last
}
```

## Revert list N

```C++
ListNode* successor = nullptr;
// return new head
ListNode* reverseN(ListNode* head, int n) {
    if (n == 1) {
        successor = head->next;
        return head;
    }
    ListNode* last = reverseN(head->next, n - 1);
    head->next->next = head;
    head->next = successor;
    return last;
}
```

```Python
successor = None
# return new head
def reverseN(head: ListNode, n: int) -> ListNode:
    global successor
    if n == 1:
        # record n + 1 node
        successor = head.next
        return head
    last = reverseN(head.next, n - 1)
    head.next.next = head
    head.next = successor
    return last 
```

## Revert list M N

```C++
ListNode* reverseBetween(ListNode* head, int m, int n) {
    // base case
    if (m == 1) {
        return reverseN(head, n);
    }
    // base case
    head->next = reverseBetween(head->next, m - 1, n - 1);
    return head;
}
```

```Python
def reverseBetween(head: ListNode, m: int, n: int) -> ListNode:
    # base case
    if m == 1:
        return reverseN(head, n)
    # base case
    head.next = reverseBetween(head.next, m - 1, n - 1)
    return head
```



