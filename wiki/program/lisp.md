lisp
====

## Sort with CL sort

```lisp
CL-USER> (sort "elbow" #'char<)
"below"
CL-USER> (sort '(a c b s) #'string<)
(A B C S)
CL-USER> (sort '(39 48 28 4 32) #'<)
(4 28 32 39 48)
CL-USER> (sort (vector "foo" "bar" "barz") #'string<)
#("bar" "barz" "foo")
```
