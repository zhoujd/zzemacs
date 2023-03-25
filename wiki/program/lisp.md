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

## Bubble sort with lisp

```lisp
;; http://vec44.com/posts/bubble-sort-in-common-lisp/
(defun bubble-sort (x)
  (let ((l (length x)))
    (dotimes (n l)
      (let ((swapped nil))
       (dotimes (m (- l 1))
         (let ((num1 (nth m x))          ; get number at current index
               (num2 (nth (+ m 1) x)))   ; get number at current index + 1
           (if (> num1 num2)             ; if current number is larger than next, then swap the values.
               (let ((temp num1))
                 (setf (nth m x) num2)
                 (setf (nth (+ m 1) x) temp)
                 (setf swapped T)))
           ))
       (when (not swapped)               ; exit if, list is already sorted.
         (return x))))))

;; https://literateprograms.org/bubble_sort__lisp_.html
(defun bubble-sort (array cmp-fun) 
  "Bubble sort implementation in common lisp. Using the extended loop facility."
  (let ((result (copy-seq array)))
    (loop for i from (1- (length result)) downto 0 do
        (loop for j from 0 to i
            when (funcall cmp-fun (aref result i) (aref result j))
               do (rotatef (aref result i) (aref result j)) ))
    result))
```
