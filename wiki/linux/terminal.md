Terminal
========

## Check both the rows and columns

```
## 1. Using Shell Variables
echo $LINES    # Outputs the number of rows (height)
echo $COLUMNS  # Outputs the number of columns (width)

## 2. Using the tput Command
tput lines     # Outputs rows
tput cols      # Outputs columns

## 3. Using stty
stty size      # Outputs: <rows> <columns> (e.g., 24 80)
```
