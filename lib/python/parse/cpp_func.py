#!/usr/bin/env python3

import re

# Sample C++ code with a multiline function
cpp_code = """
int calculateSum(
    int a, 
    int b) 
{
    int result = a + b;
    return result;
}
"""

# Regex breakdown:
# (\w+)        : Captures the return type
# \s+(\w+)     : Captures the function name
# \(.*?\s*\)   : Matches the parameter list (multiline safe with re.DOTALL)
# \{.*?\}      : Matches the function body (non-greedy)
pattern = r"(\w+)\s+(\w+)\s*\((.*?)\)\s*\{(.*?)\}"

# Using re.search with re.DOTALL to span multiple lines
match = re.search(pattern, cpp_code, re.DOTALL)

if match:
    print(f"Full Match:\n{match.group(0)}")
    print(f"Return Type: {match.group(1)}")
    print(f"Function Name: {match.group(2)}")
    print(f"Parameter List: {match.group(3)}")
    print(f"Function Body: {match.group(4)}")
