Remove Douplicate 
=================

## Remove dou with C++

```C++
string remove_dou(string str) 
{
    size_t i, j;
    string temp_str;
    temp_str.append(str, 0, 1);
    
    for (i = 1; i < str.length(); i++)
    {
        for (j = 0; j < str.length(); j++)
        {
            if (str[i] == temp_str[j])
                break;    
        }
        
        if (j == temp_str.length())
            temp_str.append(str, i, 1);
    }
    
    return temp_str;    
}
```
