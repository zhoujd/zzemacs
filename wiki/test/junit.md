junit
=====

## URLs

```
## https://labs.etsi.org/rep/help/ci/testing/unit_test_reports.md
## https://linuxsimply.com/bash-scripting-tutorial/process-and-signal-handling/process-management/time-elapsed/
```

## Time elapsed is represented

```
Units: The time attribute in JUnit XML reports is specified in seconds, not milliseconds.
While some tools might misinterpret or display it in milliseconds,
the standard specification defines it in seconds.
```

## Calculate Elapsed Time in Milliseconds

```
#!/bin/bash

#Capturing start time in milliseconds
start_time=$(date +%s%3N)

#Performing a task
sleep 3

#Capturing end time in milliseconds
end_time=$(date +%s%3N)

#Calculating elapsed time in milliseconds
milli_time=$((end_time - start_time))

#Displaying elapsed time in milliseconds
echo "Elapsed time (milliseconds): $milli_time ms"
```

## Calculate Elapsed Time in Nanoseconds

```
#!/bin/bash

#Capturing start time in nanoseconds
start_time=$(date +%s%N)

#Performing a task
sleep 2

#Capturing end time in nanoseconds
end_time=$(date +%s%N)

#Calculating elapsed time in nanoseconds
nano_time=$((end_time - start_time))

#Displaying elapsed time in nanoseconds
echo "Elapsed time (nanoseconds): $nano_time ns"
```

## Measure Elapsed Time in Seconds in Bash

```
#!/bin/bash

#Capturing the start time
start_time=$(date +%s)

#Performing task
sleep 3

#Capturing the end time
end_time=$(date +%s)

#Calculating elapsed time
elapsed=$((end_time - start_time))

#Displaying elapsed time
echo "Elapsed time is: $elapsed seconds"
```

## JUnit XML format specification

```
testcase|time|Test execution time in seconds
```

## Floating-Point Division (Including Milliseconds as Decimals)

```
milliseconds=2500
seconds=$(echo "scale=3; $milliseconds / 1000" | bc)
echo "Seconds: $seconds" # Output: Seconds: 2.500
```

## Pytest junit

```
$ pip install --upgrade pip setuptools wheel
$ pip install pytest
$ pytest -sv . --junit-xml=<path>
```
