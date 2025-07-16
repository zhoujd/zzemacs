#!/usr/bin/env python3

# https://github.com/Parveshdhull/AutoTyper
# apt install python3-tk python3-dev

import pyautogui
import time
import sys

delay = 2 # Inital Delay in Seconds
time.sleep(delay)

interval = 0.07   # In Seconds
cmd = ' '.join(sys.argv[1:]) + "\n"
pyautogui.write(cmd, interval=interval)
