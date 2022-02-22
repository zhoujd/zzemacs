#!/bin/bash

echo "Find zambie process"
echo "stat    pid     ppid"
ps -A -ostat,pid,ppid | grep -e '[zZ]'

echo "Please run 'kill -9 <ppid>'"
