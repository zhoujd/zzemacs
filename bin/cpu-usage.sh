#!/bin/bash

LOG_DIR="/var/log/cpu_threads"
LOG_FILE="$LOG_DIR/cpu_usage.log"
MAX_SIZE=10485760  # 10MB
ARCHIVE_FILE="$LOG_DIR/cpu_usage_$(date +%Y%m%d%H%M%S).tar.gz"

# Check script running
APP=$(basename $0)
if [ $(pidof -x ${APP} | wc -w) -gt 2 ]; then
    echo "${APP} is already running."
    exit 0
fi

# Run as root
if [ $EUID -ne 0 ]; then
    echo "Please run as root" 2>&1
    exit 1
fi

# Make sure log directory exists
mkdir -p "$LOG_DIR"

while true; do
    echo "Timestamp: $(date)" >> "$LOG_FILE"
    ps -eo user,pid,tid,%cpu,comm --sort=-%cpu | head -n 31 >> "$LOG_FILE"
    echo "--------------------------------------" >> "$LOG_FILE"

    # Limit log file size
    if [[ $(stat -c%s "$LOG_FILE") -gt $MAX_SIZE ]]; then
        tar -czf "$ARCHIVE_FILE" "$LOG_FILE" && rm -f "$LOG_FILE"
    fi

    # Clean logs before 24 hours
    find "$LOG_DIR" -type f -name "*.tar.gz" -mtime +1 -exec rm -f {} \;

    # Loop in 60 seconds
    sleep 60
done
