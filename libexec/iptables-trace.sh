#!/bin/sh

## 1. Run sudo iptables-trace.sh enable 22
## 2. Then run tail -f /var/log/kern.log to see iptables flow log
## 3. Once trace is finished, stop it by running sudo iptables-trace.sh disable 22

if [ $# != "2" ] || ! [ "${2}" -eq "${2}" ] 2> /dev/null
then
    if [ ${1} != "enable" ] && [ ${1} != "disable" ]
    then
        echo "Usage: ${0} [enable|disable] port"
        exit 1
    fi
fi

if [ ${1} = "enable" ]
then
    modprobe nf_log_ipv4
    sysctl -w net.netfilter.nf_log.2=nf_log_ipv4
    iptables -t raw -A PREROUTING -p tcp --dport ${2} -j TRACE
    iptables -t raw -A OUTPUT -p tcp --dport ${2} -j TRACE
    iptables -t raw -A PREROUTING -p udp --dport ${2} -j TRACE
    iptables -t raw -A OUTPUT -p udp --dport ${2} -j TRACE
    echo "iptables trace is enabled for port ${2}"
    echo "run \"tail -f /var/log/kern.log\" to view trace"
else
    iptables -t raw -D OUTPUT -p tcp --dport ${2} -j TRACE
    iptables -t raw -D PREROUTING -p tcp --dport ${2} -j TRACE
    iptables -t raw -D OUTPUT -p udp --dport ${2} -j TRACE
    iptables -t raw -D PREROUTING -p udp --dport ${2} -j TRACE
    sysctl -w net.netfilter.nf_log.2=NONE
    echo "iptables trace is disabled for port ${2}"
fi
