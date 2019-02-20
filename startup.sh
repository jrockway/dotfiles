#!/bin/bash

xset -dpms
minikube start
sudo sysctl -w net.ipv4.conf.all.route_localnet=1
sudo iptables -t nat -A POSTROUTING -m addrtype --src-type LOCAL --dst-type UNICAST -j MASQUERADE
sudo iptables -t nat -A OUTPUT -o lo -d 127.0.0.1 -p tcp --dport 5000 -j DNAT --to-destination $(minikube ip):5000
exec miniproxy
