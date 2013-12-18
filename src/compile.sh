#!/bin/bash

make cleanJava >/dev/null
make >/dev/null
./pc < $1 >/dev/null
shift
cd java
javac *.java >/dev/null

if [[ "$(ifconfig en0 2>/dev/null)" == *error* ]]; then
	MY_IP=$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')
else
	MY_IP=$(ifconfig | awk -F':' '/inet addr/&&!/127.0.0.1/{split($2,_," ");print _[1]}')
fi

java -Djava.rmi.server.codebase=http://$MY_IP:8782/ output $@
