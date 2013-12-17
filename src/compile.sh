#!/bin/sh

make cleanJava >/dev/null
make >/dev/null
./pc < $1 >/dev/null
shift
cd java
javac *.java >/dev/null
MY_IP=$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')
java -Djava.rmi.server.codebase=http://$MY_IP:8782/ output $@
