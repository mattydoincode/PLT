#!/bin/sh

make cleanJava >/dev/null
make >/dev/null
./pc < $1 >/dev/null
shift
cd java
javac *.java >/dev/null
java -Djava.rmi.server.codebase=http://localhost:8000 output $@
