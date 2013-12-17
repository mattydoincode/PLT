#!/bin/sh

make cleanJava
make
./pc < $1 >/dev/null
shift
cd java
javac *.java >/dev/null
java -Djava.rmi.server.codebase=file://$PWD/ output $@
