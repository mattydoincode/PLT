#!/bin/sh

./pc < $1 >/dev/null
cd java
javac *.java >/dev/null
java -Djava.rmi.server.codebase=file://$PWD/ output localhost 1099 localhost 8909
