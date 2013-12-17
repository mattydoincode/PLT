#!/bin/sh

./pc -j < $1 >/dev/null
cd java
javac *.java >/dev/null
java -Djava.rmi.server.codebase=file:///home/alden/Developer/columbia/PLT/src/java/ output localhost
