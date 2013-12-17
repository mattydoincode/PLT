#!/bin/sh

make cleanJava >/dev/null
make >/dev/null
./pc < $1 >/dev/null
shift
cd java
javac *.java >/dev/null
cp * ~/public_html/
java -Djava.rmi.server.codebase=warsaw.clic.cs.columbia.edu output $@
