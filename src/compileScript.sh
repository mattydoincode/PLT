#!/bin/sh

make cleanJava > /dev/null
make > /dev/null
./pc -j < $1 >/dev/null
cd bin >/dev/null
javac *.java >/dev/null
java output