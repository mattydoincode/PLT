#!/bin/sh

make cleanJava
make
./pc < $1
cd java
javac *.java
