#!/bin/sh

make all
./pc -j < $1
cd bin
javac *.java
echo "\n\nprogram output:"
java output