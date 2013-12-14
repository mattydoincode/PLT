#!/bin/sh

make cleanJava > /dev/null
make > /dev/null
./pc -j < $1 >/dev/null
cd bin >/dev/null
javac *.java >/dev/null
java -Djava.security.policy=bin/client.policy -Djava.rmi.server.codebase=file:///home/sireesh/PLT/pubCrawl/src/bin/ output localhost
