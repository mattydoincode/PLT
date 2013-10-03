package pubCrawl;

import pubCrawl.core.IPCFunction;
import pubCrawl.core.PCObject;

import java.io.*;
import java.util.*;

public class Main {

    public static void main(String[] args) throws IOException {

        ArrayList<Integer> list = new ArrayList<Integer>();

        IPCFunction myFunc = new IPCFunction() {
            @Override
            public PCObject call(PCObject... args) {
                double result = args[0].<Double>getBase() + args[1].<Double>getBase();
                return new PCObject(result);
            }
        };

        PCObject result = myFunc.call(new PCObject(1), new PCObject(2));

        System.out.println(result.<Double>getBase());
    }

}
