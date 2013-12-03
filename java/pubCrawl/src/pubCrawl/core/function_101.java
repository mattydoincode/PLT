package pubCrawl.core;

import pubCrawl.core.IPCFunction;
import pubCrawl.core.PCObject;

public class function_101 extends PCObject implements IPCFunction{
    public function_101(){}

    public PCObject call(PCObject... args){
        return new PCObject()
                .set("e",args[0]);
    }
}