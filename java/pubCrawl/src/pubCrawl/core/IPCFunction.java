package pubCrawl.core;

import pubCrawl.core.PCObject;

public abstract class IPCFunction extends PCObject
{
    abstract PCObject call(PCObject... args);
}