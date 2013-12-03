package pubCrawl.core;

import pubCrawl.core.PCObject;

public abstract class IPCFunction extends PCObject
{
    public abstract PCObject call(PCObject... args);
}