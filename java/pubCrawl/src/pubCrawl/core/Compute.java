package pubCrawl.core;

import pubCrawl.core.PCObject;
import pubCrawl.core.IPCFunction;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Compute extends Remote
{
    PCObject callFunction(IPCFunction function, PCObject param) throws RemoteException;
}
