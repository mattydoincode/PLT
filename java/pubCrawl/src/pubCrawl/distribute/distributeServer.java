package pubCrawl.distribute;

import pubCrawl.core.IPCFunction;
import pubCrawl.core.PCObject;
import pubCrawl.distribute.Compute;


import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;


public class distributeServer implements Compute{

    public distributeServer(){
        super();
    }

    public PCObject callFunction(IPCFunction function, PCObject param){
        return function.call(param);
    }

    public static void main(String[] args){


        System.setProperty("java.security.policy", "file:///home/sireesh/jsp.policy");
        System.setProperty("java.rmi.server.codebase", "file:///home/sireesh/PLT/pubCrawl/");
        System.setProperty("java.rmi.server.hostname", "localhost");

        if (System.getSecurityManager() == null) {
            System.setSecurityManager(new SecurityManager());
        }




        try {
            String name = "Compute";

            Compute engine = new distributeServer();
            Compute stub =
                    (Compute) UnicastRemoteObject.exportObject(engine, 0);

            Registry registry = LocateRegistry.createRegistry(1099);

            registry.rebind(name, stub);

            System.out.println("ComputeEngine bound");
        } catch (Exception e) {
            System.err.println("ComputeEngine exception:");
            e.printStackTrace();
        }
    }
}


