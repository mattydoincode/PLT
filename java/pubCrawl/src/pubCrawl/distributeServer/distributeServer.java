package pubCrawl.distributeServer;

import pubCrawl.core.IPCFunction;
import pubCrawl.core.PCObject;
import pubCrawl.core.Compute;

import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;


public class distributeServer implements Compute {

    public distributeServer(){
        super();
    }

    public PCObject callFunction(IPCFunction function, PCObject param){
        return function.call(param);
    }

    public static void main(String[] args){


        //System.setProperty("java.security.policy", "file:/Library/Java/Home/lib/security/server.policy");
       //System.setProperty("java.security.policy", "file:///Users/kevinmangan/School/PLT/java/pubCrawl/src/server.policy");

        //if (System.getSecurityManager() == null) {
        //    System.setSecurityManager(new SecurityManager());
        //}




        try {
            String name = "Compute";

            Compute engine = new distributeServer();
            Compute stub =
                    (Compute) UnicastRemoteObject.exportObject(engine, 0);

            Registry registry = LocateRegistry.createRegistry(Integer.parseInt(args[0]));

            registry.rebind(name, stub);

            System.out.println("ComputeEngine bound");
        } catch (Exception e) {
            System.err.println("ComputeEngine exception:");
            e.printStackTrace();
        }
    }
}


