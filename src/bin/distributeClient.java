
import java.lang.Exception;
import java.lang.SecurityManager;
import java.lang.String;
import java.lang.System;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;
import java.util.Iterator;

public class distributeClient {

    private ArrayList<Compute> slaves;

    public distributeClient(){
        slaves = new ArrayList<Compute>();
        System.setProperty("java.security.policy", "file:///home/sireesh/jsp.policy");
        if (System.getSecurityManager() == null) {
            System.setSecurityManager(new SecurityManager());
        }
    }

    public  void getRegistries(String... hosts){
        Registry registry;
        Compute comp;
        try{
            for(int i=0; i< hosts.length; i++){
               registry = LocateRegistry.getRegistry(hosts[i]);
               comp = (Compute) registry.lookup("Compute");
               slaves.add(comp);
            }
        }catch(Exception e){
            e.printStackTrace();
            System.out.println("Failed to connect");
        }
      }

    public PCList distributeFunction(IPCFunction function, PCList toProcess){
        PCList output  = new PCList();

        Iterator<PCObject> task_it = toProcess.iterator();
        Iterator<Compute> slave_it = slaves.iterator();


        try{
            while(task_it.hasNext()){
                if(!(slave_it.hasNext())){
                    slave_it = slaves.iterator();
                }

                output.add(slave_it.next().callFunction(function, task_it.next()));

            }

        }catch(RemoteException e){
            e.printStackTrace();

        }

        return output;

    }
}
