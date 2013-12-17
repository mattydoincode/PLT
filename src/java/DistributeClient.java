
import java.lang.Exception;
import java.lang.SecurityManager;
import java.lang.String;
import java.lang.System;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class DistributeClient {

    private static ArrayList<Compute> slaves;
    private static String[] slaveList;
    private static boolean initialized = false;

    public static void setSlaves(String[] args){
        slaveList = args;
    }

    private static void getRegistries(String... hosts){

        slaves = new ArrayList<Compute>();
        System.setProperty("java.security.policy", "client.policy");
        if (System.getSecurityManager() == null) {
            System.setSecurityManager(new SecurityManager());
        }

        Registry registry;
        Compute comp;
        try{
            for(int i=0; i< hosts.length; i++){
               registry = LocateRegistry.getRegistry(hosts[i]);
               comp = (Compute) registry.lookup("Compute");
               slaves.add(comp);
            }
        }catch(Exception e){
            System.out.println("Failed to connect\n\n");
            e.printStackTrace();
            System.exit(1);
        }
      }

    public static PCList distributeFunction(PCList toProcess, IPCFunction function){
        if(!initialized){
            getRegistries(slaveList);
            initialized = true;
        }

        Compute slave;
        ArrayList<PCObject> output = new ArrayList<PCObject>();
        Iterator<Compute> slave_it = slaves.iterator();

        try{

            ExecutorService exec = Executors.newFixedThreadPool(toProcess.size());
            for(PCObject param: toProcess){
                
                if(!slave_it.hasNext()){
                    slave_it = slaves.iterator();
                }

                slave = slave_it.next();

                Callable<PCObject> distCall = new DistributeCall(slave, function, param);
                Future<PCObject> result = exec.submit(distCall);
                output.add(result.get());
            }
            
            exec.shutdown();

        }catch(InterruptedException e){
            e.printStackTrace();
            System.exit(1);

        }catch(ExecutionException e){
            e.printStackTrace();
            System.exit(1);

        }/**catch(RemoteException e){
            e.printStackTrace();
            return new PCList();
        }**/

        return new PCList(output); 
    }
}
