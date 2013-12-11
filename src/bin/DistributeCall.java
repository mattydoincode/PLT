import java.util.concurrent.Callable;
import java.rmi.RemoteException;

public class DistributeCall implements Callable<PCObject>{

    Compute server;
    IPCFunction function;
    PCObject params;

    
    public DistributeCall(Compute serv, IPCFunction toCall, PCObject pars){
        server = serv;
        function = toCall;
        params = pars;
    }

    public PCObject call() throws RemoteException{
        return server.callFunction(function, params);
    }

}