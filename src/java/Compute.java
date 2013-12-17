import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Compute extends Remote
{
    PCObject callFunction(IPCFunction function, PCObject param) throws RemoteException;
}
