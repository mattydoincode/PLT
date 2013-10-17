package pubCrawl.distribute;

import pubCrawl.core.IPCFunction;
import pubCrawl.core.PCList;
import pubCrawl.core.PCObject;

import java.io.IOException;
import java.net.UnknownHostException;

import java.net.Socket;
import java.util.ArrayList;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Iterator;


public class distributeServer {

    private ArrayList<ObjectOutputStream> outStream_list;
    private ArrayList<ObjectInputStream> inStream_list;


    public distributeServer(String[] hosts) throws IOException{
        connectToSlaves(hosts);

    }

    /**
     * A method that connects to a list of hosts provided
     *
     * @param hosts An array of the hosts to connect to, in String form
     * @throws IOException Thrown if the machine has a problem connnecting the sockets
     */
    public void connectToSlaves(String[] hosts) throws  IOException{
         Socket tempSlave;

        for(int i = 0;i<hosts.length;i++){
            tempSlave = new Socket(hosts[i], 1337);
            outStream_list.add(new ObjectOutputStream(tempSlave.getOutputStream()));
            inStream_list.add( new ObjectInputStream(tempSlave.getInputStream()));
        }
    }

    /**
     *
     * @param function
     * @param toDistribute
     * @return
     */
    public PCList distribute(IPCFunction function, PCList toDistribute) throws IOException , ClassNotFoundException {

        Iterator<ObjectOutputStream> outStreams = outStream_list.iterator();
        Iterator<ObjectInputStream> inStreams = inStream_list.iterator();
        ObjectOutputStream tempSlave_out;
        ObjectInputStream tempSlave_in;
        PCObject tempObject;

        for(int i = 0; i<toDistribute.size();i++){
            if(!outStreams.hasNext())
                outStreams = outStream_list.iterator();

            tempSlave_out = outStreams.next();
            tempObject = toDistribute.get(i);

            //NEED A THING TO CHECK METHOD SIGNATURE
            tempSlave_out.writeUTF("function_name");
            tempSlave_out.writeObject(tempObject);
        }

        PCList toReturn = new PCList();

        for(int i = 0; i<toDistribute.size();i++){
            if(!inStreams.hasNext())
                inStreams = inStream_list.iterator();

            tempSlave_in = inStreams.next();

            tempSlave_in.readUTF();
            toReturn.add((PCObject)tempSlave_in.readObject());
        }

        return toReturn;

    }
}
