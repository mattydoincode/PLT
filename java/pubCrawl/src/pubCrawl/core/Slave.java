package pubCrawl.core;

import java.io.*;
import java.net.*;
import java.util.HashMap;
import java.lang.reflect.*;

public class Slave {

    public static void main(String[] args) throws IOException, ClassNotFoundException {
        int receiverPort = 1337;
        int bufferSize = 4096;

        ServerSocket receiverServerSocket = new ServerSocket(receiverPort);
        Socket receiverSocket;
        receiverSocket = receiverServerSocket.accept();
        String senderIP = receiverSocket.getInetAddress().getHostAddress();
        int senderPort = receiverSocket.getPort();
        Socket senderSocket = new Socket(senderIP, senderPort);

        OutputStream outStream = senderSocket.getOutputStream();
        InputStream inStream = receiverSocket.getInputStream();
        ObjectOutputStream oos = new ObjectOutputStream(outStream);
        ObjectInputStream ois = new ObjectInputStream(inStream);

        // Read received Data
        while (true) {

            // Stage One: Listen for <Number of functions>
            int numOfFuncs = ois.readInt();

            HashMap<String, IPCFunction> functions = new HashMap<String, IPCFunction>();
            for(int i = 0; i < numOfFuncs; i++){
                // deserialize and put into hashmap
                PCObject obj = new PCObject();
                obj = (PCObject)ois.readObject(); // Are you serializing each one separately, or is it a PCList of PCObjects?
                functions.put(); // how do we get the name of the function?
            }




            // Stage Two: Input is now in the form of a Function to be called and a PCObject
            while (PCObject does not contain end flag) {

                String functionName = ois.readUTF();
                PCObject result = new PCObject();

                if(functions.containsKey(functionName)) {
                    IPCFunction method = (IPCFunction)functions.get(functionName);
                    Array arguments = ois.readArray(); // he might send an Array of PCObjects
                    result = method.call(arguments); // can we have call take in a PCList?
                }

                oos.write(result);

            }
        }

    }


}
