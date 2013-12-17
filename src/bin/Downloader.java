import java.io.*;
import java.util.Iterator;
import java.net.*;


public class Downloader
{
    public static PCList download(PCList listOfChars){
        String fileName = new String();

        for (PCObject element : listOfChars) {
            fileName += element.<Character>getBase();
        }
        try{
        URL myURL = new URL(fileName);

        BufferedReader in = new BufferedReader(
                        new InputStreamReader(
                        myURL.openStream()));
        String result = "";
        try {
            String line = in.readLine();
            while (line != null) 
            {
                result += line;
                line = in.readLine();
            }
            
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } finally {
            in.close();
        }    }
        catch(IOException e){}
        return new PCList(result);
    }
}
    