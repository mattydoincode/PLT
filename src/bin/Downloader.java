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

        String result = "";
        BufferedReader in = null;
        try {
            URL myURL = new URL(fileName);

            in = new BufferedReader(new InputStreamReader(myURL.openStream()));
            String line = in.readLine();
            while (line != null) {
                result += line + "\n";
                line = in.readLine();
            }
        }
        catch (IOException e) {} 
        finally {
            if (in != null) {
                try {
                    in.close();                    
                }
                catch(IOException ex) {}
            }
        }

        return new PCList(result);
    }
}
    