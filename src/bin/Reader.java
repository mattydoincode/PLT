import java.io.*;
import java.util.Iterator;


public class Reader
{

    public static PCList read()
    {
        BufferedReader inStream = new BufferedReader(new InputStreamReader(System.in));
        try {
            String temp;
            return new PCList(inStream.readLine());
            }
        catch(IOException e) {
            e.printStackTrace();
        }

        return new PCList();
    }

    public static PCList readFile(PCList listOfChars)  {
        PCList toReturn = new PCList();
        String fileName = new String();

        for (Iterator<PCObject> iter = listOfChars.iterator(); iter.hasNext(); ) {
            PCObject element = iter.next();
            fileName += element.<Character>getBase();
        }

        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line = br.readLine();
            while (line != null) {
                toReturn.add(new PCList(line));
                line = br.readLine();
            }
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
        return toReturn;
    }
}
