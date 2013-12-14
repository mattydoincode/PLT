/*import java.io.*;
import java.util.Iterator;


public class Downloader
{

    public static PCList download(PCList listOfChars) throws IOException {
        PCList toReturn = new PCList();
        String fileName = new String();

        for (Iterator<PCObject> iter = listOfChars.iterator(); iter.hasNext(); ) {
            PCObject element = iter.next();
            fileNAme += element.<Character>getBase();
        }

        URL myURL = new URL(fileName);

        BufferedReader in = new BufferedReader(
                        new InputStreamReader(
                        myUrl.openStream()));

        try {
            String line = br.readLine();
            while (line != null) {
                toReturn.add(new PCList(line));
                line = br.readLine();
            }
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        } finally {
            br.close();
        }
        return toReturn;
    }
}
*/