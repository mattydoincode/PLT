import java.io.*;
import java.util.Iterator;

public class Writer
{
    public static void print(PCObject toPrint)
    {
    	PCList mylist = (PCList) toPrint;
        for(PCObject obj : mylist) {
            System.out.printf("%c", obj.<Character>getBase());
        }
        System.out.println();
       
    }

    public static void printFile(PCObject toPrint, PCList listOfChars) throws IOException {
        PCList mylist = (PCList) toPrint;
        String fileName = new String();

        for(Iterator<PCObject> iter = listOfChars.iterator(); iter.hasNext(); ) {
            PCObject element = iter.next();
            fileName += element.<char>getBase();
        }
        File f = new File(fileName);
        if (!f.exists()) {
            f.createNewFile();
        }
        PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(fileName)));

        for(PCObject obj : mylist)
        {
            out.println(obj.<String>getBase());
        }
        out.close();

       
    }
}
