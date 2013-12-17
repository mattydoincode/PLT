import java.io.*;
import java.util.Iterator;

public class Writer
{
    public static PCObject print(PCObject toPrint)
    {
        if(toPrint instanceof PCList) {
        	PCList mylist = (PCList) toPrint;
            for(PCObject obj : mylist) {
                System.out.print(getStringOfObj(obj));
            }
        }
        else{
            System.out.print(getStringOfObj(toPrint));
        }
        System.out.println();
        return toPrint;
    }

    public static String getStringOfObj(PCObject obj){
        Object myobject = obj.<Object>getBase();
        if(myobject instanceof Double){
            Double mynum = obj.<Double>getBase();
            if(Math.floor(mynum) == mynum){
                return (new Integer(mynum.intValue())).toString();
            }
            else {
                return mynum.toString();
            }
        }
        else {
            return myobject.toString();
        }
    }

    public static PCObject printFile(PCObject toPrint, PCList listOfChars) {
        String fileName = new String();

        for(PCObject element : listOfChars) {
            fileName += element.<Character>getBase();
        }

        try {
            File f = new File(fileName);
            if (!f.exists()) {
                f.createNewFile();
            }
            PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(fileName)));

            if(toPrint instanceof PCList) {
                PCList mylist = (PCList) toPrint;
                for(PCObject obj : mylist) {
                    out.print(getStringOfObj(obj));
                }
            }
            else{
                out.print(getStringOfObj(toPrint));
            }
            out.close();
        }
        catch(IOException e) {
            e.printStackTrace();
        }
        return toPrint;
    }
}
