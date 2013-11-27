import java.util.Iterator;

public class Writer{
    
    public static PCList print(PCList toPrint){
        for(Iterator<PCObject> it = toPrint.iterator();it.hasNext();){
            System.out.printf("%c", it.next().getBase());
        }

        return toPrint;
    }
}