import java.util.Iterator;

public class Writer{
    
    public static PCList print(PCObject toPrint){
    	PCList mylist = (PCList) toPrint;
        for(Iterator<PCObject> it = mylist.iterator();it.hasNext();){
            System.out.printf("%c", it.next().<Character>getBase());
        }

        return mylist;
    }
}