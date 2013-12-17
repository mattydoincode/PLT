import java.io.*;
import java.util.Iterator;
import java.net.*;


public class MakeString
{
    public static PCList makeString(PCObject origNum){
        String result = Writer.getStringOfObj(origNum);
        return new PCList(result);
    }
}
    