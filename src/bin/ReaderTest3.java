/*
import org.junit.Assert;
import org.junit.Test;



import java.io.IOException;
import java.io.Reader;

public class ReaderTest3 {



    @Test
    public void TestReadFromFile() {

        String EXISTING_FILE = "/Users/kevinmangan/School/PLT/docs/haveDone.txt";
        PCList fileName = new PCList();
        for(Character ch : EXISTING_FILE.toCharArray()){
            fileName.add(new PCList(ch));
        }
        System.out.println("Reader.FromFile starting.");
        try {
            PCList lines = Reader.readFile(fileName);
            System.out.println("Read.FromFile worked.");
            for(PCObject obj : lines)
            {
                System.out.printf("%c", obj.<Character>getBase());
            }
            System.out.println();
        }
        catch (IOException ex) {
            System.out.println("Read.FromFile failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }

    @Test
    public void TestReadromStdIn() {
        String EXISTING_FILE = "/Users/kevinmangan/School/PLT/docs/haveDone.txt";
        PCList fileName = new PCList();
        for(char ch : EXISTING_FILE.toCharArray()){
            fileName.add(new PCList(ch));
        }
        System.out.println("Read.FromStdIn starting.");
        try {
            PCList lines = Reader.read();
            System.out.println("Read.FromStdIn worked.");
            for(PCObject obj : lines)
            {
                System.out.printf("%c", obj.<Character>getBase());
            }
            System.out.println();
        }
        catch (IOException ex) {
            System.out.println("Read.FromStdIn failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }  

}
*/