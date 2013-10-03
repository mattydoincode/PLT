package pubCrawl.io;

import java.io.IOException;
import java.util.ArrayList;

import org.junit.Assert;
import org.junit.Test;

public class ReadTest {

    private final String EXISTING_FILE = "C:\\src\\columbia\\PLT\\README.md";

    @Test
    public void TestReadFromFile() {
        System.out.println("Read.FromFile starting.");
        try {
            ArrayList<String> lines = Read.FromFile(EXISTING_FILE);
            System.out.println("Read.FromFile worked.");
            System.out.println("First line: " + lines.get(0));
        }
        catch (IOException ex) {
            System.out.println("Read.FromFile failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }

    //@Test
    public void TestReadromStdIn() {
        System.out.println("Read.FromStdIn starting.");
        try {
            ArrayList<String> lines = Read.FromStdIn();
            System.out.println("Read.FromStdIn worked.");
            System.out.println("First line: " + lines.get(0));
        }
        catch (IOException ex) {
            System.out.println("Read.FromStdIn failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }

}
