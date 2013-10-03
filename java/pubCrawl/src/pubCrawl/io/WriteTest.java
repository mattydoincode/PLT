package pubCrawl.io;

import org.junit.Assert;
import org.junit.Test;
import java.io.IOException;

/**
 * Created with IntelliJ IDEA.
 * User: Matt
 * Date: 9/30/13
 * Time: 8:28 PM
 * To change this template use File | Settings | File Templates.
 */
public class WriteTest {

    private final String EXISTING_FILE = "C:\\src\\columbia\\PLT\\README.md";

    @Test
    public void TestWriteToFile() {
        System.out.println("Write.ToFile starting.");
        try {
            Write.ToFile("testing", EXISTING_FILE);
            System.out.println("Write.ToFile worked.");
        }
        catch (IOException ex) {
            System.out.println("Write.ToFile failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }
    @Test
    public void TestWriteToStdOut() {
        System.out.println("Write.ToFile starting.");
        Write.ToStdOut("testing");
        System.out.println("Write.ToFile worked.");
    }
    @Test
    public void TestWriteBytesToFile() {
        System.out.println("Write.BytesToFile starting.");
        try {
            byte[] img = Download.GetBytes("http://imgs.xkcd.com/comics/family_circus.jpg");
            Write.ToFile(img, "C:\\src\\columbia\\PLT\\img.jpg");
            System.out.println("Write.BytesToFile worked.");
        }
        catch (IOException ex) {
            System.out.println("Write.BytesToFile failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }
}
