package pubCrawl.io;

import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;

public class DownloadTest {

    @Test
    public void TestDownloadGetString(){
        System.out.println("Download.GetString starting.");
        try {
            String page = Download.GetString("http://xkcd.com/100/");
            System.out.println("Download.GetString worked.");
            System.out.println("First line: " + page.split("\n")[0]);
        }
        catch (IOException ex) {
            System.out.println("Download.GetString failed.");
            Assert.fail();
            //ex.printStackTrace();
        }
    }

   @Test
    public void TestDownloadGetBytes() {
        System.out.println("Download.GetBytes starting.");
        try {
            byte[] img = Download.GetBytes("http://imgs.xkcd.com/comics/family_circus.jpg");
            System.out.println("Download.GetBytes worked.");
            System.out.println("First byte: " + img[0]);
        }
        catch (IOException ex) {
            System.out.println("Download.GetBytes failed.");
            //ex.printStackTrace();
            Assert.fail();
        }
    }

}
