package cs4115.pubCrawl;

import java.io.*;
import java.util.*;

import cs4115.pubCrawl.io.Download;
import cs4115.pubCrawl.io.Read;
import cs4115.pubCrawl.io.Write;

public class Main {

    private final static String EXISTING_FILE = "C:\\src\\columbia\\PLT\\README.md";

    public static void main(String[] args) {
        TestDownloadGetString();
        TestDownloadGetBytes();
        TestReadFromFile();
        // TestReadromStdIn(); // can't send EOF in IntelliJ console, so not running this
        TestWriteToFile();
        TestWriteToStdOut();
        TestWriteBytesToFile();
    }

    private static void TestDownloadGetString() {
        System.out.println("Download.GetString starting.");
        try {
            String page = Download.GetString("http://xkcd.com/100/");
            System.out.println("Download.GetString worked.");
            System.out.println("First line: " + page.split("\n")[0]);
        }
        catch (IOException ex) {
            System.out.println("Download.GetString failed.");
            ex.printStackTrace();
        }
    }

    private static void TestDownloadGetBytes() {
        System.out.println("Download.GetBytes starting.");
        try {
            byte[] img = Download.GetBytes("http://imgs.xkcd.com/comics/family_circus.jpg");
            System.out.println("Download.GetBytes worked.");
            System.out.println("First byte: " + img[0]);
        }
        catch (IOException ex) {
            System.out.println("Download.GetBytes failed.");
            ex.printStackTrace();
        }
    }

    private static void TestReadFromFile() {
        System.out.println("Read.FromFile starting.");
        try {
            ArrayList<String> lines = Read.FromFile(EXISTING_FILE);
            System.out.println("Read.FromFile worked.");
            System.out.println("First line: " + lines.get(0));
        }
        catch (IOException ex) {
            System.out.println("Read.FromFile failed.");
            ex.printStackTrace();
        }
    }

    private static void TestReadromStdIn() {
        System.out.println("Read.FromStdIn starting.");
        try {
            ArrayList<String> lines = Read.FromStdIn();
            System.out.println("Read.FromStdIn worked.");
            System.out.println("First line: " + lines.get(0));
        }
        catch (IOException ex) {
            System.out.println("Read.FromStdIn failed.");
            ex.printStackTrace();
        }
    }

    private static void TestWriteToFile() {
        System.out.println("Write.ToFile starting.");
        try {
            Write.ToFile("testing", EXISTING_FILE);
            System.out.println("Write.ToFile worked.");
        }
        catch (IOException ex) {
            System.out.println("Write.ToFile failed.");
            ex.printStackTrace();
        }
    }

    private static void TestWriteToStdOut() {
        System.out.println("Write.ToFile starting.");
        Write.ToStdOut("testing");
        System.out.println("Write.ToFile worked.");
    }

    private static void TestWriteBytesToFile() {
        System.out.println("Write.BytesToFile starting.");
        try {
            byte[] img = Download.GetBytes("http://imgs.xkcd.com/comics/family_circus.jpg");
            Write.ToFile(img, "C:\\src\\columbia\\PLT\\img.jpg");
            System.out.println("Write.BytesToFile worked.");
        }
        catch (IOException ex) {
            System.out.println("Write.BytesToFile failed.");
            ex.printStackTrace();
        }
    }

}
