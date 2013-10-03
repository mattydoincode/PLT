package pubCrawl.io;

import java.io.*;

public final class Write {

    public static void ToFile(String content, String file) throws IOException {
        File f = new File(file);
        if (!f.exists()) {
            f.createNewFile();
        }
        PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(file, true)));
        out.println(content);
        out.close();
    }

    public static void ToFile(byte[] content, String file) throws IOException {
        File f = new File(file);
        if (!f.exists()) {
            f.createNewFile();
        }
        BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(file));
        bos.write(content);
        bos.flush();
        bos.close();
    }

    public static void ToStdOut(String content) {
        System.out.println(content);
    }

}
