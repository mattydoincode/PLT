package cs4115.pubCrawl.io;

import java.io.*;
import java.util.ArrayList;

public final class Read {

    public static ArrayList<String> FromFile(String file) throws IOException {
        return FromReader(new FileReader(file));
    }

    public static ArrayList<String> FromStdIn() throws IOException {
        return FromReader(new InputStreamReader(System.in));
    }

    private static ArrayList<String> FromReader(Reader in) throws IOException {
        ArrayList<String> lines = new ArrayList<String>();

        BufferedReader br = new BufferedReader(in);
        try {
            String line = br.readLine();
            while (line != null) {
                lines.add(line);
                line = br.readLine();
            }
        }
        finally {
            br.close();
        }

        return lines;
    }

}
