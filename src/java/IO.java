import java.io.*;
import java.util.Iterator;
import java.net.URL;

public class IO
{

	// Converts a number to a string and return that string
    private static String getStringOfObj(PCObject obj){
        Object myobject = obj.<Object>getBase();
        if(myobject instanceof Double){
            Double mynum = obj.<Double>getBase();
            if(Math.floor(mynum) == mynum){
                return (new Integer(mynum.intValue())).toString();
            }
            else {
                return mynum.toString();
            }
        }
        else {
            return myobject.toString();
        }
    }

	private static PCObject _obj = new PCObject();

	static {

		// Read from stdin
		_obj.set("read", new IPCFunction(){
			@Override
			public PCObject call(PCObject... args){
		        try {
		        	BufferedReader inStream = new BufferedReader(new InputStreamReader(System.in));
		            return new PCList(inStream.readLine());
	            }
		        catch(IOException e) {
		            e.printStackTrace();
		        }
        		return new PCList();
			}
		});

		// Read from a file
		_obj.set("readFile", new IPCFunction(){
			@Override
			public PCObject call(PCObject... args){
				PCList listOfChars = (PCList)args[0];

		        PCList toReturn = new PCList();
		        String fileName = new String();

		        for (PCObject element : listOfChars) {
		            fileName += element.<Character>getBase();
		        }
		        BufferedReader br;
		        try {
		            br = new BufferedReader(new FileReader(fileName));
		            String line = br.readLine();
		            while (line != null) {
		                toReturn.add(new PCList(line));
		                line = br.readLine();
		            }
		        } 
		        catch (IOException e) {
		            e.printStackTrace();
		        }
		        return toReturn;
			}
		});

		// Download given url
		_obj.set("download", new IPCFunction() {
			@Override
			public PCObject call(PCObject... args){
		        String fileName = new String();
	        	PCList listOfChars = (PCList)args[0];
		        for (PCObject element : listOfChars) {
		            fileName += element.<Character>getBase();
		        }

		        String result = "";
		        BufferedReader in = null;
		        try {
		            URL myURL = new URL(fileName);

		            in = new BufferedReader(new InputStreamReader(myURL.openStream()));
		            String line = in.readLine();
		            while (line != null) {
		                result += line + "\n";
		                line = in.readLine();
		            }
		        }
		        catch (IOException e) {} 
		        finally {
		            if (in != null) {
		                try {
		                    in.close();                    
		                }
		                catch(IOException ex) {}
		            }
		        }

		        return new PCList(result);
			}
		});

		// Print to stdout
		_obj.set("print", new IPCFunction(){
			@Override
			public PCObject call(PCObject... args){
				PCObject toPrint = (PCObject)args[0];
				if(toPrint instanceof PCList) {
        			PCList mylist = (PCList) toPrint;
            		for(PCObject obj : mylist) {
                		System.out.print(getStringOfObj(obj));
            		}
        		}
		        else{
		            System.out.print(getStringOfObj(toPrint));
		        }
		        System.out.println();
		        return toPrint;

					}
		});

		// Print to a file
		_obj.set("printFile", new IPCFunction(){
			@Override
			public PCObject call(PCObject... args){
				PCObject toPrint  = (PCObject)args[0];
				PCList listOfChars = (PCList)args[1];
				String fileName = new String();

				for(PCObject element : listOfChars) {
				    fileName += element.<Character>getBase();
				}

				try {
				    File f = new File(fileName);
				    if (!f.exists()) {
				        f.createNewFile();
				    }
				    PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(fileName)));

				    if(toPrint instanceof PCList) {
				        PCList mylist = (PCList) toPrint;
				        for(PCObject obj : mylist) {
				            out.print(getStringOfObj(obj));
				        }
				    }
				    else{
				        out.print(getStringOfObj(toPrint));
				    }
				    out.close();
				}
				catch(IOException e) {
				    e.printStackTrace();
				}
				return toPrint;


			}
		});

		// Convert a number to a string and return a PCObject
		_obj.set("numToString", new IPCFunction() {
			@Override
			public PCObject call(PCObject... args){
				PCObject origNum = args[0];
        		String result = getStringOfObj(origNum);
        		return new PCList(result);
			}
		});

		// Convert a string to a number and return a PCObject
		_obj.set("numFromString", new IPCFunction() {
			@Override
			public PCObject call(PCObject... args){
				String s = new String();
				for(PCObject element : (PCList)args[0]) {
				    s += element.<Character>getBase();
				}
				return new PCObject(Double.parseDouble(s));
			}
		});

	}

    public static IPCFunction get(String key) {
    	return _obj.<IPCFunction>get(key);
    }
}