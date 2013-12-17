import java.io.*;
import java.util.Iterator;
import java.net.URL;

public class IO
{
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

		_obj.set("toString", new IPCFunction() {
			@Override
			public PCObject call(PCObject... args){
				PCObject origNum = args[0];
        		String result = getStringOfObj(origNum);
        		return new PCList(result);
			}
		});

	}

    public static <T> T get(String key) {
    	return _obj.<T>get(key);
    }
}