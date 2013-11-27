  public class output{
      public static void main(String[] args){

      	IPCFunction x1 = new function_101();
		PCList x2 = (PCList)Util.map.call(new PCList().add(new PCObject(1.000000))
										 .add(new PCObject(2.000000))
										 .add(new PCObject(3.000000)), x1);

		IPCFunction x3 = new function_102();
		PCObject obj = new PCObject()
							.set("i",x1)
							.set("wrap",new function_103());

		PCList x4 = (PCList)x3.call(obj.<IPCFunction>get("wrap").call(obj.<IPCFunction>get("i")));
      } 
  } 