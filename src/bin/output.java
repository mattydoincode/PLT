
  public class output
  {
      public static void main(String[] args)
      {

PCList list1 = (PCList)(new PCList().add(new PCObject (1)).add(new PCObject (2)).add(new PCObject (3)).add(new PCObject (4)).add(new PCObject (5)).add(new PCObject (6)));
IPCFunction fun = (IPCFunction)(new function_101());
PCList result1 = (PCList)(List.<IPCFunction>get("where").call(list1,fun));
Writer.print(result1.<PCObject>get(new PCObject (0)));
Writer.print(result1.<PCObject>get(new PCObject (1)));
Writer.print(result1.<PCObject>get(new PCObject (2)));

      } 
  }
  