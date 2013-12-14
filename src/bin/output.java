
  public class output
  {
      public static void main(String[] args)
      {

IPCFunction func1 = (IPCFunction)(new function_101());
IPCFunction func2 = (IPCFunction)(new function_102());
PCObject result = (PCObject)(func1.call(new PCObject(100),func2));
Writer.print(result);

      } 
  }
  