
  import java.io.Serializable; 
  public class function_101 extends IPCFunction implements Serializable
  {
      public function_101() {}

      public PCObject call(PCObject... args)
      {
          PCObject x = (PCObject)args[0];
IPCFunction f2 = (IPCFunction)args[1];

          
    if (new PCObject(x.<Double>getBase() < new PCObject(1).<Double>getBase()).<Boolean>getBase())
    {
        return new PCObject(1);
    }


PCObject smaller = (PCObject)(new PCObject(x.<Double>getBase() - new PCObject(1).<Double>getBase()));
return new PCObject(x.<Double>getBase() + f2.call(smaller,this).<Double>getBase());
      }
  }
  