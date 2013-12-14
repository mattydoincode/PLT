
  import java.io.Serializable; 
  public class function_102 extends IPCFunction implements Serializable
  {
      public function_102() {}

      public PCObject call(PCObject... args)
      {
          PCObject y = (PCObject)args[0];
IPCFunction f1 = (IPCFunction)args[1];

          
    if (new PCObject(y.<Double>getBase() < new PCObject(1).<Double>getBase()).<Boolean>getBase())
    {
        return new PCObject(1);
    }


PCObject smaller = (PCObject)(new PCObject(y.<Double>getBase() / new PCObject(2).<Double>getBase()));
return new PCObject(y.<Double>getBase() + f1.call(smaller,this).<Double>getBase());
      }
  }
  