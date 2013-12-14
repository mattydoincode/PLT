
  import java.io.Serializable; 
  public class function_101 extends IPCFunction implements Serializable
  {
      public function_101() {}

      public PCObject call(PCObject... args)
      {
          PCObject x = (PCObject)args[0];

          
    if (new PCObject(new PCObject(x.<Double>getBase() % new PCObject (2).<Double>getBase()).equals(new PCObject (0))))
    {
        return new PCObject(true);
    }


return new PCObject(false);
      }
  }
  