
  import java.io.Serializable; 
  public class function_101 extends IPCFunction implements Serializable
  {
      public function_101() {}

      public PCObject call(PCObject... args)
      {
          PCObject a = (PCObject)args[0];
PCObject b = (PCObject)args[1];

          while (new PCObject(!(a.equals(b))).<Boolean>getBase())
{

    if (new PCObject(a.<Double>getBase() > b.<Double>getBase()).<Boolean>getBase())
    {
        a = (PCObject)(new PCObject(a.<Double>getBase() - b.<Double>getBase()));

    }

else{
b = (PCObject)(new PCObject(b.<Double>getBase() - a.<Double>getBase()));

}

}
return a;
      }
  }
  