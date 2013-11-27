import java.io.Serializable; 
      public class function_101 extends PCObject implements IPCFunction, Serializable{
  public function_101(){}

  public PCObject call(PCObject... args){
  PCObject a = args[0];
PCObject b = args[1];

  return new PCObject(a.<Double>getBase() + b.<Double>getBase());

}
  }