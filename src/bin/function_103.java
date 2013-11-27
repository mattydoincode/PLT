import java.io.Serializable; 
      public class function_103 extends IPCFunction implements Serializable{
  public function_103(){}

  public PCObject call(PCObject... args){
  PCObject x = args[0];

  return new PCObject()
	.set("a",new PCObject()
	.set("b",new PCList().add(x)));

}
  }