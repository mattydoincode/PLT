import java.io.Serializable; 
      public class function_102 extends IPCFunction implements Serializable{
  public function_102(){}

  public PCObject call(PCObject... args){
      PCObject o = args[0];

      return Util.map.call(new PCList().add(new PCObject('h'))
                  .add(new PCObject('e'))
                  .add(new PCObject('y')),
            (PCObject)((PCList)((PCObject)o.get("a"))
             .get("b"))
             .get(new PCObject(0.000000)));

	}
  }