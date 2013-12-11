import java.io.Serializable;

public class TestFunction extends IPCFunction implements Serializable {

    public TestFunction(){

    }

    public PCObject call(PCObject... args) {
       System.out.print("whee!");
       return new PCObject(Math.random());

    }
}
