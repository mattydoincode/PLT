import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: sireesh
 * Date: 10/26/13
 * Time: 9:59 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestFunction extends IPCFunction implements Serializable {

    public TestFunction(){

    }

    public PCObject call(PCObject... args) {
       return new PCObject();
    }
}
