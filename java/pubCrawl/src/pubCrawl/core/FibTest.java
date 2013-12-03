package pubCrawl.core;

/**
 * Created with IntelliJ IDEA.
 * User: sireesh
 * Date: 11/24/13
 * Time: 1:20 PM
 * To change this template use File | Settings | File Templates.
 */
public class FibTest extends PCObject implements IPCFunction {

    public PCObject call(PCObject... x){
        PCObject y = x[0];
        int yInt = y.getBase();

        if(yInt == 1){
            return new PCObject(1);
        }else {
            return new PCObject(2);

        }

    }
}
