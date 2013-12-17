import java.util.Iterator;
import java.io.Serializable;

public class DistributeTest {

    private class TestFunction extends IPCFunction implements Serializable {
        public PCObject call(PCObject... args) {
           System.out.print("whee!");
           return new PCObject(2);
        }
    }

    public void run(String[] args) {

        PCList testList = new PCList();
        for(int i = 0; i<12;i++){
            testList.add(new PCObject(i));
        }

        TestFunction tf = new TestFunction();

        DistributeClient client = new DistributeClient();

        client.setSlaves(args);

        PCList newList = client.distributeFunction(testList , tf);



        for(PCObject o : newList){
            // SIREESH THIS IS BREAKING FOR ME (ALDEN) SO IM COMMENTING OUT
            // System.out.println(o.getBase());
        }
    }

    public static void main(String[] args){
        new DistributeTest().run(args);
    }
}
