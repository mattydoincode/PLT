import java.util.Iterator;


public class DistributeTest {

    public static void main(String[] args){

        PCList testList = new PCList();
        for(int i = 0; i<12;i++){
            testList.add(new PCObject(i));
        }

        TestFunction tf = new TestFunction();


        DistributeClient client = new DistributeClient();

        client.getRegistries(args);

        PCList newList = client.distributeFunction(testList , tf);



        for(PCObject o : newList){
            // SIREESH THIS IS BREAKING FOR ME (ALDEN) SO IM COMMENTING OUT
            // System.out.println(o.getBase());
        }




    }
}
