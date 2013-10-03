package pubCrawl.json;

import org.junit.Test;
import org.junit.Assert;
/**
 * Created with IntelliJ IDEA.
 * User: Matt
 * Date: 10/3/13
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */
public class PCObjectTest {

    @Test
    public void BasicObjectCreation(){
    /*
        mynum = 5;
        mystring = "hey";
        myobject = {
            'a': 5,
            'b': 'testing',
            'd': {
                'x': 5',
                'y': 'hey'
            }
        };
     */
        PCObject mynum = new PCObject(5);
        //PCObject mystring = "hey"; ?????? TODO
        PCObject myObject = new PCObject();
        myObject.Set("a", 5);
        myObject.Set("b", "testing");
        PCObject __d = new PCObject();
        __d.Set("x", 5);
        __d.Set("y", "hey");
        myObject.Set("d", __d);
    }

    @Test
    public void InnerBubbleSort(){
    /*
1         bubble(values) -> {
2             swapped = true;
3             while(swapped) {
4                 swapped = false;
5                 for(i = 0; i < values.length-1; i++) {
6                     if (values[i] > values[i+1]) {
7                         temp = values[i];
8                         values[i] = values[i+1];
9                         values[i+1] = temp;
10                        swapped = true;
11                    }
12                }
13            }
14            return values;
15        }
     */
    //at line 5, we know that values must be a PCObject with a property 'length'
    //at line 5, we know that i is an integer
    //at line 6 we know that values must be a PCList (:PCObject) because of index notation WITH INTEGER
    //Therefore we know bubble must take a pclist!

        PCList values = new PCList(); //this was passed in with some values
        values.Add(new PCObject(5)); //so for now i'll just put em in here
        values.Add(new PCObject(8));
        values.Add(new PCObject(2));
        values.Add(new PCObject(4));
        values.Add(new PCObject(7));
        values.Add(new PCObject(3));
        //at this point we're writing the code that would be written to represent the function
        PCObject swapped = new PCObject(true);
        while(swapped.<Boolean>GetBase()){ //we know that a while loop needs a boolean, so swapped must be a boolean
            swapped = new PCObject(false);
            for(int i = 0; i < values.Length()-1; i++){ //if i was already declared, ideally we can use it, but "int" goes away
                if(values.Get(i).<Double>GetBase() > values.Get(i+1).<Double>GetBase()) { // > only with nums yo
                    PCObject temp = values.Get(i);
                    values.Set(i,values.Get(i+1));
                    values.Set(i+1, temp);
                    swapped = new PCObject(true);
                }
            }
        }
        //FOR OUR PURPOSES, LET'S PRINT THIS OUT TO MAKE SURE IT WORKS
        for(int j = 0; j < values.Length(); j++){
            System.out.println(values.Get(j).<Double>GetBase());
        }
    }


}
