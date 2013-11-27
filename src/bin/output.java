

  public class output{
      public static void main(String[] args){
      IPCFunction add = new function_101();
PCObject x = add.call(new PCObject(2.000000),new PCObject(3.000000));
PCObject y = new PCObject(x.<Double>getBase() + new PCObject(5.000000).<Double>getBase());
 y = new PCObject(y.<Double>getBase() % new PCObject(2.000000).<Double>getBase());

      } 
  } 