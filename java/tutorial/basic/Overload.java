public class Overload{
    public int o(int o){
	System.out.println("This is an int");
	System.out.println(o);
	return o;
    }

    /* This is not allowed. Overloaded methods are distinguished
       by their parameter list and not their return type. 
       error: o(int) is already defined in Overload.
    
    public void o(int o){
	System.out.println("This is an int");
	System.out.println(o);
    }
    */
    public double o(double o){
	System.out.println("This is a double");
	System.out.println(o);
	return o;
    }
}
