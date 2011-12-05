import static out.Out.o;
/**
This is an example showing how var args are used.
**/ 
public class VarArgsEx{
    public int vaMethod(int ... ints){
	int sum = 0;
	for(int i : ints) sum += i;
	return sum;
    }

    public int vaMethod(String message, int ... ints){
	o(message);
	o(String.format("Ints is %d long and index 4 contains %d%n",
			ints.length,ints[3]));  
	return vaMethod(ints);
    }

    /* Not allowed -> var args parameter must be the last parameter
    public int vaMethod(int ... ints, String message){
	return vaMethod(message, ints);
	} */
}
    