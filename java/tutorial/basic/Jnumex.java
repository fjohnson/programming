public class Jnumex{
    /**
     Here is an example of the new number declaration format in Java 7.
    **/
    static void o(long o){
	System.out.println(o);
    }

    public static void main(String args[]){
	long hexword = 0xDEAD_BEEF;
	long nybble = 0b1001_1101;
	byte b = 127;
	o(hexword);
	o(nybble);
	o(b);
	b+=1;
	o(b);
	o(4.0);
    }
}
	