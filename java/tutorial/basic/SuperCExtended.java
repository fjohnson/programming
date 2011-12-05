/**
  Here is a class that illustrates how super constructors must be 
  explicitly called less a default no arg constructor (that may not
  exist) is called in its stead. This example also illustrates the
  creation of a default no arg constructor when no constructors are
  defined. 

  Program output:
  
  $ java SuperCExtended 1
  Default constructor called
  
  $ java SuperCExtended 2
  Everything is...: 100 percent fine
  
  $ java SuperCExtended 3
  Default constructor called
**/
public class SuperCExtended extends SuperC{
    
    public SuperCExtended(String gravy){
	//calls super() because no super constructor has been specified.
    }
    
    public SuperCExtended(Integer number){
	super(number+" percent fine"); //must be first statement in constructor.
    }
	    
    public static void main(String args[]){
	switch(args[0]){
	case "1":
	    new SuperCExtended("Gravy!");
	    break;
	case "2":
	    new SuperCExtended(100);
	    break;
	case "3":
	    //Create a class wither no constructor to illustrate how
	    //a no argument constructor is created. 
	    class Another extends SuperC{};
	    new Another();
	    break;
	}
    }
}