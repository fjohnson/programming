import static out.Out.o;
/**
  This is a class to illustrate how static and instance code blocks
  are used for initialization.
  
  Program output:

  $ java InitializeBlocks 1
  Initializing the static message
  Initializing the static quantity
  Initializing instance var one
  Initializing instance var two
  Constructor one here...
  100
  25
  Hello world
  300
  $ java InitializeBlocks 2
  Initializing the static message
  Initializing the static quantity
  Initializing instance var one
  Initializing instance var two
  Constructor two here...
  100
  25
  Hello world
  300
  $ java InitializeBlocks 3
  Initializing the static message
  Initializing the static quantity
  Initializing instance var one
  Initializing instance var two
  Constructor one here...
  100
  25
  Hello world
  300
  Initializing instance var one
  Initializing instance var two
  Constructor two here...
  100
  25
  Hello world
  300
  $ 
**/
public class InitializeBlocks{
    static String smessage;
    static final int quantity;

    //Static blocks are executed in the order they are defined.
    static {
	o("Initializing the static message");
	smessage = "Hello world";
    }

    int instanceVar1;
    int instanceVar2;

    //Each code block is copied into every constructor
    {
	o("Initializing instance var one");
	instanceVar1 = 100;
    }

    private InitializeBlocks(){
	o("Constructor one here...");
	o(instanceVar1);
	o(instanceVar2);
	o(smessage);
	o(quantity);
    }

    private InitializeBlocks(int k){
	o("Constructor two here...");
	o(instanceVar1);
	o(instanceVar2);
	o(smessage);
	o(quantity);
    }

    {
	o("Initializing instance var two");
	instanceVar2 = 25;
    }

    public static void main(String args[]){
	switch(args[0]){
	case "1":
	    new InitializeBlocks();
	    break;
	case "2":
	    new InitializeBlocks(3);
	    break;
	case "3":
	    new InitializeBlocks();
	    new InitializeBlocks(3);
	}
    }

    //Static blocks are executed in the order they are defined.
    static {
	o("Initializing the static quantity");
	quantity = 300;
    }
}