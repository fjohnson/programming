package jaxb.example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name="Car")
public class CarModel {
	public int maxspeed = 120;
	public int gears = 4;
	public String name = "toyota";
	
	@XmlElement(name = "owner") //Expose private members for marshalling.
	private String theowner = "Fletcher";
	
	@XmlElement(name = "ownerLastName") //Expose protected.
	protected String ownerLastName = "Johnson";
		
	/*Create nested elements "year" with common root "RepairYears"*/
	@XmlElementWrapper(name = "RepairYears") 
	@XmlElement(name = "year")
	List<Integer> intlist = new ArrayList<Integer>(Arrays.asList(1976,1979,1994));
		
}
