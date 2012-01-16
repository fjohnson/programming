package context;

import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXBElement;

import org.example.carmodel.Car;
import org.example.carmodel.Fleet;
import org.example.carmodel.Planet;

@Path("/xmlexample/")
public class xmlUrlRootResource {
	Map<String,Fleet> fleets = new HashMap<String,Fleet>();
	{
		Fleet defaultFleet = new Fleet();
		fleets.put("Default Fleet", defaultFleet);
	}
	/*
	 * Actions:
	 * Add car to default fleet
	 * Get cars matching name from default fleet
	 * Add fleet
	 * Get fleet
	 * Add car to fleet
	 * Remove fleet
	 * Remove car from fleet
	 * Remove car from default fleet
	 */
	
	@Path("/car")
	@PUT
	@Consumes(MediaType.APPLICATION_XML)
	@Produces(MediaType.APPLICATION_XML)
	public Car addNewCar(Car car){
		fleets.get("Default Fleet").getCars().add(car);
		return car;
	}
	
	@Path("/car2")
	@PUT
	@Consumes({MediaType.APPLICATION_XML,MediaType.TEXT_XML})
	@Produces(MediaType.APPLICATION_XML)
	public String addNewCar2(JAXBElement fi){
		return "<hi>op</hi>";
	}
	
	 @GET
	       @Produces(MediaType.APPLICATION_XML)
	       public Planet getPlanet() {
	           Planet p = new Planet();
	           p.id = 1;
	           p.name = "Earth";
	          p.radius = 1.0;
	  
	          return p;
      }
	 
	 @PUT 
	 @Consumes(MediaType.APPLICATION_XML)
	 @Produces(MediaType.APPLICATION_XML)
	 public Planet addPlanet(Planet p) {
		 System.out.println("Got here");
		 //return p.getValue();
		 return p;
	 }
	 
	 @PUT 
	 @Path("etc")
	 @Consumes(MediaType.APPLICATION_XML)
	 @Produces(MediaType.APPLICATION_XML)
	 public String testFunc(String in){
		 return in;
	 }
	 
}
