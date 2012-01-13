package org.example.carmodel.creation;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.List;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import org.example.carmodel.Car;
import org.example.carmodel.Fleet;
import org.example.carmodel.ObjectFactory;
import org.example.carmodel.Owner;

/**
 * Example class showing how to actually use the java xml/schema bindings
 * created by JAXB. 
 * @author Fletcher Johnson
 *
 */
public class CreateCarModel{
	static JAXBElement<Fleet> assembleFleet() throws DatatypeConfigurationException{
		ObjectFactory of = new ObjectFactory();
		DatatypeFactory dtf = DatatypeFactory.newInstance();
		
		//Create the owner of the first car
		Owner o = new Owner();
		o.setFirstname("Fletcher");
		o.setLastname("Johnson");
		o.setHeight("6'1");
		o.setSex("male");
		o.setWeight("150");
				
		XMLGregorianCalendar dob = dtf.newXMLGregorianCalendar();
		dob.setYear(2052);
		dob.setDay(15);
		dob.setMonth(8);
		o.setDob(dob);
		
		Car c1 = of.createCar();
		c1.setMaxspeed(100);
		c1.setName("Toyota");
		c1.setOwner(o);
		
		Owner o2 = new Owner();
		o.setFirstname("Owen");
		o.setLastname("Pat");
		o.setHeight("5'8");
		o.setSex("male");
		o.setWeight("168");
		
		XMLGregorianCalendar dob2 = dtf.newXMLGregorianCalendar();
		dob2.setYear(2043);
		dob2.setDay(12);
		dob2.setMonth(4);
		o2.setDob(dob2);
		
		Car c2 = of.createCar();
		c2.setMaxspeed(200);
		c2.setName("Honda");
		c2.setOwner(o2);
		
		
		Fleet fleet = of.createFleet();
		List<Car> cl = fleet.getCars();
		cl.add(c1); cl.add(c2);
		return of.createFleet(fleet);
	}
	
	static void outputFleetXML(JAXBElement<Fleet> fleet, PipedOutputStream os) throws JAXBException, IOException{
		outputFleetXML(fleet, (OutputStream) os);
		os.close();
	}
	
	/**
	 * Output XML from a Fleet object.
	 * @param fleet
	 * @param os
	 * @throws JAXBException
	 * @throws IOException
	 */
	static void outputFleetXML(JAXBElement<Fleet> fleet, OutputStream os) throws JAXBException, IOException{
		JAXBContext context = JAXBContext.newInstance( "org.example.carmodel" );
		Marshaller m = context.createMarshaller();
		m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
		m.marshal(fleet, os);
	}
	
	/**
	 * Read in created XML from outputFleetXML() and recreate the Fleet element
	 * used to generate the XML. 
	 * @param os
	 * @throws IOException
	 * @throws JAXBException
	 * @throws DatatypeConfigurationException
	 */
	static void inputFleetXML(final OutputStream os) throws IOException, JAXBException, DatatypeConfigurationException{
		
		final PipedInputStream pis = new PipedInputStream();
		PipedOutputStream pos = new PipedOutputStream(pis);
		outputFleetXML(assembleFleet(), pos);
		
		new Thread(){
			public void start(){
				try{
					JAXBContext context = JAXBContext.newInstance( "org.example.carmodel" );
					Unmarshaller m = context.createUnmarshaller();
					
					JAXBElement<Fleet> fleet = (JAXBElement<Fleet>) m.unmarshal(pis);
					Fleet vfleet = fleet.getValue();
					
					for(Car car : vfleet.getCars()){
						os.write(car.getName().getBytes());
						os.write(System.lineSeparator().getBytes());
					}
				}
				catch(Exception e){
					System.err.println(e);
					System.exit(1);
				}
			}
		}.start();
	}
	
	public static void main(String args[]) throws JAXBException, DatatypeConfigurationException, IOException{
		outputFleetXML(assembleFleet(), System.out);
		inputFleetXML(System.out);
		
	}
}