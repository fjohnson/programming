package org.example.carmodel.creation;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.ValidationEvent;
import javax.xml.bind.ValidationEventHandler;
import javax.xml.bind.ValidationEventLocator;
import javax.xml.bind.util.ValidationEventCollector;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.example.carmodel.Car;
import org.example.carmodel.Fleet;
import org.example.carmodel.ObjectFactory;
import org.example.carmodel.Owner;
import org.xml.sax.SAXException;

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
		o2.setFirstname("Owen");
		o2.setLastname("Pat");
		o2.setHeight("5'8");
		o2.setSex("male");
		o2.setWeight("168");
		
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
	
	/**
	 * Generate a java schema object based on a xsd file at location.
	 * It is assumed the xsd uses the W3C schema definition.
	 * @param location Filepath to xsd file
	 * @return
	 * @throws SAXException
	 */
	static Schema getCarModelSchema(String location) throws SAXException{
		
		SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		Schema carmodelSchema = sf.newSchema(new File(location));
		return carmodelSchema;
	}
	
	/**
	 * This is a custom ValidationEventHandler. The default ValidationEventCollector stops
	 * reporting errors when it first encounters one. This VEH spits them all out.
	 */
	private static class ValidationEventAllCollector implements ValidationEventHandler{
		private List<ValidationEvent> velist = new ArrayList<ValidationEvent>();
		
		public boolean handleEvent(ValidationEvent e){
			velist.add(e);
			if(e.getSeverity() == ValidationEvent.FATAL_ERROR){
				/*Failing to return false from the handleEvent method after encountering 
				 * a fatal error is undefined by the specification and may
				 *result in unexpected behavior.
				 */
				//So what, for practice sake lets do it anyways otherwise all errors
				//are not being output.
				return true;
			}
			return true;
		}
		public List<ValidationEvent> getEvents(){
			return velist;
		}
	}
	
	/**
	 * Example of validating a schema and reporting all errors found.
	 * @param sch
	 * @param xml
	 * @return
	 * @throws Throwable
	 */
	static Object validateXMLAndGetTree(Schema sch, File xml) throws Throwable{
		JAXBContext context = JAXBContext.newInstance( "org.example.carmodel" );
		Unmarshaller um = context.createUnmarshaller();
		ValidationEventAllCollector vec = new ValidationEventAllCollector();
		
		um.setSchema(sch);
		um.setEventHandler(vec);
		
		Object tree = null;
		try{um.unmarshal(xml);}
		catch(Throwable t){;}
		
		Throwable lastEx = null; //Throw the last EX is there is one.
		for(ValidationEvent ve : vec.getEvents()){
			ValidationEventLocator vel = ve.getLocator();
			
			String errmessage = ve.getMessage();
			System.out.format("Err:%s Line:%d Column:%d %n", 
					errmessage, vel.getLineNumber(), vel.getColumnNumber());
			lastEx = ve.getLinkedException();
		}
		//if (lastEx != null) throw lastEx; 
		return tree;
	}
	
	
	/**
	 * This example shows how a schema is validated against an error in an xml file.
	 * @throws Throwable 
	 */
	static void generateValidationError() throws Throwable{
		String schemaLocation;
		schemaLocation = "/home/fletcher/git/programming/java/jaxb-example/src/org/example/carmodel/schema/CarModel.xsd";
		String fileLocation = "/tmp/badxml.xml";
		validateXMLAndGetTree(getCarModelSchema(schemaLocation), new File(fileLocation));
	}
	
	public static void main(String args[]) throws Throwable{
		outputFleetXML(assembleFleet(), System.out);
		//inputFleetXML(System.out);
		generateValidationError();
	}
}

