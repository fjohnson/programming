package jaxb.example;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

public class CarOutput {

	static void SpitOutCarModelXML() throws JAXBException{
		JAXBContext context = JAXBContext.newInstance(CarModel.class);
		Marshaller m = context.createMarshaller();
	    m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
	    m.marshal( new CarModel(), System.out );
	}

	static void reconstructCarModelFromXML() throws FileNotFoundException, JAXBException{
		InputStream xmlStream = new FileInputStream("/tmp/cm.xml");
		JAXBContext context = JAXBContext.newInstance(CarModel.class);
		Unmarshaller um = context.createUnmarshaller();
		CarModel cm = (CarModel) um.unmarshal(xmlStream);
		System.out.println(cm.gears);
		System.out.println(cm.maxspeed);
		System.out.println(cm.name);
	}
	public static void main(String[] args) throws JAXBException, FileNotFoundException {
		SpitOutCarModelXML();
		reconstructCarModelFromXML();
	}

}
