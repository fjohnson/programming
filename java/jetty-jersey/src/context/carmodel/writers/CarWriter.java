package context.carmodel.writers;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.example.carmodel.Car;
import org.example.carmodel.ObjectFactory;

import context.carmodel.rwresources.ReadWriteResource;

@Provider
@Produces("application/xml")
public class CarWriter implements MessageBodyWriter<Car> {

	@Override
	public long getSize(Car arg0, Class<?> arg1, Type arg2, Annotation[] arg3,
			MediaType arg4) {
		return -1;
	}

	@Override
	public boolean isWriteable(Class<?> arg0, Type arg1, Annotation[] arg2,
			MediaType arg3) {
		return arg1.equals(Car.class) ? true : false;
	}

	@Override
	public void writeTo(Car arg0, Class<?> arg1, Type arg2, Annotation[] arg3,
			MediaType arg4, MultivaluedMap<String, Object> arg5,
			OutputStream arg6) throws IOException, WebApplicationException {
		
		try{
			ReadWriteResource.marshaller.marshal(new ObjectFactory().createCar(arg0),arg6);
		}catch(JAXBException e){
			throw ReadWriteResource.marshallError(e);
		}
	}

}
