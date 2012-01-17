package context.carmodel.readers;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Type;

import javax.ws.rs.Consumes;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.ws.rs.ext.MessageBodyReader;
import javax.ws.rs.ext.Provider;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.example.carmodel.Car;
import org.xml.sax.SAXException;

import context.carmodel.rwresources.ReadWriteResource;

/**
 * A MessageBodyReader that is used for parsing an XML Car description.
 * This is needed so that arguments of type 'Car' can be constructed
 * out of incoming byte streams and returned for use.
 * i.e a function such as addNewCar in CarModelRootResource will call
 * upon this reader to convert the incoming XML data from the client
 * into a suitable Car object.
 * 
 * In particular, this reader also validates the incoming XML against
 * a schema.
 * @author fletcher
 *
 */

@Provider
@Consumes("application/xml")
public class ValidatingCarReader implements MessageBodyReader<Car> {
	public boolean isReadable(Class<?> arg0, Type arg1, Annotation[] arg2, MediaType arg3) {
		return arg1.equals(Car.class) ? true : false;
	}
	
	@Override
	public Car readFrom(Class<Car> arg0, Type arg1, Annotation[] arg2,
			MediaType arg3, MultivaluedMap<String, String> arg4,
			InputStream arg5) throws IOException, WebApplicationException {
		
		try{
			Object result;
			result = ReadWriteResource.unmarshaller.unmarshal(arg5);
			
			return ((JAXBElement<Car>) result).getValue();
		}catch(JAXBException e){
			throw ReadWriteResource.unmarshallError(e);
		}
	}
}
