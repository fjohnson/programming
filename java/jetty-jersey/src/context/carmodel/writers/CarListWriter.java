package context.carmodel.writers;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;

import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.MessageBodyWriter;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.example.carmodel.Car;

import context.carmodel.rwresources.ReadWriteResource;
import context.carmodel.schema.generated.carmodelreturnxml.Cargroup;

/**
 * Return a list of cars as XML. Because the returned XML result is a new kind
 * of element (i.e there exists no element that has cars as its children 
 * except for the Fleet element, and that is not what we need to return) a new
 * schema was necessary (context.carmodel.schema.CarModelReturnXml.xsd)
 * that defined the type of XML (and JAXB generated bindings) that could be 
 * returned. 
 * @author fletcher
 *
 */
@Provider
@Produces("application/xml")
public class CarListWriter implements MessageBodyWriter<List<Car>> {

	@Override
	public long getSize(List<Car> arg0, Class<?> arg1, Type arg2,
			Annotation[] arg3, MediaType arg4) {
		return -1;
	}

	@Override
	public boolean isWriteable(Class<?> arg0, Type arg1, Annotation[] arg2,
			MediaType arg3) {
		
		if(arg1 instanceof ParameterizedType && 
		List.class.isAssignableFrom(arg0)){
			ParameterizedType pt = (ParameterizedType) arg1;
			Type[] targs = pt.getActualTypeArguments();
			
			if(targs.length != 1) return false;
			if(targs[0].equals(Car.class)) return true;
		}
		return false;
	}

	@Override
	public void writeTo(List<Car> arg0, Class<?> arg1, Type arg2,
			Annotation[] arg3, MediaType arg4,
			MultivaluedMap<String, Object> arg5, OutputStream arg6)
			throws IOException, WebApplicationException {
		
		Cargroup cg = ReadWriteResource.objectFactory.createCargroup();
		cg.getCar().addAll(arg0);
		JAXBElement<Cargroup> jaxbcg = ReadWriteResource.objectFactory.createMatchingCars(cg);
		try {
			ReadWriteResource.marshaller.marshal(jaxbcg, arg6);
		} catch (JAXBException e) {
			throw ReadWriteResource.marshallError(e);
		}
				
	}

}
