package context.carmodel.rwresources;

import java.io.File;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.SAXException;

import context.carmodel.schema.generated.carmodelreturnxml.ObjectFactory;

/**
 * This class defines reusable variables/resources/items/whatever
 * for MessageBody{Readers,Writers}. Now, I have set up an unmarshaller
 * and a marshaller that use different schemas (and hence context) but there
 * is a reason behind this madness. It is because usually you will not be only
 * returning the elements that you read in but entirely new elements, elements
 * which may or may not be composed of the original elements you have read in.
 *
 * Using JAXB, if you want to return XML you need to draw up a new XSD and then
 * create the bindings, fill up the bindings and then marshall the output to XML
 * and this is why it was necessary to define a new XSD.
 * 
 * Its also conceivable that you might have two marshallers, one that uses the
 * unmarshaller's context to return elements already defined and another that 
 * uses a new XSD and generated context to generate new XML elements but this 
 * is probably more complex than just redefining the elements you need to 
 * return in the new XSD file. 
 * @author fletcher
 *
 */
public class ReadWriteResource {
	
	public static final Unmarshaller unmarshaller;
	public static final Marshaller marshaller;
	//Necessary for generating JAXBElement objects for the Marshaller.
	public static final ObjectFactory objectFactory = new ObjectFactory();
		
	static {
		
		SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
		String location = "../jaxb-example/src/org/example/carmodel/schema/CarModel.xsd";
		String rvslocation = "./src/context/carmodel/schema/CarModelReturnXML.xsd";
		try{
			
			JAXBContext jaxbContext = JAXBContext.newInstance("org.example.carmodel");
			Schema carmodelSchema = sf.newSchema(new File(location));
			unmarshaller = jaxbContext.createUnmarshaller();
			unmarshaller.setSchema(carmodelSchema);
			
			JAXBContext retvalContext = JAXBContext.newInstance(
					"context.carmodel.schema.generated.carmodelreturnxml");
			Schema returnValuesSchema = sf.newSchema(new File(rvslocation));
			marshaller = retvalContext.createMarshaller();
			marshaller.setSchema(returnValuesSchema);
			marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
						
		} catch (SAXException e) {
			throw new RuntimeException(e);
		} catch (JAXBException e){
			throw new RuntimeException(e);
		}
	}
	
	public static WebApplicationException marshallError(JAXBException e){
		String body = String.format(genericErrMsg, "Marshall Exception", e.toString());
		return new WebApplicationException(Response.serverError().entity(body).build());
	}
	
	public static WebApplicationException unmarshallError(JAXBException e){
		String body = String.format(genericErrMsg, "Unmarshall Exception", e.toString());
		return new WebApplicationException(Response.serverError().entity(body).build());
	}
	
	private static String genericErrMsg =  
	"<html>" +
	"<head>" +
	"<meta http-equiv=\"Content-Type\" content=\"text/html;charset=ISO-8859-1\"/>" +
	"<title>Error 500 Internal Server Error - %s</title>" +
	"</head>" +
	"<body>" +
	"<h2>HTTP ERROR: 500</h2>" +
	"<pre>%s</pre>" +
	"</body>" +
	"</html>";
	
}
