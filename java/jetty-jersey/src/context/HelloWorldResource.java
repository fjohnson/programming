
package context;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;

import com.sun.jersey.multipart.FormDataMultiPart;


// The Java class will be hosted at the URI path "/helloworld"
@Path("/users/")
public class HelloWorldResource {

    // The Java method will process HTTP GET requests
    @GET 
    // The Java method will produce content identified by the MIME Media
    // type "text/plain"
    @Produces("text/plain")
    @Path("{username}")
    public String getClichedMessage(@PathParam("username") String username) {
        // Return some cliched textual content
        return "Hello Mr." + username;
    }
    
    @PUT
    @Produces("text/plain")
    @Consumes("text/plain")
    public String receivePUT(){
    	
    	return "Put received";
    }
    
    @PUT 
    @Path("/xml")
    @Consumes("application/xml")
    @Produces("application/xml")
    public String receiveXML(Reader r){
    	BufferedReader br = new BufferedReader(r);
    	StringBuilder sb = new StringBuilder();
    	String line = "";
    	try {
			while( (line = br.readLine()) != null){
				sb.append(line);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	sb.append("out xml" + System.lineSeparator());
    	return sb.toString();
    }
    
    @POST
    @Path("/sxml")
    @Produces("application/xml")
    @Consumes("multipart/form-data")
//    public String receiveXML(@FormDataParam("xml") InputStream xml){
    public String receiveXML(FormDataMultiPart input){
    	/*char buf[] = new char[1024];
    	int read;
    	StringBuilder out = new StringBuilder();
    	InputStreamReader xml_reader = new InputStreamReader(xml);
    	try {
			while( (read = xml_reader.read(buf)) != -1){
				out.append(buf, 0, read);
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	return out.toString();*/
    	return "oh what fun.";
    }
    	
    
}