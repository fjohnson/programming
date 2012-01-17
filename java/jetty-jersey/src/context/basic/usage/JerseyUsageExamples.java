
package context.basic.usage;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Cookie;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.NewCookie;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXSource;

import com.sun.jersey.multipart.FormDataMultiPart;

/**
 * Examples of how to use Jersey.
 * @author fletcher
 *
 */

@Path("/users/")
public class JerseyUsageExamples {

   
	/**
	 * Example illustrating the use of url parameters (not query parameters,
	 * but url parameters)
	 * @param username
	 * @return
	 */
    @GET 
    @Produces("text/plain")
    @Path("{username}")
    public String getClichedMessage(@PathParam("username") String username) {
        // Return some cliched textual content
        return "Hello Mr." + username;
    }
   
   
    /**
     * Example illustrating the consumption of headers and cookies
     * 
     */
    @GET
    @Path("headers")
    @Produces("text/plain")
    public String getHeadersAndCookies(@Context HttpHeaders hh){
    	MediaType mediaType = hh.getMediaType();
    	Map<String, Cookie> cookies = hh.getCookies();
    	MultivaluedMap<String, String> headers = hh.getRequestHeaders();
    	StringBuilder sb = new StringBuilder();
    	
    	//This will cause a severe exception
    	//StringBuilder sb = new StringBuilder("Media type: " + mediaType.getType());
    	
    	sb.append("Cookies" + System.lineSeparator());
    	for(Entry<String, Cookie> e : cookies.entrySet()){
    		sb.append(e.getKey());
    		sb.append(":");
    		sb.append(e.getValue());
    		sb.append(System.lineSeparator());
    	}
    	
    	sb.append("Headers" + System.lineSeparator());
    	for(Entry<String, List<String>> e : headers.entrySet()){
    		sb.append(e.getKey());
    		
    		for(String s : e.getValue()){
    			sb.append(":");
    			sb.append(e.getValue());
    		}
    		sb.append(System.lineSeparator());
    	}
    	return sb.toString();
    }
   
    
    /**
     * Example of consuming xml and repeating the consumed output
     * See "Table 19-3 Types Supported for HTTP Request and Response Entity Bodies"
     * {@link http://docs.oracle.com/javaee/6/tutorial/doc/gilik.html} on why I knew
     * that the input parameter could be of type "Reader".
     * @param r
     * @return
     */
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

    /**
     * Example of consumption of multipart/form-data input.
     * Need to use the jetty-multipart.jar contrib jar file for this as  
     * standard jetty does not include this.
     * @param input
     * @return
     */
    @POST
    @Path("/sxml")
    @Produces("application/xml")
    @Consumes("multipart/form-data")

    public String receiveXML(FormDataMultiPart input){
    	return "Not yet implemented.";
    }
    	
    /**
     * Example of returning a customized header response.
     * Curl returns:
     * < HTTP/1.1 202 Accepted
       < Set-Cookie: First=Last;Version=1
       < Content-Type: text/plain
       < Custom: Header
       < Content-Language: en-CA
       < Content-Length: 0
       < Server: Jetty(7.5.4.v20111024)

     */
    @GET
    @Path("/customresponse")
    public Response getCustomResponse(){
    	ResponseBuilder rb = Response.noContent();
    	rb.cookie(new NewCookie("First","Last"));
    	rb.status(Response.Status.ACCEPTED);
    	rb.type(MediaType.TEXT_PLAIN_TYPE);
    	rb.header("Custom","Header");
    	rb.language(Locale.CANADA);
    	return rb.build();
    	
    }
    
    /**
     * Example illustrating a custom header response and body containing
     * the contents of a file.
     * The types in "table 19" list the values that can be used as
     * parameters or return values without having to write your own custom
     * MessageBodyWriter/Reader. If you need to output or read in types 
     * other than these you will need to use the "provider" annotation with
     * an implementation of a MessageBodyWriter/Reader interface. 
     * {@link http://docs.oracle.com/javaee/6/tutorial/doc/gilik.html#gkccg}
     * {@link http://christopherhunt-software.blogspot.com/2010/08/messagebodywriter-iswriteable-method.html}
     * @return
     * @throws FileNotFoundException
     */
    @GET
    @Path("/crwithbody")
    public Response getCustomResponseAndBody() throws FileNotFoundException{
    	String fileloc = "/home/fletcher/Desktop/pmri_testlog";
    	FileInputStream fs = new FileInputStream(fileloc);
    	ResponseBuilder rb = Response.ok();
    	
    	rb.entity(fs)
    	.cookie(new NewCookie("Example","Fileoutput"))
    	.header("Content-Type", "text/plain;charset=utf-8");
    	
    	return rb.build();
    }
    
    /**
     * This example will generate an exception because there is no 
     * MessageBodyWriter that knows how to write out a double as
     * an http body. Also there are no MessageBodyReaders that know
     * how to turn incoming data into the Date and Number parameters. 
     * @return
     */
    @POST
    @Path("/exception")
    public double getNoMessageBodyWriterException(Date a, Number b){
    	return 4.0;
    }
    
    /**
     * Illustrative example of how Jersey extends the available 
     * MessageBodyWriters/Readers to support some extra XML types 
     */
    @Path("/mbxml/1")
    @POST
    public DOMSource get1(DOMSource domSource){
    	return domSource;
    }
    
    @Path("/mbxml/2")
    @POST
    public SAXSource get2(SAXSource saxSource){
    	return saxSource;
    }
}