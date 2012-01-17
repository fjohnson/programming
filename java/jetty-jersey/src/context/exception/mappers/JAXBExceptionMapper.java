package context.exception.mappers;

//http://fusesource.com/docs/esb/4.2/rest/RESTExceptionMapper.html
import javax.ws.rs.core.Response;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;
import javax.xml.bind.JAXBException;

/**
 * This class is not used yet, but it is here as an example of how
 * Jersey can be configured to return custom responses upon receiving
 * certain exceptions.
 * 
 * @author fletcher
 *
 */
@Provider
public class JAXBExceptionMapper implements ExceptionMapper<JAXBException>{

	@Override
	public Response toResponse(JAXBException arg0) {
		return Response.serverError().entity(arg0.getMessage()).build();
	}

}
