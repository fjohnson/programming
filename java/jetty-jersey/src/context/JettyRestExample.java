package context;

import org.eclipse.jetty.http.ssl.SslContextFactory;
import org.eclipse.jetty.server.Connector;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.server.ssl.SslSelectChannelConnector;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.spi.container.servlet.ServletContainer;

public class JettyRestExample {

	private static void addSSLConnector(Server server){
		SslContextFactory sslcfact = new SslContextFactory("/tmp/keystore");
        String pass = "OBF:19iy19j019j219j419j619j8";
        //pass = "MD5:e10adc3949ba59abbe56e057f20f883e"; Doesn't work.
        sslcfact.setKeyStorePassword(pass);
        sslcfact.setKeyManagerPassword(pass);
        
        Connector sslConnector = new SslSelectChannelConnector(sslcfact);
        sslConnector.setPort(8443);
        server.addConnector(sslConnector);
	}
	
	private static void removeConnectors(Server server){
		for(Connector c : server.getConnectors()) server.removeConnector(c);
	}
	
	public static void main(String[] args) throws Exception
    {
        Server server = new Server(8080);
        removeConnectors(server); //remove default connector
        addSSLConnector(server); //add ssl connector
        
        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/wruz");
        server.setHandler(context);
 
        context.addServlet(new ServletHolder(
        		new ServletContainer(
        				new PackagesResourceConfig("context"))),"/frog/*");
        
        server.start();
        server.join();
    }
}
