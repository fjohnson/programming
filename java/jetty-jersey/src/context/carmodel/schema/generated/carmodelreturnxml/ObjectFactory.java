//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.4 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2012.01.17 at 05:22:57 PM EST 
//


package context.carmodel.schema.generated.carmodelreturnxml;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;
import org.example.carmodel.Car;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the org.example.carmodelreturnxml package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _Car_QNAME = new QName("http://www.example.org/CarModelReturnXML/", "car");
    private final static QName _MatchingCars_QNAME = new QName("http://www.example.org/CarModelReturnXML/", "matchingCars");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: org.example.carmodelreturnxml
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link Cargroup }
     * 
     */
    public Cargroup createCargroup() {
        return new Cargroup();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Car }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.example.org/CarModelReturnXML/", name = "car")
    public JAXBElement<Car> createCar(Car value) {
        return new JAXBElement<Car>(_Car_QNAME, Car.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Cargroup }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://www.example.org/CarModelReturnXML/", name = "matchingCars")
    public JAXBElement<Cargroup> createMatchingCars(Cargroup value) {
        return new JAXBElement<Cargroup>(_MatchingCars_QNAME, Cargroup.class, null, value);
    }

}
