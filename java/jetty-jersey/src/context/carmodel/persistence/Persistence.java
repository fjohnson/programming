package context.carmodel.persistence;

import java.util.HashMap;
import java.util.Map;

import org.example.carmodel.Fleet;

public class Persistence {
	public static Map<String,Fleet> fleets = new HashMap<String,Fleet>();
	static {
		Fleet defaultFleet = new Fleet();
		fleets.put("Default Fleet", defaultFleet);
	}
}
