package ezmount;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class SshMount {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
	private List<String> mountPoints = new ArrayList<String>();
	private String userhome;
	private Path mountdir;
	private StringBuilder lastError;
	
	SshMount() throws IOException{
		userhome = System.getProperty("user.home");
		mountdir = Paths.get(userhome + "/.ezmount");
		
		if(!Files.isDirectory(mountdir)){
			//TODO: Create directory with better default perms.
			Files.createDirectory(mountdir);
		}
	}
	
	/**
	 * Mounts a host a mount point. Does something evil by setting lastError
	 * if an error occured.
	 * @param path not really a path but a ssh string [user@]host:[dir]
	 * @return false if the mount failed under normal conditions. 
	 * @throws IOException in truly exceptional conditions i.e failed to create
	 * the mount point directory, failed to call sshfs, failed to delete the
	 * mount point directory, failed to read failed stdout of sshfs after a 
	 * failed mount attempt. You get the picture.
	 * @throws InterruptedException I hate this exception.
	 */
	boolean mountPath(String path) throws IOException, InterruptedException{
		
		Path target = mountdir.resolve("mount" + (mountPoints.size() + 1));
		boolean mountOK = false;
		
		if(!Files.isDirectory(target)){
			Files.createDirectory(target);
		}
		
		try{
			Process p = Runtime.getRuntime().exec("sshfs " + path + " " + target);
			lastError = new StringBuilder();
			BufferedReader br = new BufferedReader(new InputStreamReader(p.getInputStream()));
			String line;
			while( (line = br.readLine()) != null) lastError.append(line);	
			if (p.waitFor() != 0){
			
				
				return (mountOK = false);
		
			}
			
			return (mountOK = true);
		}
		finally{
			if(!mountOK) Files.delete(target);
		}
		
		
		
	}
	
	 String getLastError(){
		return lastError.toString();
	}

}
