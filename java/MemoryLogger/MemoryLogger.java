package MemoryLogger;

import java.io.IOException;
import java.util.logging.ConsoleHandler;
import java.util.logging.Formatter;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

public class MemoryLogger {
	private static Logger memlogger = Logger.getLogger(MemoryLogger.class.getSimpleName());
	private static final int mb = 1024*1024;
	
	private long lastmemUsage;
	
	static {
		Formatter plain = new Formatter() {

			@Override
			public String format(LogRecord record) {
			
				return String.format("%s:%s %s%n",
						record.getLoggerName(),
						record.getLevel(),
						record.getMessage());
				
			}
			
		};
		
		//remove any default configuration.
		LogManager.getLogManager().reset();
		
		ConsoleHandler chandler = new ConsoleHandler();
		chandler.setFormatter(plain);
		memlogger.addHandler(chandler);
	}
		
	MemoryLogger(){
		Runtime runtime = Runtime.getRuntime();
		lastmemUsage = (runtime.totalMemory() - runtime.freeMemory()) / (mb);
		if (lastmemUsage == 0) lastmemUsage = 1;
	}
	
	 void triggerMemStats(int threshold,String message){
		Runtime runtime = Runtime.getRuntime();
		
		long currentusage = (runtime.totalMemory() - runtime.freeMemory()) / mb;
		if (currentusage == 0) currentusage = 1;
		
		long change = ((currentusage - lastmemUsage) / lastmemUsage) * 100;
		
		if ( change > threshold){
			memlogger.info(String.format("Threshold reached (%d)",threshold));
			memlogger.info(String.format("Last: %d current %d, percent %d",
							lastmemUsage,currentusage,change));
			getMemStats("Memstats triggered by increase of " + change + "%"+
					" - " + message);
			lastmemUsage = currentusage;
		}
	}
	
	void getMemStats(String message){
		//shamelessly taken from 
		//http://viralpatel.net/blogs/2009/05/getting-jvm-heap-size-used-memory-total-memory-using-java-runtime.html
				
		memlogger.info("Memstats - " + message);
		//Getting the runtime reference from system
		Runtime runtime = Runtime.getRuntime();

		//Print used memory
		long currentusage = (runtime.totalMemory() - runtime.freeMemory()) / mb;
		memlogger.info("Used Memory:" + currentusage);

		//Print free memory
		memlogger.info("Free Memory:"
				+ runtime.freeMemory() / mb);

		//Print total available memory
		memlogger.info("Total Memory:" + runtime.totalMemory() / mb);

		//Print Maximum available memory
		
		memlogger.info("Max Memory:" + runtime.maxMemory() / mb);
		memlogger.info(System.lineSeparator());
	}	
	/**
	 * @param args
	 * @throws IOException 
	 * @throws XnatGetSessionException 
	 */
	public static void main(String[] args) throws IOException{
		MemoryLogger a = new MemoryLogger();
	}

}
