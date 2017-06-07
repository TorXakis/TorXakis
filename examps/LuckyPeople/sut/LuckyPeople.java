/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/

import java.net.*;
import java.io.*;
import java.util.concurrent.*;

public class LuckyPeople
{
	private static final String SEPARATOR = "@";
	private static final int NROFFIELDS		= 5;
	private static final int SEX			= 0;
	private static final int FIRSTNAME		= 1;
	private static final int LASTNAME		= 2;
	private static final int DAYOFBIRTH		= 3;
	private static final int MONTHOFBIRTH	= 4;
	
	
    public static void main(String[] args)
    {  
        try
        {
            // instantiate a socket for accepting a connection
            ServerSocket servsock = new ServerSocket(777);

            // wait to accept a connection request and a data socket is returned
            Socket sock = servsock.accept();

            // get an input stream for reading from the data socket
            InputStream inStream = sock.getInputStream();

            // create a BufferedReader object for text line input
            BufferedReader sockin = new BufferedReader(new InputStreamReader(inStream));

            // get an output stream for writing to the data socket
            OutputStream outStream = sock.getOutputStream();

            // create a PrinterWriter object for character-mode output
            PrintWriter sockout = new PrintWriter(new OutputStreamWriter(outStream));

            int males = 0;
            int females = 0;
            while (true)
            {
	            // read a line from the data stream: the stimulus
	            String person = sockin.readLine();
	            
	            String[] fields = person.split(SEPARATOR);
	            assert fields.length == NROFFIELDS : "Wrong input : "+person;
	            
                boolean retval;
                if ("Male".equals(fields[SEX]))
	            {
                    retval = (females >= 5) || lucky(fields);
                    males += 1;
                    females = 0;
	            }
	            else
	            {
                    retval = (males >= 5) || lucky(fields);
                    males = 0;
                    females += 1;
	            }
	            // send a line to the data stream: the response
	            if (retval)
	            	sockout.print("True\n");
	            else
	            	sockout.print("False\n");
	            sockout.flush();
            }
        }
        catch (Exception ex) { ex.printStackTrace(); }
    }


	private static boolean lucky(String[] fields) {
		return 	(fields[FIRSTNAME].charAt(0) == fields[LASTNAME].charAt(0)) 
				|| fields[DAYOFBIRTH].equals(fields[MONTHOFBIRTH]);
	}
}