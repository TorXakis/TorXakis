/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.net.*;
import java.io.*;
import java.util.concurrent.*;

public class LuckyPeopleOverflow
{
    private static final byte LUCKYAFTER    = 5;

    private static final String SEPARATOR   = "@";

    private static final int NROFFIELDS     = 5;
    private static final int SEX            = 0;
    private static final int FIRSTNAME      = 1;
    private static final int LASTNAME       = 2;
    private static final int DAYOFBIRTH     = 3;
    private static final int MONTHOFBIRTH   = 4;

    public static void main(String[] args)
    {
        try
        {
            // instantiate a socket for accepting a connection
            ServerSocket servsock = new ServerSocket(7777);

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

            byte same = 0;
            String last = "";
            while (true)
            {
                String person = sockin.readLine();
                
                String[] fields = person.split(SEPARATOR);
                assert fields.length == NROFFIELDS : "Wrong input : "+person;

                final boolean sameSex = last.equals(fields[SEX]);
                final boolean retval = (!sameSex && (same >= LUCKYAFTER)) || lucky(fields);
                if (sameSex)
                {
                    same += 1;
                }
                else
                {
                    same = 1;
                    last = fields[SEX];
                }
                // send a line to the data stream: is lucky?
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
        return     (fields[FIRSTNAME].charAt(0) == fields[LASTNAME].charAt(0)) 
                || fields[DAYOFBIRTH].equals(fields[MONTHOFBIRTH]);
    }
}