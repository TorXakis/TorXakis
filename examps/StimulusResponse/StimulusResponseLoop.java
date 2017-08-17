/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

/**********************************************************************************************
 Communication via stream-mode socket;
 *********************************************************************************************/
import java.net.*;
import java.io.*;
import java.util.concurrent.*;

public class StimulusResponseLoop
{
    public static void main(String[] args)
    {  
        try
        {
            // instantiate a socket for accepting a connection
            ServerSocket servsock = new ServerSocket(7890);

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

            // read a line from the data stream: the stimulus

            while (true)
            {
                sockin.readLine();
                // send a line to the data stream: the response
                sockout.print("\n");
                sockout.flush();
            }
        }
        catch (Exception ex) { ex.printStackTrace(); }
    }
}