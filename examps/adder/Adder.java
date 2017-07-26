/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/

import java.net.*;
import java.io.*;
import java.util.Random;

/**********************************************************************************************
 Addition: input of Plus/Minus and two integers
 which are added/subtracted.
 Communication via stream-mode socket;
 command line argument:
 <port number for the Server socket used in this process>.
 *********************************************************************************************/
public class Adder {
    public static void main(String[] args) {
        String s, r;
        int sep1, sep2, sep3, x, y;

        if (args.length != 1)
            System.out.println("own port number required");
        else {
            try {
                int portNo = Integer.parseInt(args[0]);

                // instantiate a socket for accepting a connection
                ServerSocket serverSock = new ServerSocket(portNo);

                // wait to accept a connection request
                // then a data socket is created
                Socket sock = serverSock.accept();

                // get an input stream for reading from the data socket
                InputStream inStream = sock.getInputStream();
                // create a BufferedReader object for text line input
                BufferedReader sockIn = new BufferedReader(new InputStreamReader(inStream));

                // get an output stream for writing to the data socket
                OutputStream outStream = sock.getOutputStream();
                // create a PrinterWriter object for character-mode output
                PrintWriter sockOut = new PrintWriter(new OutputStreamWriter(outStream));

                Random random = new Random();
                final int maxSleepTime = 180;

                while (true) {  // read a line from the data stream
                    s = sockIn.readLine().trim();
                    System.out.println(s);
                    sep1 = s.indexOf("(");
                    sep2 = s.indexOf(",");
                    sep3 = s.indexOf(")");
                    x = Integer.parseInt(s.substring(sep1 + 1, sep2).trim());
                    y = Integer.parseInt(s.substring(sep2 + 1, sep3).trim());

                    Thread.sleep(random.nextInt(maxSleepTime));

                    if (s.startsWith("Plus")) {
                        r = " " + (x + y);
                        sockOut.print(r + "\n");
                        sockOut.flush();
                        System.out.println(r);
                    }
                    if (s.startsWith("Minus")) {
                        r = " " + (x - y);
                        // next line inserts error
                        // if (x <= -500 && y >= 500) answer -= 1;
                        // r = " " + answer;
                        sockOut.print(r + "\n");
                        sockOut.flush();
                        System.out.println(r);
                    }
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
}
