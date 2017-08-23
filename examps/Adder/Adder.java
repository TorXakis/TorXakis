/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
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
public class Adder implements Runnable {
    // Port number on which the current object will listen.
    private int portNr;

    Adder (String portNrStr) {
        this.portNr = Integer.parseInt(portNrStr);
    }

    public void run() {
        startAdder();
    }
            
    public static void main(String[] args) {
        if (args.length == 0)
            System.out.println("Own port number required");
        else {
             // Start adders in parallel, one per-each port number.
            String msg = String.format("Starting %d adders.", args.length);
            System.out.println(msg);
            for (int i = 0; i < args.length; i++) {
                (new Thread (new Adder(args[i]))).start();
            }
        }
    }

    void startAdder() {
        String s, r;
        int sep1, sep2, sep3, x, y;
        String msg = String.format("Starting an adder listening on port %d", portNr);
        System.out.println(msg);
        try {
            // instantiate a socket for accepting a connection
            ServerSocket serverSock = new ServerSocket(portNr);

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
                s = sockIn.readLine();
                if (s != null) {
                    s = s.trim();
                    msg = String.format("Adders on port %d received input: %s", portNr, s);
                    System.out.println(msg);
                    
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
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
