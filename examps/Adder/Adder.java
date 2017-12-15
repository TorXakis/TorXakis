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
    private int portNr;

    Adder(String portNrStr) {
        this.portNr = Integer.parseInt(portNrStr);
    }

    public void run() {
        startAdder();
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Own port number required");
            return;
        }
        // Start adders in parallel, one per-each port number.
        String msg = String.format("Starting %d adders.", args.length);
        System.out.println(msg);
        for (String arg : args) {
            (new Thread(new Adder(arg))).start();
        }
    }

    private void startAdder() {
        System.out.println(String.format("Starting an adder listening on port %d", portNr));
        try {
            ServerSocket serverSock = new ServerSocket(portNr);
            Socket sock = serverSock.accept();

            InputStream inStream = sock.getInputStream();
            BufferedReader sockIn = new BufferedReader(new InputStreamReader(inStream));

            OutputStream outStream = sock.getOutputStream();
            PrintWriter sockOut = new PrintWriter(new OutputStreamWriter(outStream));

            final int maxSleepTime = 180;

            while (true) {
                String s = sockIn.readLine();
                if (s == null) {
                    continue;
                }

                processInput(s, sockOut, maxSleepTime);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private void processInput(String s, PrintWriter sockOut, int maxSleepTime) throws InterruptedException {
        s = s.trim();
        System.out.println(String.format("Adders on port %d received input: %s", portNr, s));

        int sep1 = s.indexOf("(");
        int sep2 = s.indexOf(",");
        int sep3 = s.indexOf(")");
        int x = Integer.parseInt(s.substring(sep1 + 1, sep2).trim());
        int y = Integer.parseInt(s.substring(sep2 + 1, sep3).trim());

        Random random = new Random();
        Thread.sleep(random.nextInt(maxSleepTime));

        if (s.startsWith("Plus")) {
            printResult(sockOut, x + y);
        } else if (s.startsWith("Minus")) {
            printResult(sockOut, x - y);
        }
    }

    private void printResult(PrintWriter sockOut, int result) {
        sockOut.print(" " + Integer.toString(result) + "\n");
        sockOut.flush();
        System.out.println(result);
    }
}
