/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.net.*;
import java.io.*;
import java.util.concurrent.*;

public class LuckyPeople {
    private static final byte LUCKYAFTER = 5;

    private static final String SEPARATOR = "@";

    private static final int NROFFIELDS = 5;
    private static final int SEX = 0;
    private static final int FIRSTNAME = 1;
    private static final int LASTNAME = 2;
    private static final int DAYOFBIRTH = 3;
    private static final int MONTHOFBIRTH = 4;

    public static void main(String[] args) {
        try {
            ServerSocket serverSocket = new ServerSocket(7777);

            System.out.println("Waiting for tester");
            Socket sock = serverSocket.accept();

            InputStream inStream = sock.getInputStream();
            BufferedReader socketReader = new BufferedReader(new InputStreamReader(inStream));

            OutputStream outStream = sock.getOutputStream();
            PrintWriter socketWriter = new PrintWriter(new OutputStreamWriter(outStream));

            System.out.println("Tester connected.");
            byte same = 0;
            String last = "";
            while (true) {
                String person = socketReader.readLine();

                String[] fields = person.split(SEPARATOR);
                assert fields.length == NROFFIELDS : "Wrong input : " + person;

                final boolean sameSex = last.equals(fields[SEX]);
                final boolean isLucky = (!sameSex && (same == LUCKYAFTER)) || lucky(fields);
                if (sameSex) {
                    if (same < LUCKYAFTER)
                        same += 1;
                } else {
                    same = 1;
                    last = fields[SEX];
                }
                socketWriter.print((isLucky ? "True" : "False") + "\n");
                socketWriter.flush();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }


    private static boolean lucky(String[] fields) {
        return (fields[FIRSTNAME].charAt(0) == fields[LASTNAME].charAt(0))
                || fields[DAYOFBIRTH].equals(fields[MONTHOFBIRTH]);
    }
}
