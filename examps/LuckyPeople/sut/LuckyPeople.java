/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.net.*;
import java.io.*;

public class LuckyPeople {
    private static final String SEPARATOR = "@";

    private static final int NR_OF_FIELDS = 5;

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

            LuckState state = new LuckState();
            while (true) {
                String person = socketReader.readLine();
                System.out.println("Input: " + person);
                String[] fields = person.split(SEPARATOR);
                assert fields.length == NR_OF_FIELDS : "Wrong input : " + person;

                state = new LuckState(fields, state);
                socketWriter.print((state.isLucky() ? "True" : "False") + "\n");
                socketWriter.flush();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
                // send a line to the data stream: is lucky?
class LuckState {
    private static final byte LUCKY_AFTER = 5;
    private static final int SEX = 0;
    private static final int FIRST_NAME = 1;
    private static final int LAST_NAME = 2;
    private static final int DAY_OF_BIRTH = 3;
    private static final int MONTH_OF_BIRTH = 4;

    private byte sameSexCount = 0;
    private String sex = "";
    private boolean lucky = false;

    public boolean isLucky(){
        return lucky;
    }

    LuckState() {
    }

    LuckState(String[] personFields, LuckState oldState) {
        sex = personFields[SEX];
        boolean sameSex = oldState.sex.equals(sex);
        lucky = (!sameSex && oldState.sameSexCount == LUCKY_AFTER) || checkLucky(personFields);
        sameSexCount = sameSex ? (byte) Math.min(oldState.sameSexCount + 1, LUCKY_AFTER) : 1;
        System.out.println("LuckState: sex=" + sex + " sameSexCount=" + sameSexCount + " lucky=" + lucky);
    }

    private static boolean checkLucky(String[] fields) {
        return (fields[FIRST_NAME].charAt(0) == fields[LAST_NAME].charAt(0))
                || fields[DAY_OF_BIRTH].equals(fields[MONTH_OF_BIRTH]);
    }
}
