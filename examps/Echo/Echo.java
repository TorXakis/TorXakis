/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.net.*;
import java.io.*;
import java.util.*;

class Reader implements Runnable {
    private Socket socket;
    private Vector<String> vector;

    Reader(Socket socket, Vector<String> vector) {
        this.socket = socket;
        this.vector = vector;
    }

    @Override
    public void run() {
        try {
            InputStream inStream = socket.getInputStream();
            BufferedReader socketReader = new BufferedReader(new InputStreamReader(inStream));
            
            while (true) {
                //read
                String value = socketReader.readLine();
                System.out.println("Input: " + value);
                vector.add (value);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(-1);
        }
    }
}

public class Echo {
    public static void main(String[] args) {
        try {
            ServerSocket serverSocket = new ServerSocket(9999);

            System.out.println("Waiting for tester");
            Socket socket = serverSocket.accept();

            Vector<String> vector = new Vector<String>();
            Thread reader = new Thread (new Reader (socket, vector));
            reader.start();

            OutputStream outStream = socket.getOutputStream();
            PrintWriter socketWriter = new PrintWriter(new OutputStreamWriter(outStream));

            System.out.println("Tester connected.");

            while (true) {
                Thread.sleep (1000);
                if (vector.size() > 0)
                {
                    //write
                    String value = vector.remove(0);
                    socketWriter.print(value + "\n");
                    socketWriter.flush();
                }
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            System.exit(-1);
        }
    }
}
