/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.net.*;
import java.io.*;

public class QueueServer {
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("own port number required");
            return;
        }
        try {
            int portNo = Integer.parseInt(args[0]);

            ServerSocket serverSocket = new ServerSocket(portNo);
            System.out.println("Waiting for tester");
            Socket sock = serverSocket.accept();

            InputStream inStream = sock.getInputStream();
            BufferedReader socketReader =
                    new BufferedReader(new InputStreamReader(inStream));

            OutputStream outStream = sock.getOutputStream();
            PrintWriter socketWriter =
                    new PrintWriter(new OutputStreamWriter(outStream));

            System.out.println("Tester connected.");

            String s;
            Queue q = new Queue();
            q.show();

            while (true) {
                s = socketReader.readLine().trim();
                processInput(s, q, socketWriter);
                socketWriter.flush();
                q.show();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private static void processInput(String s, Queue q, PrintWriter socketWriter) {
        if (s.startsWith("Enq")) {
            int sep1 = s.indexOf("(");
            int sep2 = s.indexOf(")");
            int x = Integer.parseInt(s.substring(sep1 + 1, sep2).trim());
            q.add(x);
        } else if (s.startsWith("Deq")) {
            if (!q.empty()) {
                socketWriter.print(q.take() + "\n");
            }
        }
    }
}

class Queue {
    private Cell first = null;
    private Cell last = null;

    public void add(int x) {
        Cell cell = new Cell();
        cell.data = x;
        cell.rest = null;
        if (empty()) {
            first = cell;
        } else {
            last.rest = cell;
        }
        last = cell;
    }

    public boolean empty() {
        return (first == null);
    }

    public int take() {
        Cell cell = first;
        first = cell.rest;
        return (cell.data);
    }

    public void show() {
        System.out.print("[ ");
        Cell cell = first;
        while (cell != null) {
            System.out.print(cell.data);
            cell = cell.rest;
            if (cell != null) {
                System.out.print(", ");
            }
        }
        System.out.println(" ]");
    }
}

class Cell {
    public int data;
    public Cell rest;
}
