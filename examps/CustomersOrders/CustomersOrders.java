/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/

import java.net.*;
import java.io.*;
import java.util.concurrent.*;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class CustomersOrders {
    private static class Storage {
        final String name;
        String storage;

        Storage(String name, String storage) {
            this.name = name;
            this.storage = storage;
        }
    }

    private static Storage customers = new Storage("Customer", "<Nil_Customer></Nil_Customer>");
    private static Storage orders = new Storage("Order", "<Nil_Order></Nil_Order>");

    private static final String TorXakisMsgOpenTag = "<TorXakisMsg>";
    private static final String TorXakisMsgCloseTag = "</TorXakisMsg>";
    private static boolean changed = true;

    private static synchronized void addItem(String item, Storage storage) {
        storage.storage = "<Cstr_" + storage.name + "><head>" + item.substring(TorXakisMsgOpenTag.length(), item.length() - TorXakisMsgCloseTag.length()) + "</head>" +
                "<tail>" + storage.storage + "</tail></Cstr_" + storage.name + ">";
        changed = true;
    }

    private synchronized String getReport() {
        changed = false;
        String report =
                TorXakisMsgOpenTag +
                        "<Report>" +
                        "<customers>"
                        + customers.storage +
                        "</customers>" +
                        "<orders>"
                        + orders.storage +
                        "</orders>" +
                        "</Report>" +
                        TorXakisMsgCloseTag;
        System.out.println("Report: " + report);
        return report;
    }

    public static void main(String[] args) {
        CustomersOrders co = new CustomersOrders();

        Thread threadCustomer = getInputThread(7890, customers);
        threadCustomer.start();

        Thread threadOrder = getInputThread(7891, orders);
        threadOrder.start();

        Thread threadReport = getOutputThread(7892, "Report", () -> {
            if (co.changed) {
                return co.getReport() + "\n";
            }
            return "";
        });
        threadReport.start();
    }

    private static Thread getInputThread(int port, Storage storage) {
        return new Thread(() -> {
            String input = "";
            try {
                ServerSocket server = new ServerSocket(port);
                System.out.println(storage.name + " thread waiting for tester...");
                Socket inputSocket = server.accept();
                InputStream inputStream = inputSocket.getInputStream();
                BufferedReader socketReader = new BufferedReader(new InputStreamReader(inputStream));
                System.out.println(storage.name + " thread ready");

                input = socketReader.readLine();
                while (input != null) {
                    System.out.println(storage.name + ": " + input);
                    addItem(input, storage);
                    input = socketReader.readLine();
                }
                server.close();
                System.exit(0);
            } catch (IOException e) {
                System.err.println("IOException while reading " + storage.name + ". Last read: " + input);
                e.printStackTrace();
                System.exit(-1);
            }
        });
    }

    private static Thread getOutputThread(int port, String type, Supplier<String> process) {
        return new Thread(() -> {
            try {
                ServerSocket server = new ServerSocket(port);
                System.out.println(type + " thread waiting for tester");
                Socket outputSocket = server.accept();
                OutputStream outputStream = outputSocket.getOutputStream();
                PrintWriter socketWriter = new PrintWriter(new OutputStreamWriter(outputStream));
                System.out.println(type + " thread ready");
                while (true) {
                    String output = process.get();
                    writeIfNotEmpty(type, output, socketWriter);
                    TimeUnit.SECONDS.sleep(25);
                }
            } catch (IOException | InterruptedException e) {
                System.err.println("Exception in OutputThread of " + type);
                e.printStackTrace();
                System.exit(-1);
            }
        });
    }

    private static void writeIfNotEmpty(String type, String output, PrintWriter socketWriter) {
        if (!output.isEmpty()) {
            System.out.println(type + ": " + output);
            socketWriter.print(output);
            socketWriter.flush();
        }
    }
}
