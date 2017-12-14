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
    private static final String TorXakisMsgOpenTag = "<TorXakisMsg>";
    private static final String TorXakisMsgCloseTag = "</TorXakisMsg>";
    private String customers = "<Nil_Customer></Nil_Customer>";
    private String orders = "<Nil_Order></Nil_Order>";
    private boolean changed = true;

    private synchronized void addCustomer(String customer) {
        customers = "<Cstr_Customer><head>" + customer.substring(TorXakisMsgOpenTag.length(), customer.length() - TorXakisMsgCloseTag.length()) + "</head>" +
                "<tail>" + customers + "</tail></Cstr_Customer>";
        changed = true;
    }

    private synchronized void addOrder(String order) {
        orders = "<Cstr_Order><head>" + order.substring(TorXakisMsgOpenTag.length(), order.length() - TorXakisMsgCloseTag.length()) + "</head>" +
                "<tail>" + orders + "</tail></Cstr_Order>";
        changed = true;
    }

    private synchronized String getReport() {
        changed = false;
        String report =
                TorXakisMsgOpenTag +
                        "<Report>" +
                        "<customers>"
                        + customers +
                        "</customers>" +
                        "<orders>"
                        + orders +
                        "</orders>" +
                        "</Report>" +
                        TorXakisMsgCloseTag;
        System.out.println("Report: " + report);
        return report;
    }

    public static void main(String[] args) {
        CustomersOrders co = new CustomersOrders();

        Thread threadCustomer = getInputThread(7890, "Customer", co::addCustomer);
        threadCustomer.start();

        Thread threadOrder = getInputThread(7891, "Order", co::addOrder);
        threadOrder.start();

        Thread threadReport = getOutputThread(7892, "Report", () -> {
            if (co.changed) {
                return co.getReport() + "\n";
            }
            return "";
        });
        threadReport.start();
    }

    private static Thread getInputThread(int port, String type, Consumer<String> process) {
        return new Thread(() -> {
            String input = "";
            try {
                ServerSocket server = new ServerSocket(port);
                System.out.println(type + " thread waiting for tester...");
                Socket inputSocket = server.accept();
                InputStream inputStream = inputSocket.getInputStream();
                BufferedReader socketReader = new BufferedReader(new InputStreamReader(inputStream));
                System.out.println(type + " thread ready");
                input = socketReader.readLine();
                while (input != null) {
                    System.out.println(type + ": " + input);
                    process.accept(input);
                    input = socketReader.readLine();
                }
                server.close();
                System.exit(0);
            } catch (IOException e) {
                System.err.println("IOException while reading " + type + ". Last read: " + input);
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
