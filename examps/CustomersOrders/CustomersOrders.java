/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/

import java.net.*;
import java.io.*;
import java.util.concurrent.*;

public class CustomersOrders
{
    static final String TorXakisMsgOpenTag = "<TorXakisMsg>";
    static final String TorXakisMsgCloseTag = "</TorXakisMsg>";
    private String customers = "<Nil_Customer></Nil_Customer>";
    private String orders = "<Nil_Order></Nil_Order>";
    private boolean changed = true;
    
    private synchronized void addCustomer(String customer)
    {
        customers = "<Cstr_Customer><head>" + customer.substring( TorXakisMsgOpenTag.length() , customer.length() - TorXakisMsgCloseTag.length() ) + "</head>" + 
                                   "<tail>" + customers + "</tail></Cstr_Customer>";
        changed = true;
    }
    
    private synchronized void addOrder(String order)
    {
        orders = "<Cstr_Order><head>" + order.substring( TorXakisMsgOpenTag.length() , order.length() - TorXakisMsgCloseTag.length() ) + "</head>" +
                             "<tail>" + orders + "</tail></Cstr_Order>";
        changed = true;
    }
    
    private synchronized String getReport()
    {
        changed = false;
        String report = // "<?xml version=\"1.0\" encoding=\"utf-8\"?>" +
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
        
    public static void main(String[] args)
    {  
        CustomersOrders co = new CustomersOrders();
        
        Thread threadCustomer = new Thread(new Runnable() {
            @Override
            public void run() {
                ServerSocket servsockCustomer;
                try{
                    // instantiate a socket for accepting a connection
                    servsockCustomer = new ServerSocket(7890);

                    // wait to accept a connection request and a data socket is returned
                    Socket sockCustomer = servsockCustomer.accept();

                    // get an input stream for reading from the data socket
                    InputStream inStreamCustomer = sockCustomer.getInputStream();

                    // create a BufferedReader object for text line input
                    BufferedReader sockinCustomer = new BufferedReader(new InputStreamReader(inStreamCustomer));

                    // read a line from the data stream: the stimulus
                    String customer = sockinCustomer.readLine();
                    while(customer != null) //read null when socket is closed
                    {
                        System.out.println("Customer: " + customer);
                        //update data structure
                        co.addCustomer(customer);
                        // read a line from the data stream: the stimulus
                        customer = sockinCustomer.readLine();
                    }
                    System.exit(0);
                }
                catch (IOException e)
                {
                    System.err.println("IOException Customer");
                    e.printStackTrace();
                    System.exit(-1);
                }
            }
        });
        threadCustomer.start();

        Thread threadOrder = new Thread(new Runnable() {
            @Override
            public void run() {
                ServerSocket servsockOrder;
                try{
                    // instantiate a socket for accepting a connection
                    servsockOrder = new ServerSocket(7891);

                    // wait to accept a connection request and a data socket is returned
                    Socket sockOrder = servsockOrder.accept();

                    // get an input stream for reading from the data socket
                    InputStream inStreamOrder = sockOrder.getInputStream();

                    // create a BufferedReader object for text line input
                    BufferedReader sockinOrder = new BufferedReader(new InputStreamReader(inStreamOrder));

                    String order = sockinOrder.readLine();
                    while(order != null)    //read null when socket is closed
                    {
                        System.out.println("Order: " + order);
                        //update data structure
                        co.addOrder(order);
                        // read a line from the data stream: the stimulus
                        order = sockinOrder.readLine();
                    }
                    System.exit(0);
                }
                catch (IOException e)
                {
                    System.err.println("IOException Order");
                    e.printStackTrace();
                    System.exit(-1);
                }
               }
        });
        threadOrder.start();

        Thread threadReport = new Thread(new Runnable() {
            @Override
            public void run() {
                ServerSocket servsockReport;
                try
                {
                    // instantiate a socket for accepting a connection
                    servsockReport = new ServerSocket(7892);

                    // wait to accept a connection request and a data socket is returned
                    Socket sockReport = servsockReport.accept();

                    // get an output stream for writing to the data socket
                    OutputStream outStreamReport = sockReport.getOutputStream();

                    // create a PrinterWriter object for character-mode output
                    PrintWriter sockoutReport = new PrintWriter(new OutputStreamWriter(outStreamReport));

                    while(true)
                    {
                        if (co.changed)
                        {
                            //send report
                            sockoutReport.print(co.getReport()+"\n");
                            sockoutReport.flush();
                        }
                        //sleep
                        TimeUnit.SECONDS.sleep(25);
                    }
                }
                catch (IOException | InterruptedException e)
                {
                    System.err.println("Exception Report");
                    e.printStackTrace();
                    System.exit(-1);
                }
            }
        });
        threadReport.start();
    }
}