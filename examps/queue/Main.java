/*
TorXakis - Model Based Testing
Copyright (c) 2015-2016 TNO and Radboud University
See license.txt
*/


/*********************************************************************
 A queue: what goes in must come out
 Communication via stream-mode socket;
 command line argument:
 <port number for the Server socket used in this process>.
 *********************************************************************/

import java.net.*;
import java.io.*;

public class Main

{  public static void main(String[] args)
   {  if (args.length != 1)
         System.out.println("own port number required");
      else
      {  try
         {  int portNo = Integer.parseInt(args[0]);

            // instantiate a socket for accepting a connection
            ServerSocket servsock = new ServerSocket(portNo);

            // wait to accept a connecion request
            // then a data socket is created
            Socket sock = servsock.accept();

            // get an input stream for reading from the data socket
            InputStream inStream = sock.getInputStream();
            // create a BufferedReader object for text line input
            BufferedReader sockin =
                   new BufferedReader(new InputStreamReader(inStream));
 
            // get an output stream for writing to the data socket
            OutputStream outStream = sock.getOutputStream();
            // create a PrinterWriter object for character-mode output
            PrintWriter sockout =
                    new PrintWriter(new OutputStreamWriter(outStream));
 
            String s;
            int sep1, sep2, x;
            Queue q = new Queue();
            q.show();

            while (true)
            {  s = sockin.readLine().trim();
               if (s.startsWith("Enq"))
               {  sep1 = s.indexOf("(");
                  sep2 = s.indexOf(")");
                  x = Integer.parseInt(s.substring(sep1+1,sep2).trim());
                  q.add(x);
                  // sockout.print("Ok\n");
               }
               if (s.startsWith("Deq"))
               {  if (q.empty())
                  {  // sockout.print("Empty\n");
                  }
                  else
                  {  sockout.print(q.take()+"\n"); }
               }
               sockout.flush();
               q.show();
            }
         }
         catch (Exception ex) { ex.printStackTrace(); }
      }
   }
}

/*********************************************************************/

