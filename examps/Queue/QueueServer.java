/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/


/*********************************************************************
 A queue: what goes in must come out
 Communication via stream-mode socket;
 command line argument:
 <port number for the Server socket used in this process>.
 *********************************************************************/

import java.net.*;
import java.io.*;

public class QueueServer

{  public static void main(String[] args)
   {  if (args.length != 1)
         System.out.println("own port number required");
      else
      {  try
         {  int portNo = Integer.parseInt(args[0]);

            // instantiate a socket for accepting a connection
            ServerSocket servsock = new ServerSocket(portNo);

            System.out.println("Waiting for tester");
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

            System.out.println("Tester connected.");

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

class Queue
{  private Cell first = null;
   private Cell last  = null;

   public void add(int x)
   {  Cell h = new Cell();
      h.data = x;
      h.rest = null;
      if (empty())
      {  first = h; }
      else
      {  last.rest = h; } ;
      last = h;
   }

   public boolean empty()
   {  return(first==null); }

   public int take()
   {  Cell h = first;
      first = h.rest;
      return(h.data);
   }

   public void show()
   {  System.out.print("[ ");
      Cell h = first;
      while (h != null)
      {  System.out.print(h.data);
         h = h.rest;
         if (h != null)
         {  System.out.print(", ");
         }
      }
      System.out.println(" ]");
   }
      
}

class Cell
{  public int data;
   public Cell rest;
}

