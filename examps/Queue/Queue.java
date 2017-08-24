/*
TorXakis - Model Based Testing
Copyright (c) 2015-2017 TNO and Radboud University
See LICENSE at root directory of this repository.
*/


/*********************************************************************
 Class for Queue
 *********************************************************************/

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

/*********************************************************************/
