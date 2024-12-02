-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2001-2014                     --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors: Michael Gonzalez       mgh@unican.es                     --
--          Jose Javier Gutierrez  gutierjj@unican.es                --
--          Jose Carlos Palencia   palencij@unican.es                --
--          Jose Maria Drake       drakej@unican.es                  --
--                                                                   --
-- This program is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This program is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this program; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-----------------------------------------------------------------------
--                      PACKAGE BODY PRIORITY_QUEUE
--                      ===========================
--
--  The implementation of the priority queue is a heapform heap
--  [W. AMSBURY, "Data Structures. From Arrays to Priority Queues",
--  Wadsworth, 1985.
--
--  This implementation has the following characteristics:
--
--      Insertion : O(log n)
--      Highest_priority extraction : O(log n)
--      Lowest_priority extraction : O(n)
--      Read highest priority : O(1)
--      Deletion : O(n) (Change is Deletion + Insertion = O(n)).
-----------------------------------------------------------------------

with List_Exceptions,Ada.Unchecked_Deallocation;

package body Priority_Queues is

   procedure Free is new Ada.Unchecked_Deallocation (Storing_Space,Storing_Space_Ref);

   ------------
   -- UpHeap --
   ------------

   procedure UpHeap (N : Positive;
                     Q : in out Queue) is

      P : Positive;
      K : Positive := N;
      Temp : Cell;

   begin
      while K >= 2 loop
         P:= K/2;
         if Q.Q(K).Pri > Q.Q(P).Pri then
            Temp:=Q.Q(K);
            Q.Q(K):=Q.Q(P);
            Q.Q(P):=Temp;
            K:=P;
         else
            exit;
         end if;
      end loop;
   end UpHeap;

   procedure Init(Q : in out Queue) is
   begin
      Q.Length:=0;
      Q.Q:=null;
   end Init;

   -----------
   -- Empty --
   -----------

   function Empty (Q : Queue) return Boolean is

   begin
      return Q.Length=0;
   end Empty;

   ------------
   -- Length --
   ------------

   function Length (Q : Queue) return Natural is

   begin
      return Q.Length;
   end Length;
   pragma Unreferenced (Length);

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue (E : Element;
                      P : Priority;
                      Q : in out Queue) is
      New_Queue : Storing_Space_Ref;
      To_Deallocate : Storing_Space_Ref;
      Size : Positive;
   begin
      if Q.Q=null then
         Q.Q:=new Storing_Space(1..Initial_Size);
      else
         Size := Q.Q'Length;
         if Q.Length>=Size then
            New_Queue:= new Storing_Space(1..Size*2);
            New_Queue(1..Size):=Q.Q.all;
            To_Deallocate := Q.Q;
            Q.Q:=New_Queue;

            Free(To_Deallocate);
         end if;
      end if;
      Q.Length:=Q.Length+1;
      Q.Q(Q.Length):=(E,P);
      UpHeap(Q.Length,Q);
   end Enqueue;


   --------------
   -- DownHeap --
   --------------

   procedure DownHeap (K : Positive;
                       N : Natural;
                       Q : in out Queue) is

      J : Integer:=2*K;
      Half : Positive:=K;
      Temp : Cell:= Q.Q(K);

   begin
      while J<=N loop
         if (J<N) and then (Q.Q(J+1).Pri>Q.Q(J).Pri) then
            J:=J+1;
         end if;
         if not (Q.Q(J).Pri > Temp.Pri) then
            Q.Q(Half):=Temp;
            return;
         end if;
         Q.Q(Half):=Q.Q(J);
         Half:=J;
         J:=2*J;
      end loop;
      Q.Q(Half):=Temp;
   end DownHeap;


   -------------
   -- Dequeue --
   -------------

   procedure Dequeue (E : out Element;
                      P : out Priority;
                      Q : in out Queue) is
      To_Deallocate : Storing_Space_Ref := Q.Q;
   begin
      if Q.Length=0 then
         raise List_Exceptions.Empty;
      end if;
      E:=Q.Q(1).E;
      P:=Q.Q(1).Pri;
      Q.Q(1):=Q.Q(Q.Length);
      Q.Length:=Q.Length-1;

      if Q.Length = 0 then
         Free(To_Deallocate);
      else
         DownHeap(1,Q.Length,Q);
      end if;
   end Dequeue;

   --------------------
   -- Dequeue_Middle --
   --------------------

   procedure Dequeue_Middle (E : in out Element;
                             P : out Priority;
                             Q : in out Queue;
                             Found : out Boolean) is

      I : Integer :=0;
      Parent : Integer;

   begin
      Found:=False;
      while I<Q.Length loop
         I:=I+1;
         if E=Q.Q(I).E then
            Found:=True;
            E:=Q.Q(I).E;
            P:=Q.Q(I).Pri;
            Q.Q(I):=Q.Q(Q.Length);
            Q.Length:=Q.Length-1;
            Parent:=I/2;
            if Parent=0 or else Q.Q(Parent).Pri > Q.Q(I).Pri then
               DownHeap(I,Q.Length,Q);
            else
               UpHeap(I,Q);
            end if;
            exit;
         end if;
      end loop;
   end Dequeue_Middle;

   --------------------
   -- Dequeue_Middle --
   --------------------

   procedure Dequeue_Middle (E : in out Element;
                             P : out Priority;
                             Q : in out Queue) is

      I : Integer :=0;
      Parent : Integer;
      Found:Boolean:=False;

   begin
      while I<Q.Length loop
         I:=I+1;
         if E=Q.Q(I).E then
            Found:=True;
            E:=Q.Q(I).E;
            P:=Q.Q(I).Pri;
            Q.Q(I):=Q.Q(Q.Length);
            Q.Length:=Q.Length-1;
            Parent:=I/2;
            if Parent=0 or else Q.Q(Parent).Pri > Q.Q(I).Pri then
               DownHeap(I,Q.Length,Q);
            else
               UpHeap(I,Q);
            end if;
            exit;
         end if;
      end loop;
      if not Found then
         raise List_Exceptions.Not_Found;
      end if;
   end Dequeue_Middle;


   ----------------
   -- Read_First --
   ----------------

   procedure Read_First (E : out Element;
                         P : out Priority;
                         Q : Queue) is

   begin
      if Q.Length=0 then
         raise List_Exceptions.Empty;
      else
         E:=Q.Q(1).E;
         P:=Q.Q(1).Pri;
      end if;
   end Read_First;

   --------------
   -- Set_Prio --
   --------------

   procedure Set_Prio (E : Element;
                       P : Priority;
                       Q : in out Queue) is

      Elem : Element;
      Parent : Integer;
   begin
      for I in 1..Q.Length loop
         if E=Q.Q(I).E then
            if P=Q.Q(I).Pri then
               return;
            else
               Elem:=Q.Q(I).E;
               Q.Q(I):=Q.Q(Q.Length);
               Q.Length:=Q.Length-1;
               Parent:=I/2;
               if Parent=0 or else Q.Q(Parent).Pri > Q.Q(I).Pri then
                  DownHeap(I,Q.Length,Q);
               else
                  UpHeap(I,Q);
               end if;
               DownHeap(I,Q.Length,Q);
               Enqueue(Elem,P,Q);
               return;
            end if;
         end if;
      end loop;
      raise List_Exceptions.Not_Found;
   end Set_Prio;

end Priority_Queues;
