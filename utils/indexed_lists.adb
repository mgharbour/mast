-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2001-2024                     --
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

with List_Exceptions;
package body Indexed_Lists is

   ---------
   -- Add --
   ---------

   procedure Add
     (Value      :        Element;
      The_List   : in out List)
   is
   begin
      if The_List.First=null then
         The_List.First:=new Node'(Value => Value, Next => null);
         The_List.Last:=The_List.First;
      else
         The_List.Last.Next:=new Node'(Value => Value, Next => null);
         The_List.Last:=The_List.Last.Next;
      end if;
      The_List.Num:=The_List.Num+1;
   end Add;

   ------------
   -- Delete --
   ------------
   -- Delete is not efficient because it is not used by the tools
   -- only the editor uses it, and we do not need efficiency there

   procedure Delete
     (The_Index     : Index;
      Deleted_Value : out Element;
      The_List      : in out List)
   is
      Pred : Index:=The_List.First;
   begin
      -- check validity of index
      if The_Index=null or else Pred=null then
         raise List_Exceptions.Invalid_Index;
      end if;
      -- determine whether first or subsequent element
      if The_Index=The_List.First then
         -- delete the first element
         Deleted_Value:=The_Index.Value;
         The_List.First:=The_Index.Next;
         The_List.Num:=The_List.Num-1;
         if The_List.Num=0 then
            The_List.First:=null;
            The_List.Last:=null;
         end if;
      else
         for I in 1..The_List.Num loop
            if Pred.Next=The_Index then
               -- delete the (non-first) element
               Deleted_Value:=The_Index.Value;
               Pred.Next:=The_Index.Next;
               The_List.Num:=The_List.Num-1;
               if The_Index.Next=null then
                  The_List.Last:=Pred;
               end if;
               return;
            end if;
            Pred:=Pred.Next;
         end loop;
         raise List_Exceptions.Invalid_Index;
      end if;
   end Delete;

   ----------
   -- Find --
   ----------

   function Find
     (The_Value : Element;
      The_List  : List   )
     return Index
   is
      Current : Index:=The_List.First;
   begin
      for I in 1..The_List.Num loop
         if The_Value=Current.Value then
            return Current;
         end if;
         Current:=Current.Next;
      end loop;
      return Null_Index;
   end Find;

   -------------------
   -- Get_Next_Item --
   -------------------

   procedure Get_Next_Item
     (Value    : out Element;
      The_List : List;
      Iterator : in out Index)
   is
      pragma Unreferenced (The_List);
   begin
      if Iterator=null then
         raise List_Exceptions.No_More_Items;
      else
         Value:=Iterator.Value;
         Iterator:=Iterator.Next;
      end if;
   end Get_Next_Item;

   ----------
   -- Item --
   ----------

   function Item
     (The_Index : Index;
      The_List  : List)
     return Element
   is
      pragma Unreferenced (The_List);
   begin
      if The_Index=null then
         raise List_Exceptions.Invalid_Index;
      else
         return The_Index.Value;
      end if;
   end Item;

   ------------
   -- Rewind --
   ------------

   procedure Rewind
     (The_List : List;
      Iterator : out Index)
   is
   begin
      Iterator:=The_List.First;
   end Rewind;

   ----------
   -- Size --
   ----------

   function Size
     (The_List : List)
     return Natural
   is
   begin
      return The_List.Num;
   end Size;

   -----------
   -- Stack --
   -----------

   procedure Stack
     (Value      :        Element;
      The_List   : in out List)
   is
      Temp : Index;
   begin
      if The_List.First=null then
         The_List.First:=new Node'(Value => Value, Next => null);
         The_List.Last:=The_List.First;
      else
         Temp:=new Node'(Value => Value, Next => The_List.First);
         The_List.First:=Temp;
      end if;
      The_List.Num:=The_List.Num+1;
   end Stack;

   ------------
   -- Update --
   ------------

   procedure Update
     (The_Index :        Index;
      New_Value :        Element;
      The_List  : in out List)
   is
      pragma Unreferenced (The_List);
   begin
      if The_Index=null then
         raise List_Exceptions.Invalid_Index;
      else
         The_Index.Value:=New_Value;
      end if;
   end Update;

end Indexed_Lists;

