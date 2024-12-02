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

with Var_Strings;

generic
   type Element is private;
   with function Name (E : Element ) return Var_Strings.Var_String;
package Named_Lists is

   type List is private;

   type Index is private;
   Null_Index : constant Index;

   subtype Iteration_Object is Index;

   procedure Add
     (Value     :        Element;
      The_List  : in out List);
   -- raises Already_Exists if Found

   procedure Add_Or_Find
     (Value     :        Element;
      The_Index :    out Index;
      The_List  : in out List);

   procedure Update
     (The_Index :        Index;
      New_Value :        Element;
      The_List  : in out List     );

   procedure Delete
     (The_Index     : Index;
      Deleted_Value : out Element;
      The_List      : in out List );
   -- not efficient, because seldom used

   function Item
     (The_Index : Index;
      The_List  : List   )
     return Element;

   function Find
     (The_Name : Var_Strings.Var_String;
      The_List : List                    )
     return Index;
   -- returns Null_Index if not found

   function Size
     (The_List : List )
     return Natural;

   procedure Rewind
     (The_List : List; Iterator : out Index );

   procedure Get_Next_Item
     (Value    :    out Element;
      The_List : List;
      Iterator : in out Index);

private

   type Node;

   type Index is access Node;

   Null_Index : constant Index:=null;

   type Node is record
      Value : Element;
      Next  : Index;
   end record;

   type List is
      record
         First, Last : Index:=null;
         Num      : Natural:=0;
      end record;

end Named_Lists;
