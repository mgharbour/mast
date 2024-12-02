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
with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Combo_Box; use Gtk.Combo_Box;
with Error_Window_Pkg; use Error_Window_Pkg;
with Help_Hopa_Pkg; use Help_Hopa_Pkg;
with Mast.Sched_Param_Assignment_Parameters;
with Mast.Hospa_Parameters;
with Parameters_Handling;
with Ada.Strings,Ada.Strings.Fixed,
 Ada.Characters.Handling;
use  Ada.Strings,Ada.Strings.Fixed,Ada.Characters.Handling;

package body Hopa_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   type Float_Array is array(Positive range <>) of Float;
   type Integer_Array is array(Positive range <>) of Integer;

   -------------
   -- Size_Of --
   -------------

   function Size_Of (L : String) return Natural is
   begin
      return Count(L,",")+1;
   end Size_Of;

   -------------
   -- Convert --
   -------------

   function Convert (L : String) return Float_Array is
      Size : Natural:=0;
      Pos,Pos_Comma : Natural;
      Num : Natural:=1;
      S: String := Trim(L,Both);
      List : Float_Array(1..Size_Of(L));
   begin
      if S(1)/='(' or S(S'Length)/=')' then
         raise Constraint_Error;
      end if;
      Pos:=2;
      loop
         Pos_Comma:=Index(S(Pos..S'Length-1),",");
         if Pos_Comma=0 then
            if Num/=Size_Of(L) then
               raise Constraint_Error;
            end if;
            List(Num):=Float'Value(S(Pos..S'Length-1));
            return List;
         else
            List(Num):=Float'Value(S(Pos..Pos_Comma-1));
            Pos:=Pos_Comma+1;
            Num:=Num+1;
         end if;
      end loop;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert (L : String) return Integer_Array is
      Size : Natural:=0;
      Pos,Pos_Comma : Natural;
      Num : Natural:=1;
      S: String := Trim(L,Both);
      List : Integer_Array(1..Size_Of(L));
   begin
      if S(1)/='(' or S(S'Length)/=')' then
         raise Constraint_Error;
      end if;
      Pos:=2;
      loop
         Pos_Comma:=Index(S(Pos..S'Length-1),",");
         if Pos_Comma=0 then
            if Num/=Size_Of(L) then
               raise Constraint_Error;
            end if;
            List(Num):=Integer'Value(S(Pos..S'Length-1));
            return List;
         else
            List(Num):=Integer'Value(S(Pos..Pos_Comma-1));
            Pos:=Pos_Comma+1;
            Num:=Num+1;
         end if;
      end loop;
   end Convert;

   ----------------------------
   -- On_Hset_Button_Clicked --
   ----------------------------

   procedure On_Hset_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      type Steps is (Ka,Kr,Equal,Iter,Overiter);
   begin
      declare
         Ka_List : String:=Get_Text(Hopa_Window.Ka_Entry);
         Kr_List : String:=Get_Text(Hopa_Window.Kr_Entry);
         Kr_Array, Ka_Array : Float_Array(1..Size_Of(Ka_List));
         Iter_List : String:=Get_Text(Hopa_Window.Iter_List_Entry);
         Iter_Array : Integer_Array(1..Size_Of(Iter_List));
         Over_Iter : String:=Get_Text(Hopa_Window.Opiter_Entry);
         Overiterations : Mast.Hospa_Parameters.Iteration_Type;
         Step : Steps;
         Pair : Mast.Hospa_Parameters.K_Pair;
         Initial : Mast.Hospa_Parameters.Valid_Initial_Assignments:=
           Mast.Hospa_Parameters.Valid_Initial_Assignments'Value
           (Get_Active_Text(Hopa_Window.Initial_Assignment_Combo));

      begin

         Mast.Hospa_Parameters.Set_Initialization(Initial);

         Step:=Equal;
         if Size_Of(Ka_List)/=Size_Of(Kr_List) then
            raise Constraint_Error;
         end if;

         Step:=Ka;
         Ka_Array:=Convert(Ka_List);
         Step:=Kr;
         Kr_Array:=Convert(Kr_List);
         Mast.Hospa_Parameters.Init_K_List;
         for I in Ka_Array'Range loop
            Mast.Hospa_Parameters.Set_Ka(Ka_Array(I),Pair);
            Mast.Hospa_Parameters.Set_Kr(Kr_Array(I),Pair);
            Mast.Hospa_Parameters.Set_Next_K_Pair(Pair);
         end loop;

         Step:=Iter;
         Iter_Array:=Convert(Iter_List);
         Mast.Hospa_Parameters.Init_Iterations_List;
         for I in Iter_Array'Range loop
            Mast.Hospa_Parameters.Set_Next_Iterations(Iter_Array(I));
         end loop;


         Step:=Overiter;
         Overiterations:=Mast.Hospa_Parameters.Iteration_Type'Value
           (Over_Iter);
         Mast.Hospa_Parameters.Set_Overiterations(Overiterations);

         Mast.Sched_Param_Assignment_Parameters.Store_Parameters;
         Destroy(Hopa_Window);
      exception
         when Constraint_Error =>
            Gtk_New (Error_Window);
            Set_Position(Error_Window,Win_Pos_Mouse);
            Show_All (Error_Window);
            case Step is
               when Ka =>
                  Set_Text(Error_Window.Label_Error,
                           "Format Error in Ka List");
               when Kr =>
                  Set_Text(Error_Window.Label_Error,
                           "Format Error in Kr List");
               when Equal =>
                  Set_Text(Error_Window.Label_Error,
                           "Ka and Kr Lists have different sizes");
               when Iter =>
                  Set_Text(Error_Window.Label_Error,
                           "Format Error in Iterations List");
               when Overiter =>
                  Set_Text(Error_Window.Label_Error,
                           "Format Error in Optimization Iterations");
            end case;
            Set_Modal(Error_Window,True);
      end;
   end On_Hset_Button_Clicked;

   -------------------------------------
   -- On_Hget_Defaults_Button_Clicked --
   -------------------------------------

   procedure On_Hget_Defaults_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Mast.Hospa_Parameters.Load_Default_Parameters;
      Parameters_Handling.Load_Hospa;
   end On_Hget_Defaults_Button_Clicked;

   ---------------------------------
   -- On_Hopa_Help_Button_Clicked --
   ---------------------------------

   procedure On_Hopa_Help_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Gtk_New (Help_HOPA);
      Show_All (Help_HOPA);
   end On_Hopa_Help_Button_Clicked;

   -------------------------------
   -- On_Hcancel_Button_Clicked --
   -------------------------------

   procedure On_Hcancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Destroy(Hopa_Window);
   end On_Hcancel_Button_Clicked;

end Hopa_Window_Pkg.Callbacks;
