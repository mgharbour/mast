-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                           GMastEditor                             --
--          Graphical Editor for Modelling and Analysis              --
--                    of Real-Time Applications                      --
--                                                                   --
--                       Copyright (C) 2001-2014                     --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors : Pilar del Rio                                           --
--           Michael Gonzalez                                        --
--                                                                   --
-- Contact info: Michael Gonzalez       mgh@unican.es                --
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
with System;          use System;
with Glib;            use Glib;
with Gdk.Event;       use Gdk.Event;
with Gdk.Types;       use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Style;       use Gtk.Style;
with Gtk.Widget;      use Gtk.Widget;
with Utilities;       use Utilities;

package body Sched_Server_Dialog_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------------------
   -- On_Server_Cancel_Button_Clicked --
   -------------------------------------

   procedure On_Server_Cancel_Button_Clicked
     (Object : access Gtk_Dialog_Record'Class)
   is
   begin
      Destroy (Object);
   end On_Server_Cancel_Button_Clicked;

   --------------------------------
   -- On_Sync_Type_Entry_Changed --
   --------------------------------

   procedure On_Sync_Type_Entry_Changed
     (Object : access Sched_Server_Dialog_Record'Class)
   is
      Sync_Type : String :=
        String (Get_Active_Text (Object.Sync_Type_Combo));
   begin
      if Sync_Type = "(NONE)" then
         Hide (Object.Srp_Table);
      else
         Show_All (Object.Srp_Table);
      end if;
   end On_Sync_Type_Entry_Changed;

   ----------------------------------
   -- On_Policy_Type_Entry_Changed --
   ----------------------------------

   procedure On_Policy_Type_Entry_Changed
     (Object : access Sched_Server_Dialog_Record'Class)
   is
      Policy_Type : String :=
        String (Get_Active_Text (Object.Policy_Type_Combo));
      Yes_No      : String_List.Glist;
   begin
      String_List.Append (Yes_No, "YES");
      if Policy_Type /= "Interrupt Fixed Priority" then
         String_List.Prepend (Yes_No, "NO");
      end if;
      Utilities.Set_Popdown_Strings (Object.Pre_Prior_Combo, Yes_No);

      if Policy_Type = "Non Preemptible Fixed Priority"
        or else Policy_Type = "Fixed Priority"
        or else Policy_Type = "Interrupt Fixed Priority"
      then
         Show_All (Object.Priority_Table);
         Hide (Object.Polling_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Edf_Table);
      elsif Policy_Type = "Polling" then
         Show_All (Object.Priority_Table);
         Show_All (Object.Polling_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Edf_Table);
      elsif Policy_Type = "Sporadic Server" then
         Show_All (Object.Priority_Table);
         Show_All (Object.Sporadic_Table);
         Hide (Object.Polling_Table);
         Hide (Object.Edf_Table);
      elsif Policy_Type = "Earliest Deadline First" then
         Show_All (Object.Edf_Table);
         Hide (Object.Priority_Table);
         Hide (Object.Polling_Table);
         Hide (Object.Sporadic_Table);
      else
         Hide (Object.Priority_Table);
         Hide (Object.Polling_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Edf_Table);
      end if;
      Object.Resize(300,150);
   end On_Policy_Type_Entry_Changed;

end Sched_Server_Dialog_Pkg.Callbacks;
