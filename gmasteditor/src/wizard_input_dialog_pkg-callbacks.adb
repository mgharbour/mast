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
with Ada.Text_IO;     use Ada.Text_IO;

package body Wizard_Input_Dialog_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------------------
   -- On_External_Event_Type_Changed --
   ------------------------------------

   procedure On_External_Event_Type_Changed
     (Object : access Wizard_Input_Dialog_Record'Class)
   is
      External_Type : String :=
        Get_Active_Text (Object.External_Event_Type_Combo);
   begin
      if External_Type = "Periodic" then
         Show_All (Object);
         Hide (Object.Singular_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Unbounded_Table);
         Hide (Object.Bursty_Table);
	 Object.Resize(150,150);
      elsif External_Type = "Singular" then
         Show_All (Object);
         Hide (Object.Periodic_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Unbounded_Table);
         Hide (Object.Bursty_Table);
	 Object.Resize(150,150);
      elsif External_Type = "Sporadic" then
         Show_All (Object);
         Hide (Object.Periodic_Table);
         Hide (Object.Singular_Table);
         Hide (Object.Unbounded_Table);
         Hide (Object.Bursty_Table);
	 Object.Resize(150,150);
      elsif External_Type = "Unbounded" then
         Show_All (Object);
         Hide (Object.Periodic_Table);
         Hide (Object.Singular_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Bursty_Table);
	 Object.Resize(150,150);
      elsif External_Type = "Bursty" then
         Show_All (Object);
         Hide (Object.Periodic_Table);
         Hide (Object.Singular_Table);
         Hide (Object.Sporadic_Table);
         Hide (Object.Unbounded_Table);
	 Object.Resize(150,150);
      end if;

   end On_External_Event_Type_Changed;

end Wizard_Input_Dialog_Pkg.Callbacks;
