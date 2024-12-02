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

package body Sop_Dialog_Pkg.Callbacks is

   use Gtk.Arguments;

   ----------------------------------
   -- On_Sop_Cancel_Button_Clicked --
   ----------------------------------

   procedure On_Sop_Cancel_Button_Clicked
     (Object : access Gtk_Dialog_Record'Class)
   is
   begin
      Destroy (Object);
   end On_Sop_Cancel_Button_Clicked;

   -----------------------------------------
   -- On_Overrid_Param_Type_Entry_Changed --
   -----------------------------------------

   procedure On_Overrid_Param_Type_Changed
     (Object : access Sop_Dialog_Record'Class)
   is
      Overrid_Param_Type : String :=
        String (Get_Active_Text (Object.Overrid_Param_Type_Combo));
   begin
      if Overrid_Param_Type = "(NONE)" then
         Hide (Object.Overrid_Prior_Table);
      else
         Show_All (Object);
      end if;
   end On_Overrid_Param_Type_Changed;

end Sop_Dialog_Pkg.Callbacks;
