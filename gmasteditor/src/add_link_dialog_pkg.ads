-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                           GMastEditor                             --
--          Graphical Editor for Modelling and Analysis              --
--                    of Real-Time Applications                      --
--                                                                   --
--                       Copyright (C) 2001-2019                     --
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
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Box;       use Gtk.Box;
with Gtk.Button;    use Gtk.Button;
with Gtk.Table;     use Gtk.Table;
with Gtk.Combo_Box_Text;     use Gtk.Combo_Box_Text;
with Gtk.GEntry;    use Gtk.GEntry;
with Glib.Unicode;  use Glib.Unicode;
with Gtk.Label;     use Gtk.Label;
with Gtk.Window;    use Gtk.Window;

with Gtk.Enums; use Gtk.Enums;

package Add_Link_Dialog_Pkg is

   type Add_Link_Dialog_Record is new Gtk_Dialog_Record with record
      Hbox          : Gtk_Hbox;
      Ok_Button     : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Table         : Gtk_Table;
      Combo         : Gtk_Combo_Box_Text;
      Label         : Gtk_Label;
   end record;
   type Add_Link_Dialog_Access is access all Add_Link_Dialog_Record'Class;

   procedure Gtk_New (Add_Link_Dialog : out Add_Link_Dialog_Access);
   procedure Initialize
     (Add_Link_Dialog : access Add_Link_Dialog_Record'Class);

   procedure Gtk_New 
     (Add_Link_Dialog : out Add_Link_Dialog_Access;
      Parent : Gtk.Window.Gtk_Window);
   procedure Initialize
     (Add_Link_Dialog : access Add_Link_Dialog_Record'Class;
      Parent : Gtk.Window.Gtk_Window);

   Combo_Items : String_List.Glist;

end Add_Link_Dialog_Pkg;
