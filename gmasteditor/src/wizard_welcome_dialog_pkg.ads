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
with Gtk.Dialog;      use Gtk.Dialog;
with Gtk.Button;      use Gtk.Button;
with Gtk.Box;         use Gtk.Box;
with Gtk.Alignment;   use Gtk.Alignment;
with Gtk.Image;       use Gtk.Image;
with Gtk.Label;       use Gtk.Label;
with Gtk.Table;       use Gtk.Table;
with Gtk.Text_View;   use Gtk.Text_View;
with Gtk.Text_Buffer; use Gtk.Text_Buffer;
with Gtk.Text_Iter;   use Gtk.Text_Iter;
with Gdk.Pixbuf;

package Wizard_Welcome_Dialog_Pkg is

   type Wizard_Welcome_Dialog_Record is new Gtk_Dialog_Record with record
      Cancel_Button : Gtk_Button;
      Alignment1    : Gtk_Alignment;
      Hbox76        : Gtk_Hbox;
      Image2        : Gtk_Image;
      Label527      : Gtk_Label;
      Next_Button   : Gtk_Button;
      Alignment3    : Gtk_Alignment;
      Hbox78        : Gtk_Hbox;
      Image4        : Gtk_Image;
      Label529      : Gtk_Label;
      Table1        : Gtk_Table;
      Label         : Gtk_Label;
      Textview      : Gtk_Text_View;
      Image         : Gtk_Image;
      Pixbuf1       : Gdk.Pixbuf.Gdk_Pixbuf;
   end record;
   
   type Wizard_Welcome_Dialog_Access is access Wizard_Welcome_Dialog_Record'
     Class;

   procedure Gtk_New
     (Wizard_Welcome_Dialog : out Wizard_Welcome_Dialog_Access);
   procedure Initialize
     (Wizard_Welcome_Dialog : access Wizard_Welcome_Dialog_Record'Class);

end Wizard_Welcome_Dialog_Pkg;
