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
with Glib;                      use Glib;
with Gtk;                       use Gtk;
with Gdk.Types;                 use Gdk.Types;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Enums;                 use Gtk.Enums;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Utilities;                 use Utilities;
with Callbacks_Mast_Editor;     use Callbacks_Mast_Editor;
with Mast_Editor_Intl;          use Mast_Editor_Intl;
with Mieh_Dialog_Pkg.Callbacks; use Mieh_Dialog_Pkg.Callbacks;
with Mast_Editor_Window_Pkg;

package body Mieh_Dialog_Pkg is

   procedure Gtk_New (Mieh_Dialog : out Mieh_Dialog_Access) is
   begin
      Mieh_Dialog := new Mieh_Dialog_Record;
      Mieh_Dialog_Pkg.Initialize (Mieh_Dialog);
   end Gtk_New;
   
   procedure Gtk_New 
     (Mieh_Dialog : out Mieh_Dialog_Access;
      Parent : Gtk.Window.Gtk_Window) is
   begin
      Mieh_Dialog := new Mieh_Dialog_Record;
      Mieh_Dialog_Pkg.Initialize (Mieh_Dialog, Parent);
   end Gtk_New;
   
   procedure Initialize 
     (Mieh_Dialog : access Mieh_Dialog_Record'Class) is
   begin
      Initialize
	(Mieh_Dialog, 
	 Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window));
   end Initialize;

   procedure Initialize 
     (Mieh_Dialog : access Mieh_Dialog_Record'Class;
      Parent : Gtk.Window.Gtk_Window)
   is
      pragma Suppress (All_Checks);
      Minput_Type_Combo_Items : String_List.Glist;

   begin
      --  Gtk.Dialog.Initialize (Mieh_Dialog);
      --  Set_Title (Mieh_Dialog, -"Multi-Input Event Handler Parameters");
      --  Set_Modal (Mieh_Dialog, True);
      Gtk.Dialog.Initialize 
	(Dialog => Mieh_Dialog,
	 Title => "Multi-Input Event Handler Parameters",
	 Parent => Parent,
	 Flags => Gtk.Dialog.Modal);                
      
      Set_Position (Mieh_Dialog, Win_Pos_Center_Always);

      Gtk_New_Hbox (Mieh_Dialog.Hbox72, True, 100);
      Pack_Start (Get_Action_Area (Mieh_Dialog), Mieh_Dialog.Hbox72);

      Gtk_New (Mieh_Dialog.Ok_Button, -"OK");
      Set_Relief (Mieh_Dialog.Ok_Button, Relief_Normal);
      Pack_Start
        (Mieh_Dialog.Hbox72,
         Mieh_Dialog.Ok_Button,
         Expand  => True,
         Fill    => True,
         Padding => 40);

      Gtk_New (Mieh_Dialog.Cancel_Button, -"Cancel");
      Set_Relief (Mieh_Dialog.Cancel_Button, Relief_Normal);
      Pack_End
        (Mieh_Dialog.Hbox72,
         Mieh_Dialog.Cancel_Button,
         Expand  => True,
         Fill    => True,
         Padding => 40);
      --     Button_Callback.Connect
      --       (Mieh_Dialog.Cancel_Button, "clicked",
      --        Button_Callback.To_Marshaller
      --(On_Cancel_Button_Clicked'Access), False);

      Dialog_Callback.Object_Connect
        (Mieh_Dialog.Cancel_Button,
         "clicked",
         Dialog_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access),
         Mieh_Dialog);

      Gtk_New (Mieh_Dialog.Table18, 1, 2, True);
      Set_Border_Width (Mieh_Dialog.Table18, 5);
      Set_Row_Spacings (Mieh_Dialog.Table18, 5);
      Set_Col_Spacings (Mieh_Dialog.Table18, 3);
      Pack_Start
        (Get_Content_Area (Mieh_Dialog),
         Mieh_Dialog.Table18,
         Expand  => False,
         Fill    => True,
         Padding => 0);

      Gtk_New (Mieh_Dialog.Label202, -("Multi-Input Event Handler Type"));
      Set_Alignment (Mieh_Dialog.Label202, 0.95, 0.5);
      Set_Padding (Mieh_Dialog.Label202, 0, 0);
      Set_Justify (Mieh_Dialog.Label202, Justify_Center);
      Set_Line_Wrap (Mieh_Dialog.Label202, False);
      Set_Selectable (Mieh_Dialog.Label202, False);
      Set_Use_Markup (Mieh_Dialog.Label202, False);
      Set_Use_Underline (Mieh_Dialog.Label202, False);
      Attach
        (Mieh_Dialog.Table18,
         Mieh_Dialog.Label202,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (Mieh_Dialog.Minput_Type_Combo);
      --Set_Value_In_List (Mieh_Dialog.Minput_Type_Combo, False);
      --Set_Use_Arrows (Mieh_Dialog.Minput_Type_Combo, True);
      --Set_Case_Sensitive (Mieh_Dialog.Minput_Type_Combo, False);
      --Set_Editable (Get_Entry (Mieh_Dialog.Minput_Type_Combo), False);
      --Set_Max_Length (Get_Entry (Mieh_Dialog.Minput_Type_Combo), 0);
      --Set_Invisible_Char
      --  (Get_Entry (Mieh_Dialog.Minput_Type_Combo),
      --   UTF8_Get_Char ("*"));
      --Set_Has_Frame (Get_Entry (Mieh_Dialog.Minput_Type_Combo), True);
      String_List.Append (Minput_Type_Combo_Items, -("Concentrator"));
      String_List.Append (Minput_Type_Combo_Items, -("Barrier"));
      Utilities.Set_Popdown_Strings
        (Mieh_Dialog.Minput_Type_Combo,
         Minput_Type_Combo_Items);
      Set_Text_In_Combo_Box
        (Mieh_Dialog.Minput_Type_Combo,
         -("Concentrator"));
      Free_String_List (Minput_Type_Combo_Items);
      Attach
        (Mieh_Dialog.Table18,
         Mieh_Dialog.Minput_Type_Combo,
         Left_Attach   => 1,
         Right_Attach  => 2,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

   end Initialize;

end Mieh_Dialog_Pkg;
