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
--           Felisa Hidalgo                                          --
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
with Gtk.Dialog;      use Gtk.Dialog;
with Gtk.Main;        use Gtk.Main;
with Gtk.Window;      use Gtk.Window;
with Mast_Editor_Window_Pkg;
with Ada.Text_IO;                       use Ada.Text_IO;
with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with Gnat.Strings;
with Gtk.Main;                          use Gtk.Main;
with Gtkada.Dialogs;                    use Gtkada.Dialogs;
with Editor_Actions;                    --use Editor_Actions;
with Open_File_Selection_Pkg;           use Open_File_Selection_Pkg;
with Save_File_Selection_Pkg;           use Save_File_Selection_Pkg;
with Import_File_Selection_Pkg;         use Import_File_Selection_Pkg;
with Editor_Error_Window_Pkg;           use Editor_Error_Window_Pkg;
with Save_Changes_Dialog_Pkg;           use Save_Changes_Dialog_Pkg;
with File_Execution;
with Change_Control;
with Simple_Transaction_Wizard_Control;
with Mast_Editor;
with Mast;

package body Mast_Editor_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------
   -- On_New_Activate --
   ---------------------

   procedure On_New_Activate (Object : access Gtk_Menu_Item_Record'Class) is
   begin
      Editor_Actions.New_File;
   end On_New_Activate;

   ----------------------
   -- On_Open_Activate --
   ----------------------

   procedure On_Open_Activate (Object : access Gtk_Menu_Item_Record'Class) is
      Save_Changes_Dialog : Save_Changes_Dialog_Access;
      Open_File_Selection : Open_File_Selection_Access;
   begin
      if Change_Control.Saved_Changes then
         Gtk_New (Open_File_Selection);
         Show_All (Open_File_Selection);
      else
         Gtk_New (Save_Changes_Dialog);
         Show_All (Save_Changes_Dialog);
         Save_Changes_Dialog.Open_File := True;
      end if;
   end On_Open_Activate;

   ------------------------
   -- On_Import_Activate --
   ------------------------

   procedure On_Import_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Import_File_Selection : Import_File_Selection_Access;
   begin
      Gtk_New (Import_File_Selection);
      Show_All (Import_File_Selection);
   end On_Import_Activate;

   ----------------------
   -- On_Save_Activate --
   ----------------------

   procedure On_Save_Activate (Object : access Gtk_Menu_Item_Record'Class) is
      Save_File_Selection : Save_File_Selection_Access;
   begin
      if Editor_Actions.Current_Filename = null then
         Gtk_New (Save_File_Selection);
         Show_All (Save_File_Selection);
      else
         Editor_Actions.Save_System (Editor_Actions.Current_Filename.all);
         Editor_Actions.Save_Editor_System 
	   (Editor_Actions.Current_Filename.all);
      end if;
   exception
      when Ada.Text_IO.Name_Error   |
        Ada.Text_IO.Status_Error |
        Ada.Text_IO.Use_Error    =>
         Gtk_New (Editor_Error_Window);
         Set_Position (Editor_Error_Window, Win_Pos_Mouse);
         Set_Text
           (Editor_Error_Window.Label,
            "Error while saving file " & Editor_Actions.Current_Filename.all);
         Show_All (Editor_Error_Window);
   end On_Save_Activate;

   -------------------------
   -- On_Save_As_Activate --
   -------------------------

   procedure On_Save_As_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Save_File_Selection : Save_File_Selection_Access;
      Success : Boolean;
   begin
      Gtk_New (Save_File_Selection);
      if Editor_Actions.Current_Filename /= null then
         Success:= Set_Filename 
	   (Save_File_Selection, Editor_Actions.Current_Filename.all);
      end if;
      Show_All (Save_File_Selection);
   end On_Save_As_Activate;

   ----------------------
   -- On_Quit_Activate --
   ----------------------

   procedure On_Quit_Activate (Object : access Gtk_Menu_Item_Record'Class) is
      Save_Changes_Dialog : Save_Changes_Dialog_Access;
   begin
      if Change_Control.Saved_Changes then
         Gtk.Main.Main_Quit;
      else
         Gtk_New (Save_Changes_Dialog);
         Show_All (Save_Changes_Dialog);
         Save_Changes_Dialog.Quit:=True;
      end if;
   end On_Quit_Activate;

   -------------------------------------------
   -- On_Create_Simple_Transaction_Activate --
   -------------------------------------------

   procedure On_Create_Simple_Transaction_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      Simple_Transaction_Wizard_Control.Create_Simple_Transaction_Wizard;
   end On_Create_Simple_Transaction_Activate;

   -------------------------------------------
   -- On_Create_Linear_Transaction_Activate --
   -------------------------------------------

   procedure On_Create_Linear_Transaction_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Create_Linear_Transaction_Activate;

   --------------------------
   -- On_Analysis_Activate --
   --------------------------

   procedure On_Analysis_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Response : Gtk_Response_Type;
      Save_Changes_Dialog : Save_Changes_Dialog_Access;
      Argument1 : access String;
      Message : access String;
      -- No_Arguments : File_Execution.String_List(1..0);
      Arg_List :  File_Execution.String_List(1..1);
      Success,Running : Boolean;
      Button : Message_Dialog_Buttons;
   begin
      if not Change_Control.Saved_Changes then
         Gtk_New (Save_Changes_Dialog);
         Show_All (Save_Changes_Dialog);
         Response:=Run(Save_Changes_Dialog);
         --Hide_All(Save_Changes_Dialog);
         while Gtk.Main.Events_Pending loop
            Running:=Gtk.Main.Main_Iteration;
         end loop;
      end if;
      -- Gtk_New (Mast_Analysis);
      -- Set_Position (Mast_Analysis, Win_Pos_Center);
      -- Show_All (Mast_Analysis);
      -- Read_Past_Values;

      -- The following code is dependent on the GNAT compiler.
      -- If compiling with some other compiler you can comment it out,
      -- and make the call to the analysis tool with no arguments
      if Editor_Actions.Current_Filename/=null then
         Argument1 := Editor_Actions.Current_Filename;
      else
         Argument1 := new String'("");
      end if;
      Arg_List(1) := Gnat.Strings.String_Access(Argument1);
      
      -- Invoke Mast driver
      Message:=new String'("gmast_analysis tool driver not found");
      File_Execution.Execute_Command("gmast_analysis",Arg_List,Success);

      if Success then
         -- Invoke Mast analysis
         Message:=new String'
           ("mast_analysis tool or mast_command file not found");
         File_Execution.Execute_File("mast_command",Success);

         if Success then
            -- Invoke Results Viewer
            Message:=new String'
              ("gmast_results viewer or mast_results_command file not found");
            File_Execution.Execute_File("mast_results_command",Success);
         end if;
      end if;

   exception
         when File_Execution.Program_Not_Found =>
	    Button := Message_Dialog
	      (Msg => "Warning: "&Message.all&
		 ASCII.LF&
		 "Check that the program is configured in the PATH",
	       Dialog_Type => Information,
	       Buttons => Button_OK, -- Button_Help
	       Default_Button => Button_OK,
	       -- Help button displays a window with no parent, which causes an
	       -- error message
               -- Help_Msg => 
	       --   "Check that the program is configured in the PATH",
	       Parent => Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window));
   end On_Analysis_Activate;

   ----------------------------
   -- On_Simulation_Activate --
   ----------------------------

   procedure On_Simulation_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Simulation_Activate;

   ------------------------------
   -- On_View_Results_Activate --
   ------------------------------

   procedure On_View_Results_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
      Success : Boolean;
      Button : Message_Dialog_Buttons;
   begin
      -- Invoke Results Viewer
      File_Execution.Execute_File("mast_results_command",Success);

   exception
         when File_Execution.Program_Not_Found =>
	    Button := Message_Dialog
	      (Msg => "Warning: gmast_results viewer or mast_results_command"&
		 " file not found"&
		 ASCII.LF&
		 "Check that the program is configured in the PATH",
	       Dialog_Type => Information,
	       Buttons => Button_OK, -- Button_Help
	       Default_Button => Button_OK,
	       -- Help button displays a window with no parent, which causes an
	       -- error message
               -- Help_Msg => 
		 -- "Check that the program is configured in the PATH",
	       Parent => Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window));
   end On_View_Results_Activate;

   -----------------------
   -- On_About_Activate --
   -----------------------

   procedure On_About_Activate (Object : access Gtk_Menu_Item_Record'Class) is
      Button : Message_Dialog_Buttons;
   begin
      Button :=
        Message_Dialog
        (Msg => "Modeling and Analysis Suite for Real Time Applications"&
	   " (MAST) GUI"
           &
           ASCII.LF &
           ASCII.LF &
           "(gMAST version " &
           Mast_Editor.Gmasteditor_Version &
           ")" &
           ASCII.LF &
           ASCII.LF &
           "Designed for MAST version " &
           Mast.Version_String &
           ASCII.LF &
           ASCII.LF & "Created by: Pilar Del Rio Trueba. " &
           "Changed by: Felisa Hidalgo. Universidad de Cantabria, Spain",
	 Dialog_Type => Information,
	 Buttons => Button_OK, -- Button_Help
	 Default_Button => Button_OK,
	 -- Help button displays a window with no parent, which causes an
	 -- error message
         --Help_Msg => "Please see the Mast documentation" &
           --ASCII.LF &
           --ASCII.LF &
           --"Click on the OK button to close this window.",
         Title    => " About gMAST version " &
           Mast_Editor.Gmasteditor_Version &
           " ",
	 Parent => Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window));
   end On_About_Activate;

   ---------------------------------
   -- On_Main_Window_Delete_Event --
   ---------------------------------

   function On_Main_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
     return   Boolean
   is
      Arg1                : Gdk_Event := To_Event (Params, 1);
      Save_Changes_Dialog : Save_Changes_Dialog_Access;
   begin
      if Change_Control.Saved_Changes then
         Gtk.Main.Main_Quit;
      else
         Present (Mast_Editor_Window);
         Gtk_New (Save_Changes_Dialog);
         Show_All (Save_Changes_Dialog);
         Save_Changes_Dialog.Quit:=True;
      end if;
      return True;
   end On_Main_Window_Delete_Event;

end Mast_Editor_Window_Pkg.Callbacks;
