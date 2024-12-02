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
--           Michael Gonzalez Harbour                                --
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
with Ada.Tags;       use Ada.Tags;
with Ada.Text_IO;    use Ada.Text_IO;
with Gdk.RGBA;      use Gdk.RGBA;
with Pango.Font;     use Pango.Font;
with Pango.Layout;   use Pango.Layout;
with Pango.Cairo; use Pango.Cairo;
with Pango.Enums; use Pango.Enums;
with Cairo;          use Cairo;
with Cairo.Region; use Cairo.Region;
with Gdk.Cairo; use Gdk.Cairo;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.GEntry;     use Gtk.GEntry;
with Gtk.Handlers;   use Gtk.Handlers;
with Gtk.Label;      use Gtk.Label;
with Gtk.Table;      use Gtk.Table;
with Gtk.Widget;     use Gtk.Widget;
with Gtk.Window;     use Gtk.Window;
with Pango.Font;     use Pango.Font;
with Gtkada.Dialogs; use Gtkada.Dialogs;
with Utilities; use Utilities;
with List_Exceptions;                   use List_Exceptions;
with Mast;                              use Mast;
with Mast.IO;                           use Mast.IO;
with Mast.Processing_Resources.Network; use Mast.Processing_Resources.Network;
with Mast.Operations;                   use Mast.Operations;
with Mast.Scheduling_Servers;           use Mast.Scheduling_Servers;
with Mast_Editor_Window_Pkg;            use Mast_Editor_Window_Pkg;
with Mast_Editor.Processing_Resources;  use Mast_Editor.Processing_Resources;
with Editor_Error_Window_Pkg;           use Editor_Error_Window_Pkg;
with Item_Menu_Pkg;                     use Item_Menu_Pkg;
with Driver_Dialog_Pkg;                 use Driver_Dialog_Pkg;
with Editor_Actions;                    use Editor_Actions;
with Change_Control;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Exceptions;                    use Ada.Exceptions;

package body Mast_Editor.Drivers is

   Verbose : constant Boolean := True; --...
   
   function Remove_Driver_Link
     (Canvas: access Interactive_Canvas_Record'Class;
      Link: access Canvas_Link_Record'Class)
     return Boolean 
   is
   begin
      Remove_Link(Canvas, Link);
      return True; -- continue traversing links
   end Remove_Driver_Link;
   
   Remove_Processor : Link_Processor := 
     Remove_Driver_Link'Access;
   
   package Button_Cb is new Gtk.Handlers.User_Callback 
     (Widget_Type => Gtk_Widget_Record,
      User_Type => ME_Driver_Ref);

   Zoom_Levels : constant array (Positive range <>) of Guint  :=
     (100,
      130,
      150);
   Font        : Pango_Font_Description;
   Font1       : Pango_Font_Description;
   Font_Size   : Gint;
   Line_Height : Gdouble;
   
   NL : String:=""&Ada.Characters.Latin_1.CR &
     Ada.Characters.Latin_1.LF;

   -------------------------------------------------
   -- Types and packages used to handle dialogs info
   -------------------------------------------------

   type ME_Driver_And_Dialog is record
      It  : ME_Driver_Ref;
      Dia : Gtk_Dialog;
   end record;

   type ME_Driver_And_Dialog_Ref is access all ME_Driver_And_Dialog;

   package Me_Driver_And_Dialog_Cb is new Gtk.Handlers.User_Callback 
     (Widget_Type => Gtk_Widget_Record,
      User_Type => ME_Driver_And_Dialog_Ref);

   --------------
   -- Name     --
   --------------
   function Name (Item : in ME_Driver) return Var_String is
   begin
      return (Name (Item.Proc) & Delimiter & Name (Item.Net));
   end Name;

   --------------
   -- Name     --
   --------------
   function Name (Item_Ref : in ME_Driver_Ref) return Var_String is
   begin
      return (Name (Item_Ref.Proc) & Delimiter & Name (Item_Ref.Net));
   end Name;

   -----------------
   -- Print       --
   -----------------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      Item        : in out ME_Driver;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Ada.Text_IO.Set_Col (File, Ada.Text_IO.Count (Indentation));
      Ada.Text_IO.Put (File, "ME_Driver");
   end Print;

   -----------------
   -- Print       --
   -----------------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      The_List    : in out Lists.List;
      Indentation : Positive)
   is
      Item_Ref : ME_Driver_Ref;
      Iterator : Lists.Index;
   begin
      Lists.Rewind (The_List, Iterator);
      for I in 1 .. Lists.Size (The_List) loop
         Lists.Get_Next_Item (Item_Ref, The_List, Iterator);
         Print (File, Item_Ref.all, Indentation, True);
         Ada.Text_IO.New_Line (File);
      end loop;
   end Print;

   ----------------------
   -- Write Parameters --
   ----------------------
   -- Get the data from the dialog and write it into a new driver, deleting
   -- the old one
   -- Used when selecting the "Properties" of the driver and completing the
   -- driver dialog
   procedure Write_Parameters
     (Item   : access ME_Packet_Driver;
      Dialog : access Gtk_Dialog_Record'Class)
   is

      function Server_Reference
        (Combo : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text)
        return  Scheduling_Server_Ref
      is
         Serv_Name  : Var_Strings.Var_String;
         Serv_Index : Mast.Scheduling_Servers.Lists.Index;
         Serv_Ref   : Mast.Scheduling_Servers.Scheduling_Server_Ref;
      begin
         Serv_Name  := To_Var_String (Get_Active_Text (Combo));
         Serv_Index :=
           Mast.Scheduling_Servers.Lists.Find
           (Serv_Name,
            The_System.Scheduling_Servers);
         Serv_Ref   :=
           Mast.Scheduling_Servers.Lists.Item
           (Serv_Index,
            The_System.Scheduling_Servers);
         return Serv_Ref;
      exception
         when Invalid_Index =>
            Serv_Ref := null;
            return Serv_Ref;
      end Server_Reference;

      function Operation_Reference
        (Combo : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text)
        return  Operation_Ref
      is
         Op_Name  : Var_Strings.Var_String;
         Op_Index : Mast.Operations.Lists.Index;
         Op_Ref   : Mast.Operations.Operation_Ref;
      begin
         Op_Name  := To_Var_String (Get_Active_Text (Combo));
         Op_Index :=
           Mast.Operations.Lists.Find (Op_Name, The_System.Operations);
         Op_Ref   :=
           Mast.Operations.Lists.Item (Op_Index, The_System.Operations);
         return Op_Ref;
      exception
         when Invalid_Index =>
            Op_Ref := null;
            return Op_Ref;
      end Operation_Reference;
      
      function Driver_Exists
	(Item : access ME_Packet_Driver;
	 This_Proc_Name : Var_String) 
	return boolean
      is
	 This_Driv_Ref : Driver_Ref :=
	   ME_Driver_Ref (Item).Driv;
	 Net_Ref : Mast.Processing_Resources.Processing_Resource_Ref :=
	   Item.Net;
	 Drive_Iterator :
	   Mast.Processing_Resources.Network.Driver_Iteration_Object;
	 Drive_Ref : Mast.Drivers.Driver_Ref;
	 Serv_Ref : Mast.Scheduling_Servers.Scheduling_Server_Ref;
      begin
	 Mast.Processing_Resources.Network.Rewind_Drivers
	   (Mast.Processing_Resources.Network.
	      Packet_Based_Network(Net_Ref.all),
	    Drive_Iterator);
	 for I in
	   1 .. Mast.Processing_Resources.Network.Num_Of_Drivers
	   (Mast.Processing_Resources.Network.Packet_Based_Network
	      (Net_Ref.all))
	 loop
	    Mast.Processing_Resources.Network.Get_Next_Driver
	      (Mast.Processing_Resources.Network.Packet_Based_Network
		 (Net_Ref.all),
	       Drive_Ref,
	       Drive_Iterator);
	    -- Ignore current driver
	    if Drive_Ref /= This_Driv_Ref and Drive_Ref /= null then
	       Serv_Ref := Packet_Driver(Drive_Ref.all).Packet_Server;
	       if Serv_Ref/=null and then
		 This_Proc_Name = Serv_Ref.Server_Processing_Resource.Name 
	       then
		  -- duplicate found
		  return True;
	       end if;
	    end if;
	 end loop;
	 return False;
      end Driver_Exists;      
            
      Driv_Ref      : Driver_Ref                                        :=
        ME_Driver_Ref (Item).Driv;
      Driver_Dialog : Driver_Dialog_Access                              :=
        Driver_Dialog_Access (Dialog);
      Net_Ref       : Mast.Processing_Resources.Processing_Resource_Ref :=
        Item.Net;
      New_Proc_Name : Var_String;
      Serv_Ref       : Mast.Scheduling_Servers.Scheduling_Server_Ref;
      Send_Op_Ref   : Mast.Operations.Operation_Ref;
      Recv_Op_Ref   : Mast.Operations.Operation_Ref;
      Ovhd_Model : Rta_Overhead_Model_Type;
      Me_Proc_Iter, Me_Net_Iter :
	Mast_Editor.Processing_Resources.Lists.Iteration_Object;
      Me_Proc_Ref :
	Mast_Editor.Processing_Resources.ME_Processing_Resource_Ref;
      
   begin
      -- Check packet server
      Serv_Ref := Server_Reference (Driver_Dialog.Packet_Server_Combo);
      New_Proc_Name := Serv_Ref.Server_Processing_Resource.Name;
      if Serv_Ref = null then
	 -- Driver cannot be without packet server
         Gtk_New (Editor_Error_Window, Gtk_Window(Driver_Dialog));
         Set_Text
           (Editor_Error_Window.Label,
            "Error: Driver has no Packet Server");
         Show_All (Editor_Error_Window);
	 return;
      end if;
      
      -- Check send operation
      Send_Op_Ref := Operation_Reference (Driver_Dialog.Packet_Send_Op_Combo);
      if Send_Op_Ref = null then
	 -- Driver cannot be without send operation
         Gtk_New (Editor_Error_Window, Gtk_Window(Driver_Dialog));
         Set_Text
           (Editor_Error_Window.Label,
            "Error: Driver has no Packet Send Operation");
         Show_All (Editor_Error_Window);
	 return;
      end if;
      
      -- Check receive operation
      Recv_Op_Ref := Operation_Reference (Driver_Dialog.Packet_Rece_Op_Combo);
      if Recv_Op_Ref = null then
	 -- Driver cannot be without receive operation
         Gtk_New (Editor_Error_Window, Gtk_Window(Driver_Dialog));
         Set_Text
           (Editor_Error_Window.Label,
            "Error: Driver has no Packet Receive Operation");
         Show_All (Editor_Error_Window);
	 return;
      end if;
      
      -- Check Rta_Overhead_Model
      begin
	 Ovhd_Model := Rta_Overhead_Model_Type'Value
	      (Get_Active_Text (Driver_Dialog.Rta_Overhead_Model_Combo));
      exception
	 when Constraint_Error =>
	    -- Driver Rta_Overhead_Model is wrong
	    Gtk_New (Editor_Error_Window, Gtk_Window(Driver_Dialog));
	    Set_Text
	      (Editor_Error_Window.Label,
	       "Error: Driver RTA Overhead Model has not been properly set");
	    Show_All (Editor_Error_Window);
	    return;
      end;
            
      -- Check that this is not a duplicate driver between the 
      -- same network and processor
      if Driver_Exists(Item, New_Proc_Name) then
	 -- A duplicate driver was found
	 Gtk_New (Editor_Error_Window, Gtk_Window(Driver_Dialog));
	 Set_Text
	   (Editor_Error_Window.Label,
	    "Error: Duplicate Driver" & ASCII.LF &
	      "A Driver for network "& To_String(Item.Net.Name) & 
	      " and processor " & To_String(New_Proc_Name) & " already exists");
	 Show_All (Editor_Error_Window);
	 return;
      end if;            
      
      Change_Control.Changes_Made;
      -- Remove previous driver_ref from drivers list of the network
      Mast.Processing_Resources.Network.Remove_Driver
        (Mast.Processing_Resources.Network.Packet_Based_Network (Net_Ref.all),
         Driv_Ref);

      if (Get_Active_Text (Driver_Dialog.Driver_Type_Combo) =
            "Packet Driver")
      then
         Driv_Ref := new Packet_Driver;
      elsif (Get_Active_Text (Driver_Dialog.Driver_Type_Combo) =
               "Character Packet Driver")
      then
         Driv_Ref := new Character_Packet_Driver;
      elsif (Get_Active_Text (Driver_Dialog.Driver_Type_Combo) =
               "RT-EP Packet Driver")
      then
         Driv_Ref := new RTEP_Packet_Driver;
      end if;

      Set_Packet_Server
        (Packet_Driver (Driv_Ref.all), Serv_Ref);
      Set_Packet_Send_Operation
        (Packet_Driver (Driv_Ref.all), Send_Op_Ref);
      Set_Packet_Receive_Operation
        (Packet_Driver (Driv_Ref.all), Recv_Op_Ref);
      if Get_Active_Text (Driver_Dialog.Message_Partitioning_Combo) =
        "YES"
      then
         Set_Message_Partitioning (Packet_Driver (Driv_Ref.all), True);
      else
         Set_Message_Partitioning (Packet_Driver (Driv_Ref.all), False);
      end if;
      Set_Rta_Overhead_Model
        (Packet_Driver (Driv_Ref.all), Ovhd_Model);

      if Driv_Ref.all'Tag = Character_Packet_Driver'Tag then
         -- Character Packet Driver
         Set_Character_Server
           (Character_Packet_Driver (Driv_Ref.all),
            Server_Reference (Driver_Dialog.Char_Server_Combo));
         Set_Character_Send_Operation
           (Character_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Char_Send_Op_Combo));
         Set_Character_Receive_Operation
           (Character_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Char_Rece_Op_Combo));
         Set_Character_Transmission_Time
           (Character_Packet_Driver (Driv_Ref.all),
            Time'Value (Get_Text (Driver_Dialog.Char_Tx_Time_Entry)));
      elsif Driv_Ref.all'Tag = RTEP_Packet_Driver'Tag then
         -- RTEP Packet Driver
         Set_Number_Of_Stations
           (RTEP_Packet_Driver (Driv_Ref.all),
            Positive'Value (Get_Text (Driver_Dialog.Num_Of_Stations_Entry)));
         Set_Token_Delay
           (RTEP_Packet_Driver (Driv_Ref.all),
            Time'Value (Get_Text (Driver_Dialog.Token_Delay_Entry)));
         Set_Failure_Timeout
           (RTEP_Packet_Driver (Driv_Ref.all),
            Time'Value (Get_Text (Driver_Dialog.Failure_Timeout_Entry)));
         Set_Token_Transmission_Retries
           (RTEP_Packet_Driver (Driv_Ref.all),
            Natural'Value
              (Get_Text (Driver_Dialog.Token_Transmission_Retries_Entry)));
         Set_Packet_Transmission_Retries
           (RTEP_Packet_Driver (Driv_Ref.all),
            Natural'Value
              (Get_Text (Driver_Dialog.Packet_Transmission_Retries_Entry)));
         Set_Packet_Interrupt_Server
           (RTEP_Packet_Driver (Driv_Ref.all),
            Server_Reference (Driver_Dialog.Packet_Interrupt_Server_Combo));
         Set_Packet_ISR_Operation
           (RTEP_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Packet_Isr_Op_Combo));
         Set_Token_Check_Operation
           (RTEP_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Token_Check_Op_Combo));
         Set_Token_Manage_Operation
           (RTEP_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Token_Manage_Op_Combo));
         Set_Packet_Discard_Operation
           (RTEP_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Packet_Discard_Op_Combo));
         Set_Token_Retransmission_Operation
           (RTEP_Packet_Driver (Driv_Ref.all),
            Operation_Reference (Driver_Dialog.Token_Retransmission_Op_Combo));
         Set_Packet_Retransmission_Operation
           (RTEP_Packet_Driver (Driv_Ref.all),
            Operation_Reference
              (Driver_Dialog.Packet_Retransmission_Op_Combo));
      end if;

      if Net_Ref /= null then
         Add_Driver
           (Mast.Processing_Resources.Network.Packet_Based_Network 
	      (Net_Ref.all), Driv_Ref);
      end if;
      
      -- Record the new driver
      Item.Driv := Driv_Ref;
      Item.Proc := Packet_Driver(Driv_Ref.all).Packet_Server.
	Server_Processing_Resource;      
      
      -- Obtain processor reference
      Me_Proc_Iter :=
	Mast_Editor.Processing_Resources.Lists.Find
	(Item.Proc.Name,
	 Editor_System.Me_Processing_Resources);
      Me_Proc_Ref  :=
	Mast_Editor.Processing_Resources.Lists.Item
	(Me_Proc_Iter,
	 Editor_System.Me_Processing_Resources);
      
      -- Remove the driver's links to processors
      For_Each_Link 
	(Canvas => Proc_Res_Canvas, 
	 Execute => Remove_Processor,
	 From => Canvas_Item(Item),
	 To => null);
      
      -- Repaint the driver's links to processors
        
      if Me_Proc_Ref /= null then
	 Add_Canvas_Link
	   (Proc_Res_Canvas, Item, Me_Proc_Ref);
      end if;

      Destroy (Driver_Dialog);
      
   end Write_Parameters;

   ---------------------
   -- Read Parameters --
   ---------------------
   -- Get the data from the driver and write it into the dialog
   -- Used when selecting the driver "Properties" to create the driver dialog
   procedure Read_Parameters
     (Item   : access ME_Packet_Driver;
      Dialog : access Gtk_Dialog_Record'Class)
   is

      function Server_Name
        (Serv_Ref : Scheduling_Server_Ref)
        return     Var_String
      is
      begin
         if Serv_Ref = null then
            return To_Var_String ("(NONE)");
         else
            return Mast.Scheduling_Servers.Name (Serv_Ref);
         end if;
      end Server_Name;

      function Operation_Name (Op_Ref : Operation_Ref) return Var_String is
      begin
         if Op_Ref = null then
            return To_Var_String ("(NONE)");
         else
            return Mast.Operations.Name (Op_Ref.all);
         end if;
      end Operation_Name;

      Driv_Ref      : Mast.Drivers.Driver_Ref := Item.Driv;
      Driver_Dialog : Driver_Dialog_Access    :=
        Driver_Dialog_Access (Dialog);
   begin
      if Driv_Ref /= null then
         Set_Text_In_Combo_Box
	   (Driver_Dialog.Packet_Server_Combo,
	    To_String
	      (Server_Name (Packet_Server (Packet_Driver (Driv_Ref.all)))));
         Set_Text_In_Combo_Box
	   (Driver_Dialog.Packet_Send_Op_Combo,
	    To_String
              (Operation_Name
                 (Packet_Send_Operation (Packet_Driver (Driv_Ref.all)))));
         Set_Text_In_Combo_Box
	   (Driver_Dialog.Packet_Rece_Op_Combo,
            To_String
              (Operation_Name
                 (Packet_Receive_Operation (Packet_Driver (Driv_Ref.all)))));
	 
         if Message_Partitioning (Packet_Driver (Driv_Ref.all)) then
            Set_Text_In_Combo_Box 
              (Driver_Dialog.Message_Partitioning_Combo,
               "YES");
         else
            Set_Text_In_Combo_Box
              (Driver_Dialog.Message_Partitioning_Combo,
               "NO");
         end if;
	 begin
	    Set_Text_In_Combo_Box
	      (Driver_Dialog.Rta_Overhead_Model_Combo,
	       Rta_Overhead_Model_Type'Image
		 (Rta_Overhead_Model (Packet_Driver (Driv_Ref.all))));
	 exception
	    when Constraint_Error =>
	       -- We Leave The Rta_Overhead_Model unchanged
	       null;
	 end;
         if Driv_Ref.all'Tag = Packet_Driver'Tag then
            Set_Text_In_Combo_Box
              (Driver_Dialog.Driver_Type_Combo,
               "Packet Driver");
            Show_All (Driver_Dialog);
            Hide (Driver_Dialog.Character_Server_Table);
            Hide (Driver_Dialog.Rtep_Table);
         elsif Driv_Ref.all'Tag = Character_Packet_Driver'Tag then
            Set_Text_In_Combo_Box
              (Driver_Dialog.Driver_Type_Combo,
               "Character Packet Driver");
            Set_Text_In_Combo_Box
              (Driver_Dialog.Char_Server_Combo,
               To_String
                 (Server_Name
                    (Character_Server
                       (Character_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Char_Send_Op_Combo,
               To_String
                 (Operation_Name
                    (Character_Send_Operation
                       (Character_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Char_Rece_Op_Combo,
               To_String
                 (Operation_Name
                    (Character_Receive_Operation
                       (Character_Packet_Driver (Driv_Ref.all)))));
            Set_Text
              (Driver_Dialog.Char_Tx_Time_Entry,
               Time_Image
                 (Character_Transmission_Time
                    (Character_Packet_Driver (Driv_Ref.all))));
            Show_All (Driver_Dialog);
            Hide (Driver_Dialog.Rtep_Table);
         elsif Driv_Ref.all'Tag = RTEP_Packet_Driver'Tag then
            Set_Text_In_Combo_Box
              (Driver_Dialog.Driver_Type_Combo,
               "RT-EP Packet Driver");
            Set_Text
              (Driver_Dialog.Num_Of_Stations_Entry,
               Positive'Image
                 (Number_Of_Stations (RTEP_Packet_Driver (Driv_Ref.all))));
            Set_Text
              (Driver_Dialog.Token_Delay_Entry,
               Time'Image (Token_Delay (RTEP_Packet_Driver (Driv_Ref.all))));
            Set_Text
              (Driver_Dialog.Failure_Timeout_Entry,
               Time'Image
                 (Failure_Timeout (RTEP_Packet_Driver (Driv_Ref.all))));
            Set_Text
              (Driver_Dialog.Token_Transmission_Retries_Entry,
               Natural'Image
                 (Token_Transmission_Retries
                    (RTEP_Packet_Driver (Driv_Ref.all))));
            Set_Text
              (Driver_Dialog.Packet_Transmission_Retries_Entry,
               Natural'Image
                 (Packet_Transmission_Retries
                    (RTEP_Packet_Driver (Driv_Ref.all))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Packet_Interrupt_Server_Combo,
               To_String
                 (Server_Name
                    (Packet_Interrupt_Server
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Packet_Isr_Op_Combo,
               To_String
                 (Operation_Name
                    (Packet_ISR_Operation
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Token_Check_Op_Combo,
               To_String
                 (Operation_Name
                    (Token_Check_Operation
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Token_Manage_Op_Combo,
               To_String
                 (Operation_Name
                    (Token_Manage_Operation
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Packet_Discard_Op_Combo,
               To_String
                 (Operation_Name
                    (Packet_Discard_Operation
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Token_Retransmission_Op_Combo,
               To_String
                 (Operation_Name
                    (Token_Retransmission_Operation
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Set_Text_In_Combo_Box
              (Driver_Dialog.Packet_Retransmission_Op_Combo,
               To_String
                 (Operation_Name
                    (Packet_Retransmission_Operation
                       (RTEP_Packet_Driver (Driv_Ref.all)))));
            Show_All (Driver_Dialog);
            Hide (Driver_Dialog.Character_Server_Table);
         end if;
      end if;
      Driver_Dialog.Resize(600, 276);
      Driver_Dialog.Set_Modal(True);
   end Read_Parameters;

   -----------------
   -- Draw Driver --
   -----------------
   procedure Draw
     (Item   : access ME_Packet_Driver;
      Cr     : Cairo_Context)
   is
      Rect      : constant Cairo.Region.Cairo_Rectangle_Int  := 
	Get_Coord (Item);
      W         : constant Gdouble        :=Gdouble(Rect.Width);
      H         : constant Gdouble        :=Gdouble(Rect.Height);
      Color,Black : Gdk_RGBA;
      Layout : Pango.Layout.Pango_Layout;
      Success : Boolean;
      Drive_Ref : Mast.Drivers.Driver_Ref := Item.Driv;
      Text : Unbounded_String;
   begin
      Editor_Actions.Load_System_Font (Font, Font1);
      Layout := Mast_Editor_Window.Create_Pango_Layout;
      Set_Font_Description (Layout, Font);
      Font_Size:=Get_Size(Font);
      Line_Height:=Gdouble(Layout.Get_Baseline/Pango_Scale);

      Parse (Color, To_String (ME_Driver_Ref (Item).Color_Name),Success);
      Black := (0.0, 0.0, 0.0, 1.0);
      Item.X_Coord := Gint (Get_Coord (Item).X);
      Item.Y_Coord := Gint (Get_Coord (Item).Y);
      
      Gdk.Cairo.Set_Source_RGBA (Cr, Color);
      Cairo.Rectangle
        (Cr, 0.5, 0.5, Gdouble (W) - 1.0, Gdouble (H) - 1.0);
      Cairo.Fill (Cr);

      Gdk.Cairo.Set_Source_RGBA (Cr, Black);
      Rectangle
        (Cr, 0.5, 0.5, Gdouble (W) - 1.0, Gdouble (H) - 1.0);
      Cairo.Stroke (Cr);

      if Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (1) then
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(1)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(1)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 Set_Font_Description (Layout, Font1);
	 
         if Drive_Ref /= null then
            if Drive_Ref.all'Tag = Packet_Driver'Tag then
	       Set_Text (Layout,"PACKET"&NL&"DRIVER"); 
	       Cairo.Move_To (Cr, 0.0,2.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
	       
            elsif Drive_Ref.all'Tag = Character_Packet_Driver'Tag then
	       Set_Text (Layout,"CHARACTER"&NL&"PACKET"&NL&"DRIVER");
	       Cairo.Move_To (Cr, 0.0,2.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
	       	       
            elsif Drive_Ref.all'Tag = RTEP_Packet_Driver'Tag then
	       Set_Text (Layout,"RT_EP"&NL&"PACKET"&NL&"DRIVER");
	       Cairo.Move_To (Cr, 0.0, 2.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
            end if;
         end if;
      elsif Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (2)
      then
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(2)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(2)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 Set_Font_Description (Layout, Font1);
	 
         if Drive_Ref /= null then
            if Drive_Ref.all'Tag = Packet_Driver'Tag then
	       Set_Text (Layout,"PACKET"&NL&"DRIVER"); 
	       Cairo.Move_To (Cr, 0.0,6.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
	       
            elsif Drive_Ref.all'Tag = Character_Packet_Driver'Tag then
	       Set_Text (Layout,"CHARACTER"&NL&"PACKET"&NL&"DRIVER");
	       Cairo.Move_To (Cr, 0.0,6.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
	       	       
            elsif Drive_Ref.all'Tag = RTEP_Packet_Driver'Tag then
	       Set_Text (Layout,"RT_EP"&NL&"PACKET"&NL&"DRIVER");
	       Cairo.Move_To (Cr, 0.0, 6.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
            end if;
         end if;
      else
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(3)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(3)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 Set_Font_Description (Layout, Font1);
	 
         if Drive_Ref /= null then
            if Drive_Ref.all'Tag = Packet_Driver'Tag then
	       Set_Text (Layout,"PACKET"&NL&"DRIVER"); 
	       Cairo.Move_To (Cr, 0.0,9.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
	       
            elsif Drive_Ref.all'Tag = Character_Packet_Driver'Tag then
	       Set_Text (Layout,"CHARACTER"&NL&"PACKET"&NL&"DRIVER");
	       Cairo.Move_To (Cr, 0.0,9.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
	       	       
            elsif Drive_Ref.all'Tag = RTEP_Packet_Driver'Tag then
	       Set_Text (Layout,"RT_EP"&NL&"PACKET"&NL&"DRIVER");
	       Cairo.Move_To (Cr, 0.0, 9.0);
 	       Pango.Cairo.Show_Layout (Cr, Layout);
            end if;
         end if;
      end if;
   end Draw;

   ------------------
   -- Write Driver -- 
   -- (Write the params of an existing driver and refresh the
   --  canvas)
   ------------------
   procedure Write_Driver
     (Widget : access Gtk_Widget_Record'Class;
      Data   : ME_Driver_And_Dialog_Ref)
   is
      Item          : ME_Driver_Ref        := Data.It;
      Driver_Dialog : Driver_Dialog_Access := Driver_Dialog_Access (Data.Dia);
	
   begin
      Write_Parameters (Item, Gtk_Dialog (Driver_Dialog));
      
      
      Refresh_Canvas (Proc_Res_Canvas);

   exception
      when E: Constraint_Error =>
         Gtk_New (Editor_Error_Window);
	 if Verbose then
	    Set_Text 
	      (Editor_Error_Window.Label, 
	       "Invalid Value !!!" & ASCII.LF&
		 Exception_Information(E));
	 else
	    Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
	 end if;
         Show_All (Editor_Error_Window);
         Destroy (Driver_Dialog);
      when E : others =>
         Gtk_New (Editor_Error_Window);
         Set_Text 
	   (Editor_Error_Window.Label,
	    Ada.Exceptions.Exception_Information(E)&
	    ASCII.LF&
	    "ERROR WRITING DRIVER !!!");
         Show_All (Editor_Error_Window);
         Destroy (Driver_Dialog);
   end Write_Driver;

   --------------------
   -- Remove_Driver  --
   --------------------
   procedure Remove_Driver
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Driver_Ref)
   is
      Driv_Ref           : Driver_Ref := ME_Driver_Ref (Item).Driv;
      Driver_Name        : Var_String;
      Me_Driver_Iterator : Mast_Editor.Drivers.Lists.Iteration_Object;
      Me_Driver_Ref      : Mast_Editor.Drivers.ME_Driver_Ref;
      Net_Ref            : Mast.Processing_Resources.Processing_Resource_Ref
        :=
        Item.Net;

   begin
      if Message_Dialog
        (Msg => " Do you really want to remove this object? ",
         Dialog_Type => Confirmation,
         Buttons => Button_Yes or Button_No,
         Default_Button => Button_Yes,
	 Parent => Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window)) =
        Button_Yes
      then
         Driver_Name := Name (Item);
         if Net_Ref /= null then
            Mast.Processing_Resources.Network.Remove_Driver
              (Mast.Processing_Resources.Network.Packet_Based_Network 
		 (Net_Ref.all), Driv_Ref);
         end if;

         Me_Driver_Iterator :=
           Mast_Editor.Drivers.Lists.Find
           (Driver_Name,
            Editor_System.Me_Drivers);
         Me_Driver_Ref      :=
           Mast_Editor.Drivers.Lists.Item
           (Me_Driver_Iterator,
            Editor_System.Me_Drivers);
         Mast_Editor.Drivers.Lists.Delete
           (Me_Driver_Iterator,
            Me_Driver_Ref,
            Editor_System.Me_Drivers);

         Remove (Proc_Res_Canvas, Me_Driver_Ref);
         Refresh_Canvas (Proc_Res_Canvas);
         Change_Control.Changes_Made;
         Destroy (Item_Menu);
      end if;
   exception
      when others =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "ERROR IN DRIVER REMOVAL !!!");
         Show_All (Editor_Error_Window);
         Destroy (Item_Menu);
   end Remove_Driver;

   -----------------------
   -- Properties_Driver --
   -----------------------
   procedure Properties_Driver
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Driver_Ref)
   is
      Driver_Dialog : Driver_Dialog_Access;
      Me_Data       : ME_Driver_And_Dialog_Ref := new ME_Driver_And_Dialog;
   begin
      Gtk_New (Driver_Dialog);
      --Set_Modal (Driver_Dialog, False);
      --Driver_Dialog.Resize(600, 276);
      Read_Parameters (Item, Gtk_Dialog (Driver_Dialog));
      Me_Data.It  := Item;
      Me_Data.Dia := Gtk_Dialog (Driver_Dialog);

      Me_Driver_And_Dialog_Cb.Connect
        (Driver_Dialog.Driver_Ok_Button,
         "clicked",
         Me_Driver_And_Dialog_Cb.To_Marshaller (Write_Driver'Access),
         Me_Data);

      Refresh_Canvas (Proc_Res_Canvas);
      Destroy (Item_Menu);
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Error Reading Driver !!!");
         Show_All (Editor_Error_Window);
   end Properties_Driver;

   ---------------------
   -- On Button Click --
   ---------------------
   function On_Button_Click
     (Item  : access ME_Packet_Driver;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      Num_Button    : Guint;
      Event_Type    : Gdk_Event_Type;
      Driver_Dialog : Driver_Dialog_Access;
      Me_Data       : ME_Driver_And_Dialog_Ref := new ME_Driver_And_Dialog;
   begin
      Event_Type := Event.The_Type;
      if Event_Type = Gdk_2button_Press then
	 Num_Button := Event.Button;
	 if Num_Button = Guint (1) then
	    Gtk_New (Driver_Dialog);
	    Set_Modal (Driver_Dialog, False);
	    Read_Parameters (Item, Gtk_Dialog (Driver_Dialog));
	    Me_Data.It  := ME_Driver_Ref (Item);
	    Me_Data.Dia := Gtk_Dialog (Driver_Dialog);

	    Me_Driver_And_Dialog_Cb.Connect
	      (Driver_Dialog.Driver_Ok_Button,
	       "clicked",
	       Me_Driver_And_Dialog_Cb.To_Marshaller (Write_Driver'Access),
	       Me_Data);
	    return True;
	 end if;
      elsif Event_Type = Button_Press then
	 Num_Button := Event.Button;
	 if Num_Button = Guint (3) then
	    Gtk_New (Item_Menu);
	    Button_Cb.Connect
	      (Item_Menu.Remove,
	       "activate",
	       Button_Cb.To_Marshaller (Remove_Driver'Access),
	       ME_Driver_Ref (Item));
	    Button_Cb.Connect
	      (Item_Menu.Properties,
	       "activate",
	       Button_Cb.To_Marshaller (Properties_Driver'Access),
	       ME_Driver_Ref (Item));
	    return True;
	 end if;
      end if;
      return False;
   end On_Button_Click;

   -----------
   -- Print --
   -----------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      Item        : in out ME_Packet_Driver;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Mast_Editor.Drivers.Print (File, ME_Driver (Item), Indentation);
      Put (File, " ");
      Put (File, "Me_Packet_Driver");
      Put (File, " ");
      Put (File, To_String (Name (Item)));
      Put (File, " ");
      Put (File, To_String (Item.Canvas_Name));
      if Item.X_Coord < 0 then
         Put (File, " ");
      end if;
      Put (File, Gint'Image (Item.X_Coord));
      if Item.Y_Coord < 0 then
         Put (File, " ");
      end if;
      Put (File, Gint'Image (Item.Y_Coord));
      Put (File, " ");
   end Print;

end Mast_Editor.Drivers;
