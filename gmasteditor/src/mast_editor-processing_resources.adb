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
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Tags;                use Ada.Tags;
with Callbacks_Mast_Editor;   use Callbacks_Mast_Editor;
with Cairo;                   use Cairo;
with Gdk.RGBA;      use Gdk.RGBA;
with Pango.Font;     use Pango.Font;
with Pango.Layout;   use Pango.Layout;
with Pango.Enums; use Pango.Enums;
with Pango.Cairo; use Pango.Cairo;
with Cairo;          use Cairo;
with Cairo.Region; use Cairo.Region;
with Gdk.Cairo; use Gdk.Cairo;
with Gtk.Alignment;           use Gtk.Alignment;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Combo_Box_Text;      use Gtk.Combo_Box_Text;
with Gtk.Frame;               use Gtk.Frame;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Label;               use Gtk.Label;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Table;               use Gtk.Table;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Tree_Selection;      use Gtk.Tree_Selection;
with Gtk.Tree_Store;          use Gtk.Tree_Store;
with Gtk.Tree_View;           use Gtk.Tree_View;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Canvas;           use Gtkada.Canvas;
with Pango.Font;              use Pango.Font;
with Gtkada.Dialogs;          use Gtkada.Dialogs;
with Utilities; use Utilities;
with Mast_Editor_Window_Pkg;   use Mast_Editor_Window_Pkg;

with List_Exceptions;                     use List_Exceptions;
with Mast;                                use Mast;
with Mast.Drivers;                        use Mast.Drivers;
with Mast.IO;                             use Mast.IO;
with Mast.Operations;                     use Mast.Operations;
with Mast.Processing_Resources;           use Mast.Processing_Resources;
with Mast.Processing_Resources.Processor;
use Mast.Processing_Resources.Processor;

with Mast.Processing_Resources.Network;
use Mast.Processing_Resources.Network;

with Mast.Schedulers;
with Mast.Scheduling_Servers;             use Mast.Scheduling_Servers;
with Mast.Timers;                         use Mast.Timers;
with Mast_Editor.Timers;                  use Mast_Editor.Timers;
with Mast_Editor.Drivers;                 use Mast_Editor.Drivers;
with Mast_Editor.Schedulers;              use Mast_Editor.Schedulers;
with Driver_Dialog_Pkg;                   use Driver_Dialog_Pkg;
with Processor_Dialog_Pkg;                use Processor_Dialog_Pkg;
with Network_Dialog_Pkg;                  use Network_Dialog_Pkg;
with Network_Dialog_Pkg.Callbacks;        use Network_Dialog_Pkg.Callbacks;
with Editor_Error_Window_Pkg;             use Editor_Error_Window_Pkg;
with Item_Menu_Pkg;                       use Item_Menu_Pkg;
with Editor_Actions;                      use Editor_Actions;
with Change_Control;
with Cut_Strings;                         --use Cut_Strings;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;

package body Mast_Editor.Processing_Resources is

   Verbose : constant Boolean := True;
   
   package Canvas_Cb is new Gtk.Handlers.Callback
     (Interactive_Canvas_Record);

   package Button_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type => ME_Processing_Resource_Ref);

   Zoom_Levels : constant array (Positive range <>) of Guint  :=
     (100,
      130,
      150);
   
   Font        : Pango_Font_Description;
   Font1       : Pango_Font_Description;
   P_Layout    : Pango.Layout.Pango_Layout;
   Font_Size   : Gint;
   Line_Height : Gdouble;
   
   NL : String:=""&Ada.Characters.Latin_1.CR &
     Ada.Characters.Latin_1.LF;
   
   -------------------------------------------------
   -- Types and packages used to handle dialogs info
   -------------------------------------------------
   type Network_Args is record
      It  : ME_Processing_Resource_Ref;
      Net : Processing_Resource_Ref;
      Dia : Gtk_Dialog;
   end record;

   type Network_Args_Ref is access all Network_Args;

   package Network_Args_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type => Network_Args_Ref);

   type Processing_Resource_And_Dialog is record
      It  : Processing_Resource_Ref;
      Dia : Gtk_Dialog;
   end record;

   type Processing_Resource_And_Dialog_Ref is access all
     Processing_Resource_And_Dialog;

   package Processing_Resource_And_Dialog_Cb is new Gtk.Handlers.User_Callback
     (
      Widget_Type => Gtk_Widget_Record,
      User_Type => Processing_Resource_And_Dialog_Ref);

   type Processing_Resource_And_Dialog2 is record
      It   : Processing_Resource_Ref;
      Dia  : Gtk_Dialog;
      Dia2 : Gtk_Dialog;
   end record;

   type Processing_Resource_And_Dialog2_Ref is access all
     Processing_Resource_And_Dialog2;

   package Processing_Resource_And_Dialog2_Cb is new
     Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type => Processing_Resource_And_Dialog2_Ref);

   type ME_Processing_Resource_And_Dialog is record
      It  : ME_Processing_Resource_Ref;
      Dia : Gtk_Dialog;
   end record;

   type ME_Processing_Resource_And_Dialog_Ref is access all
     ME_Processing_Resource_And_Dialog;

   package Me_Processing_Resource_And_Dialog_Cb is new
     Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type => ME_Processing_Resource_And_Dialog_Ref);

   --------------
   -- Name     --
   --------------
   function Name (Item : in ME_Processing_Resource) return Var_String is
   begin
      return Name (Item.Res);
   end Name;

   --------------
   -- Name     --
   --------------
   function Name
     (Item_Ref : in ME_Processing_Resource_Ref)
     return     Var_String
   is
   begin
      return Name (Item_Ref.Res);
   end Name;

   -----------------
   -- Print       --
   -----------------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      Item        : in out ME_Processing_Resource;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Ada.Text_IO.Set_Col (File, Ada.Text_IO.Count (Indentation));
      Ada.Text_IO.Put (File, "ME_Processing_Resource");
   end Print;

   -----------------
   -- Print       --
   -----------------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      The_List    : in out Lists.List;
      Indentation : Positive)
   is
      Item_Ref : ME_Processing_Resource_Ref;
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
   procedure Write_Parameters
     (Item   : access ME_Regular_Processor;
      Dialog : access Gtk_Dialog_Record'Class)
   is
      Proc_Res         : Processing_Resource_Ref :=
        ME_Processing_Resource_Ref (Item).Res;
      Proc             : Mast.Processing_Resources.Processor.Regular_Processor;
      Timer_Ref        : System_Timer_Ref;
      Processor_Dialog : Processor_Dialog_Access :=
        Processor_Dialog_Access (Dialog);
   begin
      Change_Control.Changes_Made;
      Init
        (Proc_Res.all,
         Var_Strings.To_Lower
           (To_Var_String (Get_Text (Processor_Dialog.Proc_Name_Entry))));

      Set_Speed_Factor
        (Proc_Res.all,
         Processor_Speed'Value (Get_Text (Processor_Dialog.Proc_Speed_Entry)));

      Proc :=
        Mast.Processing_Resources.Processor.Regular_Processor (Proc_Res.all);

      Set_Max_Interrupt_Priority
        (Proc,
         Priority'Value (Get_Text (Processor_Dialog.Proc_Max_Int_Pri_Entry)));

      Set_Min_Interrupt_Priority
        (Proc,
         Priority'Value (Get_Text (Processor_Dialog.Proc_Min_Int_Pri_Entry)));

      Set_Worst_ISR_Switch
        (Proc,
         Normalized_Execution_Time'Value
           (Get_Text (Processor_Dialog.Proc_Wor_Isr_Swi_Entry)));

      Set_Avg_ISR_Switch
        (Proc,
         Normalized_Execution_Time'Value
           (Get_Text (Processor_Dialog.Proc_Avg_Isr_Swi_Entry)));

      Set_Best_ISR_Switch
        (Proc,
         Normalized_Execution_Time'Value
           (Get_Text (Processor_Dialog.Proc_Bes_Isr_Swi_Entry)));

      Timer_Ref := The_System_Timer (Proc);
      if (Get_Active_Text (Processor_Dialog.System_Timer_Type_Combo) =
            "(NONE)")
      then
         Timer_Ref := null;
      elsif (Get_Active_Text (Processor_Dialog.System_Timer_Type_Combo) =
               "Ticker")
      then
         Timer_Ref := new Ticker;
         Set_Worst_Overhead
           (Timer_Ref.all,
            Normalized_Execution_Time'Value
              (Get_Text (Processor_Dialog.Proc_Wor_Over_Entry)));
         Set_Avg_Overhead
           (Timer_Ref.all,
            Normalized_Execution_Time'Value
              (Get_Text (Processor_Dialog.Proc_Avg_Over_Entry)));
         Set_Best_Overhead
           (Timer_Ref.all,
            Normalized_Execution_Time'Value
              (Get_Text (Processor_Dialog.Proc_Bes_Over_Entry)));
         Set_Period
           (Ticker (Timer_Ref.all),
            Time'Value (Get_Text (Processor_Dialog.Proc_Period_Entry)));
      elsif (Get_Active_Text (Processor_Dialog.System_Timer_Type_Combo) =
               "Alarm Clock")
      then
         Timer_Ref := new Alarm_Clock;
         Set_Worst_Overhead
           (Timer_Ref.all,
            Normalized_Execution_Time'Value
              (Get_Text (Processor_Dialog.Proc_Wor_Over_Entry)));
         Set_Avg_Overhead
           (Timer_Ref.all,
            Normalized_Execution_Time'Value
              (Get_Text (Processor_Dialog.Proc_Avg_Over_Entry)));
         Set_Best_Overhead
           (Timer_Ref.all,
            Normalized_Execution_Time'Value
              (Get_Text (Processor_Dialog.Proc_Bes_Over_Entry)));
      end if;
      Set_System_Timer (Proc, Timer_Ref);
      Mast.Processing_Resources.Processor.Regular_Processor (Proc_Res.all) :=
        Proc;
   end Write_Parameters;

   ----------------------
   -- Write Parameters --
   ----------------------
   procedure Write_Parameters
     (Item   : access ME_Packet_Based_Network;
      Dialog : access Gtk_Dialog_Record'Class)
   is
      Proc_Res       : Processing_Resource_Ref :=
        ME_Processing_Resource_Ref (Item).Res;
      Net            : Mast.Processing_Resources.Network.Packet_Based_Network;
      Network_Dialog : Network_Dialog_Access   :=
        Network_Dialog_Access (Dialog);
   begin
      Change_Control.Changes_Made;
      Init
        (Proc_Res.all,
         Var_Strings.To_Lower
           (To_Var_String (Get_Text (Network_Dialog.Net_Name_Entry))));
      Set_Speed_Factor
        (Proc_Res.all,
         Processor_Speed'Value (Get_Text (Network_Dialog.Net_Speed_Entry)));
      Net :=
        Mast.Processing_Resources.Network.Packet_Based_Network (Proc_Res.all);
      Set_Transmission_Mode
        (Net,
         Transmission_Kind'Value
           (Get_Active_Text (Network_Dialog.Tx_Kind_Combo)));
      Set_Max_Blocking
        (Net,
         Normalized_Execution_Time'Value
           (Get_Text (Network_Dialog.Net_Max_Blo_Entry)));
      Set_Throughput
        (Net,
         Throughput_Value'Value (Get_Text (Network_Dialog.Throughput_Entry)));
      Set_Max_Packet_Size
        (Net,
         Bit_Count'Value (Get_Text (Network_Dialog.Max_Pack_Size_Entry)));
      Set_Min_Packet_Size
        (Net,
         Bit_Count'Value (Get_Text (Network_Dialog.Min_Pack_Size_Entry)));
      Mast.Processing_Resources.Network.Packet_Based_Network (Proc_Res.all) :=
        Net;
   end Write_Parameters;

   ----------------------
   -- Read Parameters --
   ----------------------
   procedure Read_Parameters
     (Item   : access ME_Regular_Processor;
      Dialog : access Gtk_Dialog_Record'Class)
   is
      Proc_Res         : Processing_Resource_Ref := Item.Res;
      Proc_Name        : String                  :=
        To_String (Name (Proc_Res));
      Proc             : Mast.Processing_Resources.Processor.Regular_Processor;
      Timer_Ref        : Mast.Timers.System_Timer_Ref;
      Processor_Dialog : Processor_Dialog_Access :=
        Processor_Dialog_Access (Dialog);
   begin
      Set_Text (Processor_Dialog.Proc_Name_Entry, Proc_Name);
      Set_Text
        (Processor_Dialog.Proc_Speed_Entry,
         Speed_Image (Speed_Factor (Proc_Res.all)));
      Proc :=
        Mast.Processing_Resources.Processor.Regular_Processor (Proc_Res.all);

      Set_Text
        (Processor_Dialog.Proc_Max_Int_Pri_Entry,
         Priority'Image (Max_Interrupt_Priority (Proc)));
      Set_Text
        (Processor_Dialog.Proc_Min_Int_Pri_Entry,
         Priority'Image (Min_Interrupt_Priority (Proc)));

      Set_Text
        (Processor_Dialog.Proc_Wor_Isr_Swi_Entry,
         Execution_Time_Image (Worst_ISR_Switch (Proc)));

      Set_Text
        (Processor_Dialog.Proc_Avg_Isr_Swi_Entry,
         Execution_Time_Image (Avg_ISR_Switch (Proc)));
      Set_Text
        (Processor_Dialog.Proc_Bes_Isr_Swi_Entry,
         Execution_Time_Image (Best_ISR_Switch (Proc)));
      Timer_Ref := The_System_Timer (Proc);
      if Timer_Ref = null then
         Set_Text_In_Combo_Box
           (Processor_Dialog.System_Timer_Type_Combo,
            "(NONE)");
         Show_All (Processor_Dialog);
         Hide (Processor_Dialog.Timer_Table);
      else
         Set_Text
           (Processor_Dialog.Proc_Wor_Over_Entry,
            Execution_Time_Image (Worst_Overhead (Timer_Ref.all)));
         Set_Text
           (Processor_Dialog.Proc_Avg_Over_Entry,
            Execution_Time_Image (Avg_Overhead (Timer_Ref.all)));
         Set_Text
           (Processor_Dialog.Proc_Bes_Over_Entry,
            Execution_Time_Image (Best_Overhead (Timer_Ref.all)));
         if Timer_Ref.all in Ticker then
            Set_Text_In_Combo_Box
              (Processor_Dialog.System_Timer_Type_Combo,
               "Ticker");
            Set_Text
              (Processor_Dialog.Proc_Period_Entry,
               Time_Image (Period (Ticker (Timer_Ref.all))));
            Show_All (Processor_Dialog);
         elsif Timer_Ref.all in Alarm_Clock then
            Set_Text_In_Combo_Box
              (Processor_Dialog.System_Timer_Type_Combo,
               "Alarm Clock");
            Show_All (Processor_Dialog);
            Hide (Processor_Dialog.Proc_Period_Label);
            Hide (Processor_Dialog.Proc_Period_Entry);
         end if;
      end if;
      if Mast.Processing_Resources.Has_Scheduler (Proc_Res.all) then
         Set_Sensitive (Processor_Dialog.New_Primary_Button, False);
      end if;
   end Read_Parameters;

   ---------------------
   -- Read Parameters --
   ---------------------
   procedure Read_Parameters
     (Item   : access ME_Packet_Based_Network;
      Dialog : access Gtk_Dialog_Record'Class)
   is
      Proc_Res       : Processing_Resource_Ref := Item.Res;
      Net_Name       : String                  :=
        To_String (Name (Proc_Res));
      Net            : Mast.Processing_Resources.Network.Packet_Based_Network;
      Driv_Ref       : Mast.Drivers.Driver_Ref;
      Serv_Ref       : Mast.Scheduling_Servers.Scheduling_Server_Ref;
      Proc_Ref       : Mast.Processing_Resources.Processing_Resource_Ref;
      Iterator       : Driver_Iteration_Object;
      Network_Dialog : Network_Dialog_Access   :=
        Network_Dialog_Access (Dialog);
      Parent         : Gtk_Tree_Iter           := Null_Iter;
      Iter           : Gtk_Tree_Iter;
   begin
      Set_Text (Network_Dialog.Net_Name_Entry, Net_Name);


      Set_Text
        (Network_Dialog.Net_Speed_Entry,
         Speed_Image (Speed_Factor (Proc_Res.all)));

      Net :=
        Mast.Processing_Resources.Network.Packet_Based_Network (Proc_Res.all);

      Set_Text_In_Combo_Box
        (Network_Dialog.Tx_Kind_Combo,
         Transmission_Kind'Image (Transmission_Mode (Net)));

      Set_Text
        (Network_Dialog.Net_Max_Blo_Entry,
         Execution_Time_Image (Max_Blocking (Net)));

      Set_Text
        (Network_Dialog.Throughput_Entry,
         Throughput_Value'Image (Throughput (Net)));

      Set_Text
        (Network_Dialog.Max_Pack_Size_Entry,
         Bit_Count_Image (Max_Packet_Size (Net)));

      Set_Text
        (Network_Dialog.Min_Pack_Size_Entry,
         Bit_Count_Image (Min_Packet_Size (Net)));

      Show_All (Network_Dialog);

      if Mast.Processing_Resources.Has_Scheduler (Proc_Res.all) then
         Set_Sensitive (Network_Dialog.New_Primary_Button, False);
      end if;

      Rewind_Drivers
        (Mast.Processing_Resources.Network.Packet_Based_Network (Proc_Res.all),
         Iterator);

      for I in
        1 ..
        Num_Of_Drivers
        (Mast.Processing_Resources.Network.Packet_Based_Network (
                                                                 Proc_Res.all))
      loop
         Get_Next_Driver
           (Mast.Processing_Resources.Network.Packet_Based_Network
              (Proc_Res.all),
            Driv_Ref,
            Iterator);
         Serv_Ref := Packet_Server (Packet_Driver (Driv_Ref.all));
         if Serv_Ref /= null then
            Append (Network_Dialog.Tree_Store, Iter, Parent);
            Set
              (Network_Dialog.Tree_Store,
               Iter,
               Server_Column,
               To_String (Mast.Scheduling_Servers.Name (Serv_Ref)));
            begin
               Proc_Ref := Server_Processing_Resource (Serv_Ref.all);   
	       -- may raise Constraint_Error
               if Proc_Ref = null then
                  Set
                    (Network_Dialog.Tree_Store,
                     Iter,
                     Processor_Column,
                     "(NOT FOUND)");
               else
                  Set
                    (Network_Dialog.Tree_Store,
                     Iter,
                     Processor_Column,
                     To_String (Mast.Processing_Resources.Name (Proc_Ref)));
               end if;
            exception
               when Constraint_Error =>
                  Set
                    (Network_Dialog.Tree_Store,
                     Iter,
                     Processor_Column,
                     "(NOT FOUND)");
            end;
         end if;
      end loop;

   exception
      when No_More_Items =>
         Put_Line ("No more drivers in the network");
   end Read_Parameters;

   --------------------
   -- Draw Processor --
   --------------------
   procedure Draw
     (Item         : access ME_Regular_Processor;
      Cr   : Cairo_Context)
   is
      Rect      : constant Cairo.Region.Cairo_Rectangle_Int  := 
	Get_Coord (Item);
      W         : constant Gdouble        :=Gdouble(Rect.Width);
      H         : constant Gdouble        :=Gdouble(Rect.Height);
      Color,Black : Gdk_RGBA;
      Layout : Pango.Layout.Pango_Layout;
      Success : Boolean;
      Proc      : Processing_Resource_Ref := Item.Res;
      Proc_Name : String                  := To_String (Name (Proc));
      Text : Unbounded_String;
   begin
      Layout := Mast_Editor_Window.Create_Pango_Layout;
      Set_Font_Description (Layout, Font);
      Set_Size(Font,Font_Size);

      Parse (Color, To_String (Item.Color_Name),Success);
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
      Cairo.Stroke(Cr);
      
      Move_To(Cr,0.0,Line_Height+5.0);
      Line_To(Cr,W,Line_Height+6.0);
      Cairo.Stroke(Cr);
      
      if Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (1) then
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(1)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(1)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text
	   (Layout,
	    Cut_Strings.Cut (Proc_Name, "REGULAR PROCESSOR", "PROCESSOR", 22));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String
	   ("Speed factor: " & Speed_Image (Speed_Factor (Proc.all))&NL&
	      "Worst ISR Switch: " & Execution_Time_Image
              (Worst_ISR_Switch
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all))));
	 
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

	 
      elsif Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (2) then

	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(2)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(2)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text
	   (Layout,
	    Cut_Strings.Cut (Proc_Name, "REGULAR PROCESSOR", "PROCESSOR", 30));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String
	   ("Speed factor: " & Speed_Image (Speed_Factor (Proc.all))&NL&
	      "Worst ISR Switch: " & Execution_Time_Image
              (Worst_ISR_Switch
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all)))&NL&
	      "Max Prio: " &
              Priority'Image
              (Max_Interrupt_Priority
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all)))&NL&
	      "Min Prio: " &
              Priority'Image
              (Min_Interrupt_Priority
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all))));

	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

      else
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(3)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(3)));

	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 Set_Font_Description (Layout, Font1);
	 Set_Text
	   (Layout,
	    Cut_Strings.Cut (Proc_Name, "REGULAR PROCESSOR", "PROCESSOR", 35));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String
	   ("Speed factor: " & Speed_Image (Speed_Factor (Proc.all))&NL&
	      "Worst ISR Switch: " & Execution_Time_Image
              (Worst_ISR_Switch
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all)))&NL&
	      "Max Prio: " &
              Priority'Image
              (Max_Interrupt_Priority
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all)))&NL&
	      "Min Prio: " &
              Priority'Image
              (Min_Interrupt_Priority
                 (Mast.Processing_Resources.Processor.Regular_Processor
                    (Proc.all))));
	 
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

      end if;
   end Draw;

   ------------------
   -- Draw Network --
   ------------------
   procedure Draw
     (Item         : access ME_Packet_Based_Network;
      Cr   : Cairo_Context)
   is
      Rect      : constant Cairo.Region.Cairo_Rectangle_Int  := 
	Get_Coord (Item);
      W         : constant Gdouble        :=Gdouble(Rect.Width);
      H         : constant Gdouble        :=Gdouble(Rect.Height);
      Color,Black : Gdk_RGBA;
      Layout : Pango.Layout.Pango_Layout;
      Success : Boolean;
      Net      : Processing_Resource_Ref := Item.Res;
      Net_Name : String                  := To_String (Name (Net));
      Text : Unbounded_String;
   begin
      Layout := Mast_Editor_Window.Create_Pango_Layout;
      Set_Font_Description (Layout, Font);
      Set_Size(Font,Font_Size);

      Parse (Color, To_String (Item.Color_Name),Success);
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
      Cairo.Stroke(Cr);
      
      Move_To(Cr,0.0,Line_Height+5.0);
      Line_To(Cr,W,Line_Height+6.0);
      Cairo.Stroke(Cr);
      
      if Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (1) then
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(1)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(1)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text 
	   (Layout,
	    Cut_Strings.Cut
	      (Net_Name, "PACKET BASED NETWORK", "PCKT NTWK", 22));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String
	   ("Speed Factor: " & Speed_Image (Speed_Factor (Net.all))&NL&
	      "Max Packet: " &
	      Bit_Count_Image
	      (Max_Packet_Size
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all)))&NL&
	      "Min Packet: " &
	      Bit_Count_Image
	      (Min_Packet_Size
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all))));

	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

      elsif Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (2) then
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(2)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(2)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text 
	   (Layout,
	    Cut_Strings.Cut
	      (Net_Name, "PACKET BASED NETWORK", "PCKT NTWK", 30));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String
	   ("Speed Factor: " & Speed_Image (Speed_Factor (Net.all))&NL&
	      "Max Packet: " &
	      Bit_Count_Image
	      (Max_Packet_Size
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all)))&NL&
	      "Min Packet: " &
	      Bit_Count_Image
	      (Min_Packet_Size
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all)))&NL&
	      "Tx Kind: " &
	      Transmission_Kind'Image
	      (Transmission_Mode
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all))));

	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

      else
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(3)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(3)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text 
	   (Layout,
	    Cut_Strings.Cut
	      (Net_Name, "PACKET BASED NETWORK", "PCKT NTWK", 35));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String
	   ("Speed Factor: " & Speed_Image (Speed_Factor (Net.all))&NL&
	      "Max Packet: " &
	      Bit_Count_Image
	      (Max_Packet_Size
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all)))&NL&
	      "Min Packet: " &
	      Bit_Count_Image
	      (Min_Packet_Size
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all)))&NL&
	      "Tx Kind: " &
	      Transmission_Kind'Image
	      (Transmission_Mode
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all)))&NL&
	      "Throughput: " &
	      Throughput_Value'Image
	      (Throughput
		 (Mast.Processing_Resources.Network.Packet_Based_Network
		    (Net.all))));

	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

      end if;
   end Draw;

   --------------------------------
   -- Draw_Timer_In_Proc_Canvas  --
   --------------------------------
   procedure Draw_Timer_In_Proc_Canvas (Item : ME_Processing_Resource_Ref) is
      Time_Ref          : Mast.Timers.System_Timer_Ref;
      Res_Name          : Var_String;
      Me_Timer_Iterator : Mast_Editor.Timers.Lists.Iteration_Object;
      Me_Time_Ref       : ME_Timer_Ref;
      Me_Timer_Found    : Boolean;
   begin
      Time_Ref :=
        Mast.Processing_Resources.Processor.The_System_Timer
        (Mast.Processing_Resources.Processor.Regular_Processor (Item.Res.all));
      Res_Name := Name (Item);
      begin   -- we search the old timer in Me_Timers list
         Me_Timer_Iterator :=
           Mast_Editor.Timers.Lists.Find (Res_Name, Editor_System.Me_Timers);
         Me_Time_Ref       :=
           Mast_Editor.Timers.Lists.Item
           (Me_Timer_Iterator,
            Editor_System.Me_Timers);
         Me_Timer_Found    := True;
      exception
         when Invalid_Index => -- timer not found in list
            Me_Timer_Found := False;
      end;
      if Me_Timer_Found then
         if Time_Ref = null then -- we delete old timer
            Remove (Proc_Res_Canvas, Me_Time_Ref);
            Mast_Editor.Timers.Lists.Delete
              (Me_Timer_Iterator,
               Me_Time_Ref,
               Editor_System.Me_Timers);
         else -- we assign new values to timer
            Me_Time_Ref.Tim  := Time_Ref;
            Me_Time_Ref.Proc := Item.Res;
         end if;
      else
         if Time_Ref /= null then
            Me_Time_Ref             := new ME_System_Timer;
            Me_Time_Ref.Name        := Name (Item);
            Me_Time_Ref.W           := Timer_Width;
            Me_Time_Ref.H           := Timer_Height;
            Me_Time_Ref.Color_Name  := Timer_Color;
            Me_Time_Ref.Canvas_Name := To_Var_String ("Proc_Res_Canvas");
            Me_Time_Ref.Tim         := Time_Ref;
            Me_Time_Ref.Proc        := Item.Res;
            Mast_Editor.Timers.Lists.Add
              (Me_Time_Ref,
               Editor_System.Me_Timers);
            Set_Screen_Size (Me_Time_Ref, Me_Time_Ref.W, Me_Time_Ref.H);
            Put (Proc_Res_Canvas, Me_Time_Ref);
            Show_Item (Proc_Res_Canvas, Me_Time_Ref);
            Add_Canvas_Link (Proc_Res_Canvas, Item, Me_Time_Ref);
         end if;
      end if;
      Refresh_Canvas (Proc_Res_Canvas);
   end Draw_Timer_In_Proc_Canvas;

   ----------------------------------
   -- Draw_Drivers_In_Proc_Canvas  --
   ----------------------------------
   procedure Draw_Drivers_In_Proc_Canvas
     (Item : ME_Processing_Resource_Ref)
   is
      
      -- To check that there are no duplicate pairs of networks-processors
      -- We create a set of strings where each string is the  
      -- processor name appended to the network name with a ',' separator
      -- in between
      package Name_Sets is new Ada.Containers.Indefinite_Ordered_Sets 
	(String, "<", "=");
      
      package Driver_Lists is new Ada.Containers.Vectors
	(Positive, Driver_Ref, "=");
      
      -- Delete the specified drivers, which were found to be duplicates
      procedure Delete_Drivers_From_Network
	(Net : in out Mast.Processing_Resources.Network.Packet_Based_Network;
	 Drivers_To_Delete: Driver_Lists.Vector)
      is
      begin
	 for Drv of Drivers_To_Delete loop
	    begin
	       Mast.Processing_Resources.Network.Remove_Driver(Net, Drv);
	    exception
	       when List_Exceptions.Not_Found =>
		  if Verbose then 
		     Put_Line("Driver not found while removing duplicate");
		  end if;
	    end;
	 end loop;
      end Delete_Drivers_From_Network;
      
      Drive_Ref          : Mast.Drivers.Driver_Ref;
      Drive_Name         : Var_Strings.Var_String;
      Me_Driver_Iterator : Mast_Editor.Drivers.Lists.Iteration_Object;
      Me_Drive_Ref       : ME_Driver_Ref;
      Me_Driver_Found    : Boolean;
      Proc_Ref           : Mast.Processing_Resources.Processing_Resource_Ref;
      Drive_Iterator     :
        Mast.Processing_Resources.Network.Driver_Iteration_Object;
      Serv_Ref           : Mast.Scheduling_Servers.Scheduling_Server_Ref;
      Me_Proc_Ref        :
        Mast_Editor.Processing_Resources.ME_Processing_Resource_Ref;
      Me_Res_Iterator    :
        Mast_Editor.Processing_Resources.Lists.Iteration_Object;
      
      Name_Set : Name_Sets.Set := Name_Sets.Empty_Set;
      Drivers_To_Delete : Driver_Lists.Vector :=Driver_Lists.Empty_Vector;
      
   begin
      Mast.Processing_Resources.Network.Rewind_Drivers
        (Mast.Processing_Resources.Network.Packet_Based_Network (Item.Res.all),
         Drive_Iterator);
      for I in
        1 ..
        Mast.Processing_Resources.Network.Num_Of_Drivers
        (Mast.Processing_Resources.Network.Packet_Based_Network 
	   (Item.Res.all))
      loop
         Me_Driver_Found := False;
         Mast.Processing_Resources.Network.Get_Next_Driver
           (Mast.Processing_Resources.Network.Packet_Based_Network
              (Item.Res.all),
            Drive_Ref,
            Drive_Iterator);
         if Drive_Ref /= null then
	    if Packet_Server (Packet_Driver (Drive_Ref.all)) /= null
	    then
	       Serv_Ref := Packet_Server (Packet_Driver (Drive_Ref.all));
	       begin
		  Proc_Ref := Server_Processing_Resource (Serv_Ref.all);
		  -- may raise Constraint_Error

		  if Proc_Ref = null then
		     Gtk_New (Editor_Error_Window);
		     Set_Text
		       (Editor_Error_Window.Label,
			"Error: Processor not found!!!");
		     Show_All (Editor_Error_Window);
		  else
		     Drive_Name := (Name (Proc_Ref) & Delimiter & Name (Item));
		     
		     -- If this is a duplicate driver we display a warning 
		     -- message and ignore the driver
		     if Name_Set.Contains(To_String(Drive_Name)) then
			-- Add the driver to the list of drivers to delete
			Driver_Lists.Append(Drivers_To_Delete, Drive_Ref);
			
			-- Warning message
			Gtk_New (Editor_Error_Window);
			Set_Text
			  (Editor_Error_Window.Label,
			   "A duplicate driver between " & 
			     To_String(Drive_Name) &
			     " was found."&
			     ASCII.LF & "The duplicate driver is deleted");
			Show_All (Editor_Error_Window);
		     else
			Name_Set.Insert(To_String(Drive_Name));
			begin  -- we search the old driver in Me_Drivers list
			   Me_Driver_Iterator :=
			     Mast_Editor.Drivers.Lists.Find
			     (Drive_Name,
			      Editor_System.Me_Drivers);
			   Me_Drive_Ref       :=
			     Mast_Editor.Drivers.Lists.Item
			     (Me_Driver_Iterator,
			      Editor_System.Me_Drivers);
			   Me_Driver_Found    := True;
			exception
			   when Invalid_Index => -- driver not found in list
			      Me_Driver_Found := False;
			end;
			if Me_Driver_Found then
			   Me_Drive_Ref.Driv := Drive_Ref;
			   Me_Drive_Ref.Net  := Item.Res;
			   Me_Drive_Ref.Proc := Proc_Ref;
			else  
			   -- we create a new ME_Packet_Driver, draw it and add
			   --it to Me_Drivers list
			   Me_Drive_Ref             := new ME_Packet_Driver;
			   Me_Drive_Ref.Name        :=
			     (Name (Proc_Ref) & Delimiter & Name (Item));
			   Me_Drive_Ref.W           := Driv_Width;
			   Me_Drive_Ref.H           := Driv_Height;
			   Me_Drive_Ref.Color_Name  := Driv_Color;
			   Me_Drive_Ref.Canvas_Name :=
			     To_Var_String ("Proc_Res_Canvas");
			   Me_Drive_Ref.Driv        := Drive_Ref;
			   Me_Drive_Ref.Net         := Item.Res;
			   Me_Drive_Ref.Proc        := Proc_Ref;
			   Mast_Editor.Drivers.Lists.Add
			     (Me_Drive_Ref,
			      Editor_System.Me_Drivers);
			   Set_Screen_Size
			     (Me_Drive_Ref,
			      Me_Drive_Ref.W,
			      Me_Drive_Ref.H);
			   Put (Proc_Res_Canvas, Me_Drive_Ref);
			   Show_Item (Proc_Res_Canvas, Me_Drive_Ref);
			   Add_Canvas_Link 
			     (Proc_Res_Canvas, Item, Me_Drive_Ref);
			   begin    -- we search processor in
				    --Me_Processing_Resources list
			      Me_Res_Iterator :=
				Mast_Editor.Processing_Resources.Lists.Find
				(Name (Proc_Ref),
				 Editor_System.Me_Processing_Resources);
			      Me_Proc_Ref     :=
				Mast_Editor.Processing_Resources.Lists.Item
				(Me_Res_Iterator,
				 Editor_System.Me_Processing_Resources);
			      Add_Canvas_Link
				(Proc_Res_Canvas,
				 Me_Drive_Ref,
				 Me_Proc_Ref);
			   exception
			      when Invalid_Index =>   
				 -- processor not found in
				 --list, can't draw (driver ->
				 --proc) link
				 null;
			   end;
			end if;
		     end if;
		  end if;
	       exception
		  when Constraint_Error =>   
		     -- can't find processor of driver at
		     --position I
		     if Verbose then
			Put_Line ("can't find processor of driver");
		     end if;
	       end;
	    else
	       -- Add the driver to the list of drivers to delete
	       Driver_Lists.Append(Drivers_To_Delete, Drive_Ref);
	       
	       -- Warning message
	       Gtk_New (Editor_Error_Window);
	       Set_Text
		 (Editor_Error_Window.Label,
		  "A driver for network " & To_String(Item.Name) &
		    " has no packet server."&
		    ASCII.LF & "The incorrect driver is deleted");
	       Show_All (Editor_Error_Window);

	    end if;
         end if;
      end loop;
      Delete_Drivers_From_Network
	(Mast.Processing_Resources.Network.Packet_Based_Network (Item.Res.all),
	 Drivers_To_Delete);
      Refresh_Canvas (Proc_Res_Canvas);
   exception
      when No_More_Items =>
	 Delete_Drivers_From_Network
	   (Mast.Processing_Resources.Network.Packet_Based_Network 
	      (Item.Res.all),
	    Drivers_To_Delete);
         Refresh_Canvas (Proc_Res_Canvas);
	 if Verbose then
	    Put_Line("Cannot find more driver items");
	 end if;
   end Draw_Drivers_In_Proc_Canvas;

   ---------------------
   -- Write Processor -- (Write the params of an existing processor and
   --refresh the canvas)
   ---------------------
   procedure Write_Processor
     (Widget : access Gtk_Widget_Record'Class;
      Data   : ME_Processing_Resource_And_Dialog_Ref)
   is
      Item             : ME_Processing_Resource_Ref := Data.It;
      Processor_Dialog : Processor_Dialog_Access    :=
        Processor_Dialog_Access (Data.Dia);
   begin
      if Id_Name_Is_Valid
        (Ada.Characters.Handling.To_Lower
           (Get_Text (Processor_Dialog.Proc_Name_Entry)))
      then
         Write_Parameters (Item, Gtk_Dialog (Processor_Dialog));
         Draw_Timer_In_Proc_Canvas (Item);
         Refresh_Canvas (Proc_Res_Canvas);
         Destroy (Processor_Dialog);
      else
         Gtk_New (Editor_Error_Window, Gtk_Window(Processor_Dialog));
	 --Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Identifier not Valid!!!");
         Show_All (Editor_Error_Window);
      end if;
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
         Show_All (Editor_Error_Window);
         Destroy (Processor_Dialog);
   end Write_Processor;

   -------------------
   -- New_Processor -- (Add new processor to canvas and to the lists of the
   --systems)
   -------------------
   procedure New_Processor
     (Widget : access Gtk_Widget_Record'Class;
      Data   : ME_Processing_Resource_And_Dialog_Ref)
   is
      Item             : ME_Processing_Resource_Ref := Data.It;
      Processor_Dialog : Processor_Dialog_Access    :=
        Processor_Dialog_Access (Data.Dia);
   begin
      if Id_Name_Is_Valid
        (Ada.Characters.Handling.To_Lower
           (Get_Text (Processor_Dialog.Proc_Name_Entry)))
      then
         Write_Parameters (Item, Gtk_Dialog (Processor_Dialog));
         Mast.Processing_Resources.Lists.Add
           (Item.Res,
            The_System.Processing_Resources);
         Mast_Editor.Processing_Resources.Lists.Add
           (Item,
            Editor_System.Me_Processing_Resources);
         Set_Screen_Size (Item, Item.W, Item.H);
         Put (Proc_Res_Canvas, Item);
         Refresh_Canvas (Proc_Res_Canvas);
         Show_Item (Proc_Res_Canvas, Item);
         Draw_Timer_In_Proc_Canvas (Item);
         Destroy (Processor_Dialog);
      else
         Gtk_New (Editor_Error_Window, Gtk_Window(Processor_Dialog));
         Set_Text (Editor_Error_Window.Label, "Identifier not Valid!!!");
         Show_All (Editor_Error_Window);
      end if;
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
         Show_All (Editor_Error_Window);
         Destroy (Processor_Dialog);
      when Already_Exists =>
         Gtk_New (Editor_Error_Window);
         Set_Text
           (Editor_Error_Window.Label,
            "The processing resource already exists !!!");
         Show_All (Editor_Error_Window);
         Destroy (Processor_Dialog);
   end New_Processor;

   ---------------------------
   -- Show Processor Dialog -- (Show processor_dialog with default params)
   ---------------------------
   procedure Show_Processor_Dialog
     (Widget : access Gtk_Button_Record'Class)
   is
      Item             : ME_Processing_Resource_Ref            :=
        new ME_Regular_Processor;
      Reso             : Processing_Resource_Ref               :=
        new Mast.Processing_Resources.Processor.Regular_Processor;
      Processor_Dialog : Processor_Dialog_Access;
      Me_Data          : ME_Processing_Resource_And_Dialog_Ref :=
        new ME_Processing_Resource_And_Dialog;
   begin
      Item.W           := Proc_Width;
      Item.H           := Proc_Height;
      Item.Canvas_Name := To_Var_String ("Proc_Res_Canvas");
      Item.Color_Name  := Proc_Color;
      Item.Res         := Reso;

      Gtk_New (Processor_Dialog);
      Set_Sensitive (Processor_Dialog.New_Primary_Button, False);
      Read_Parameters (Item, Gtk_Dialog (Processor_Dialog));
      Me_Data.It  := Item;
      Me_Data.Dia := Gtk_Dialog (Processor_Dialog);

      Me_Processing_Resource_And_Dialog_Cb.Connect
        (Processor_Dialog.Proc_Dialog_Ok_Button,
         "clicked",
         Me_Processing_Resource_And_Dialog_Cb.To_Marshaller
           (New_Processor'Access),
         Me_Data);
   end Show_Processor_Dialog;

   -------------------
   -- Remove Driver -- (Remove driver selected in network_dialog)
   -------------------
   procedure Remove_Driver
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Processing_Resource_And_Dialog_Ref)
   is
      Res                                   : Processing_Resource_Ref :=
        Data.It;
      Network_Dialog                        : Network_Dialog_Access   :=
        Network_Dialog_Access (Data.Dia);
      Server_Name, Processor_Name           : Var_String;
      Temp_Server_Name, Temp_Processor_Name : Var_String;
      Temp_Serv_Ref                         :
        Mast.Scheduling_Servers.Scheduling_Server_Ref;
      Temp_Proc_Ref                         :
        Mast.Processing_Resources.Processing_Resource_Ref;
      Driv_Ref                              : Mast.Drivers.Driver_Ref;
      Driv_Iterator                         :
        Mast.Processing_Resources.Network.Driver_Iteration_Object;

      Me_Driver_Iterator : Mast_Editor.Drivers.Lists.Iteration_Object;
      Me_Driver_Ref      : Mast_Editor.Drivers.ME_Driver_Ref;

      Row_Selected : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Model        : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter         : Gtk.Tree_Model.Gtk_Tree_Iter;
      Processor_Not_Found : exception;
   begin
      Row_Selected := Gtk.Tree_View.Get_Selection (Network_Dialog.Tree_View);
      Get_Selected (Row_Selected, Model, Iter);

      if Iter /= Null_Iter then -- Iter = Null_Iter when no row selected
         Server_Name    :=
           To_Var_String (Get_String (Network_Dialog.Tree_Store, Iter, 0));
         Processor_Name :=
           To_Var_String (Get_String (Network_Dialog.Tree_Store, Iter, 1));

         Mast.Processing_Resources.Network.Rewind_Drivers
           (Mast.Processing_Resources.Network.Packet_Based_Network (Res.all),
            Driv_Iterator);
         for I in
           1 ..
           Mast.Processing_Resources.Network.Num_Of_Drivers
           (Mast.Processing_Resources.Network.Packet_Based_Network (Res.all))
         loop
            Mast.Processing_Resources.Network.Get_Next_Driver
              (Mast.Processing_Resources.Network.Packet_Based_Network
                 (Res.all),
               Driv_Ref,
               Driv_Iterator);
            Temp_Serv_Ref := Packet_Server (Packet_Driver (Driv_Ref.all));
            if Temp_Serv_Ref /= null then
               Temp_Server_Name := Name (Temp_Serv_Ref);
               begin
                  Temp_Proc_Ref :=
                    Server_Processing_Resource (Temp_Serv_Ref.all); -- raise
                                                                    --Constrai
                                                                    --nt_Error
                  if Temp_Proc_Ref /= null then
                     Temp_Processor_Name := Name (Temp_Proc_Ref);
                     if Processor_Name = Temp_Processor_Name
                       and then Server_Name = Temp_Server_Name
                     then

                        -- Change Canvas_Item flag (visibility boolean) to
                        --false in order to know
                        -- which drivers should be removed from Me_Drivers
                        --list and Proc_Res_Canvas
                        -- if OK_Button is clicked.
                        -- If Cancel_Button is clicked, we have to change the
                        --flag again to true
                        -- and no changes will take effect.

                        Mast_Editor.Drivers.Lists.Rewind
                          (Editor_System.Me_Drivers,
                           Me_Driver_Iterator);
                        for I in
                          1 ..
                          Mast_Editor.Drivers.Lists.Size
                          (Editor_System.Me_Drivers)
                        loop
                           Me_Driver_Ref :=
                             Mast_Editor.Drivers.Lists.Item
                             (Me_Driver_Iterator,
                              Editor_System.Me_Drivers);
                           if Me_Driver_Ref.Driv /= null
                             and then Me_Driver_Ref.Driv = Driv_Ref
                           then
                              Set_Visibility (Me_Driver_Ref, False); -- Change
                                                                     --flag
                           end if;
                           Mast_Editor.Drivers.Lists.Get_Next_Item
                             (Me_Driver_Ref,
                              Editor_System.Me_Drivers,
                              Me_Driver_Iterator);
                        end loop;
                        -- Remove driver from drivers list of network
                        Mast.Processing_Resources.Network.Remove_Driver
                          (
                           Mast.Processing_Resources.Network.
                             Packet_Based_Network (Res.all),
                           Driv_Ref);
                        Remove (Network_Dialog.Tree_Store, Iter);
                        exit;
                     end if;
                  else
                     raise Processor_Not_Found;
                  end if;

               exception
                  when Constraint_Error | Processor_Not_Found =>  -- Processor
                                                                  --Not Found
                     if Processor_Name = To_Var_String ("(NOT FOUND)")
		       and then Server_Name = Temp_Server_Name
                     then
			Mast_Editor.Drivers.Lists.Rewind
			  (Editor_System.Me_Drivers,
			   Me_Driver_Iterator);
			for I in
			  1 ..
			  Mast_Editor.Drivers.Lists.Size
			  (Editor_System.Me_Drivers)
			loop
			   Me_Driver_Ref :=
			     Mast_Editor.Drivers.Lists.Item
			     (Me_Driver_Iterator,
			      Editor_System.Me_Drivers);
			   if Me_Driver_Ref.Driv /= null
			     and then Me_Driver_Ref.Driv = Driv_Ref
			   then
			      Set_Visibility (Me_Driver_Ref, False); -- Change
								     --flag
			   end if;
			   Mast_Editor.Drivers.Lists.Get_Next_Item
			     (Me_Driver_Ref,
			      Editor_System.Me_Drivers,
			      Me_Driver_Iterator);
			end loop;
			-- Remove driver from drivers list of network
			Mast.Processing_Resources.Network.Remove_Driver
			  (Mast.Processing_Resources.Network.
			     Packet_Based_Network (Res.all),
			   Driv_Ref);
			Remove (Network_Dialog.Tree_Store, Iter);
			exit;
                     end if;
               end;
            end if;
         end loop;
      end if;

   exception
      when No_More_Items =>
         Put_Line ("No More Items in drivers list");
   end Remove_Driver;

   -------------------
   -- Write Driver  -- (Read the params from driver dialog and add driver to
   --network)
   -------------------
   procedure Write_Driver
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Processing_Resource_And_Dialog2_Ref)
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
	(Data : Processing_Resource_And_Dialog2_Ref;
	 This_Proc_Name : Var_String;
	 This_Net_Name : Var_String) 
	return boolean
      is
	 -- This_Driv_Ref : Driver_Ref :=
	 --  ME_Driver_Ref (Item).Driv;
	 Net_Ref : Mast.Processing_Resources.Processing_Resource_Ref :=
	   Data.It; -- the network
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
	    Serv_Ref := Packet_Driver(Drive_Ref.all).Packet_Server;
	    if Serv_Ref/=null and then
	      This_Proc_Name = Serv_Ref.Server_Processing_Resource.Name 
	    then
	       -- duplicate found
	       return True;
	    end if;
	 end loop;
	 return False;
      end Driver_Exists;      
      
      Res            : Processing_Resource_Ref := Data.It; -- the network
      Network_Dialog : Network_Dialog_Access   :=
        Network_Dialog_Access (Data.Dia);
      Driver_Dialog  : Driver_Dialog_Access    :=
        Driver_Dialog_Access (Data.Dia2);
      Driv_Ref       : Mast.Drivers.Driver_Ref;
      Proc_Ref : Mast.Processing_Resources.Processing_Resource_Ref;
      Serv_Ref       : Mast.Scheduling_Servers.Scheduling_Server_Ref;
      Send_Op_Ref   : Mast.Operations.Operation_Ref;
      Recv_Op_Ref   : Mast.Operations.Operation_Ref;
      Ovhd_Model : Rta_Overhead_Model_Type;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Iter   : Gtk_Tree_Iter;

   begin
      -- Check packet server
      Serv_Ref := Server_Reference (Driver_Dialog.Packet_Server_Combo);
      
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
      
      -- Establish processor reference
      Proc_Ref := Serv_Ref.Server_Processing_Resource;
      
      -- Check that this is not a duplicate driver between the 
      -- same network and processor
      if Driver_Exists(Data, Proc_Ref.Name, Res.Name) then
      	 -- A duplicate driver was found
      	 Gtk_New (Editor_Error_Window, Gtk_Window(Driver_Dialog));
      	 Set_Text
      	   (Editor_Error_Window.Label,
      	    "Error: Duplicate Driver" & ASCII.LF &
      	      "A Driver for network "& To_String(Res.Name) & 
      	      " and processor " &
      	      To_String(Proc_Ref.Name) & " already exists");
      	 Show_All (Editor_Error_Window);
      	 return;
      end if;      
      
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
        (Packet_Driver(Driv_Ref.all), Serv_Ref);

      -- Show Server and Processor names in Network_Dialog.Tree_View
      if Serv_Ref /= null then
         Append (Network_Dialog.Tree_Store, Iter, Parent);
         Set
           (Network_Dialog.Tree_Store,
            Iter,
            Server_Column,
            To_String (Mast.Scheduling_Servers.Name (Serv_Ref)));
         begin
            if Proc_Ref = null then
               Set
                 (Network_Dialog.Tree_Store,
                  Iter,
                  Processor_Column,
                  "(NOT FOUND)");
            else
               Set
                 (Network_Dialog.Tree_Store,
                  Iter,
                  Processor_Column,
                  To_String (Mast.Processing_Resources.Name (Proc_Ref)));
            end if;
         exception
            when Constraint_Error =>
               Set
                 (Network_Dialog.Tree_Store,
                  Iter,
                  Processor_Column,
                  "(NOT FOUND)");
         end;
      end if;

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
      
      -- Process Rta_Overhead_Model
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

      Add_Driver
        (Mast.Processing_Resources.Network.Packet_Based_Network (Res.all),
         Driv_Ref);
      Destroy (Driver_Dialog);
   exception
      when others =>
         Gtk_New (Editor_Error_Window, Gtk_Window(Data.Dia));
         Set_Text
           (Editor_Error_Window.Label,
            "Error while writing driver parameters !!!");
         Show_All (Editor_Error_Window);
         Destroy (Driver_Dialog);
   end Write_Driver;

   ----------------
   -- Add Driver -- (Show driver dialog)
   ----------------
   procedure Add_Driver
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Processing_Resource_And_Dialog_Ref)
   is
      Driver_Dialog : Driver_Dialog_Access;
      Data2         : Processing_Resource_And_Dialog2_Ref :=
        new Processing_Resource_And_Dialog2;
   begin
      Gtk_New (Driver_Dialog, Gtk_Window(Data.Dia));
      --Gtk_New (Driver_Dialog);
      Show_All (Driver_Dialog);
      Hide (Driver_Dialog.Character_Server_Table);
      Hide (Driver_Dialog.Rtep_Table);

      Data2.It   := Data.It;
      Data2.Dia  := Data.Dia;
      Data2.Dia2 := Gtk_Dialog (Driver_Dialog);

      Processing_Resource_And_Dialog2_Cb.Connect
        (Driver_Dialog.Driver_Ok_Button,
         "clicked",
         Processing_Resource_And_Dialog2_Cb.To_Marshaller
           (Write_Driver'Access),
         Data2);
      Driver_Dialog.Resize(600, 276);
   end Add_Driver;

   -------------------
   -- Write_Network --
   -------------------
   procedure Write_Network
     (Widget : access Gtk_Widget_Record'Class;
      Data   : Network_Args_Ref)
   is
      New_Net            : Processing_Resource_Ref    := Data.Net;
      Item               : ME_Processing_Resource_Ref := Data.It;
      Network_Dialog     : Network_Dialog_Access      :=
        Network_Dialog_Access (Data.Dia);
      Me_Driver_Iterator : Mast_Editor.Drivers.Lists.Iteration_Object;
      Me_Driver_Ref      : Mast_Editor.Drivers.ME_Driver_Ref;
   begin
      if (Throughput_Value'Value
            (Get_Text (Network_Dialog.Throughput_Entry)) <=
            0.0) or
        (Bit_Count'Value (Get_Text (Network_Dialog.Max_Pack_Size_Entry)) <=
           0.0) or
        (Bit_Count'Value (Get_Text (Network_Dialog.Min_Pack_Size_Entry)) <=
           0.0)
      then

         Gtk_New (Editor_Error_Window, Gtk_Window(Network_Dialog));
         Set_Text
           (Editor_Error_Window.Label,
            "Throughput, Max Packet Size and Min Packet Size");
         Set_Text
           (Editor_Error_Window.Down_Label,
            " must be greater than zero !");
         Show_All (Editor_Error_Window);

      else
         if Id_Name_Is_Valid
           (Ada.Characters.Handling.To_Lower
              (Get_Text (Network_Dialog.Net_Name_Entry)))
         then

            Item.Res.all := New_Net.all;

            Mast_Editor.Drivers.Lists.Rewind
              (Editor_System.Me_Drivers,
               Me_Driver_Iterator);
            for I in
              1 ..
              Mast_Editor.Drivers.Lists.Size (Editor_System.Me_Drivers)
            loop
               Me_Driver_Ref :=
                 Mast_Editor.Drivers.Lists.Item
                 (Me_Driver_Iterator,
                  Editor_System.Me_Drivers);
               -- Check visibility flag for each element of Me_Drivers list
               if not Is_Visible (Me_Driver_Ref) then
                  Mast_Editor.Drivers.Lists.Delete
                    (Me_Driver_Iterator,
                     Me_Driver_Ref,
                     Editor_System.Me_Drivers);
                  Remove (Proc_Res_Canvas, Me_Driver_Ref);
               end if;
               Mast_Editor.Drivers.Lists.Get_Next_Item
                 (Me_Driver_Ref,
                  Editor_System.Me_Drivers,
                  Me_Driver_Iterator);
            end loop;

            Write_Parameters (Item, Gtk_Dialog (Network_Dialog));
            Draw_Drivers_In_Proc_Canvas (Item);
            Refresh_Canvas (Proc_Res_Canvas);
            Destroy (Network_Dialog);
         else
            Gtk_New (Editor_Error_Window, Gtk_Window(Network_Dialog));
            Set_Text (Editor_Error_Window.Label, "Identifier not Valid!!!");
            Show_All (Editor_Error_Window);
         end if;

      end if;
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
         Show_All (Editor_Error_Window);
         Destroy (Network_Dialog);
      when others =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Error writing network !!!");
         Show_All (Editor_Error_Window);
         Destroy (Network_Dialog);
   end Write_Network;

   -------------------
   -- New Network   -- (Add new network to canvas and to the lists of the
   --systems)
   -------------------
   procedure New_Network
     (Widget : access Gtk_Widget_Record'Class;
      Data   : ME_Processing_Resource_And_Dialog_Ref)
   is
      Item           : ME_Processing_Resource_Ref := Data.It;
      Network_Dialog : Network_Dialog_Access      :=
        Network_Dialog_Access (Data.Dia);
   begin
      if (Throughput_Value'Value
            (Get_Text (Network_Dialog.Throughput_Entry)) <=
            0.0) or
        (Bit_Count'Value (Get_Text (Network_Dialog.Max_Pack_Size_Entry)) <=
           0.0) or
        (Bit_Count'Value (Get_Text (Network_Dialog.Min_Pack_Size_Entry)) <=
           0.0)
      then

         Gtk_New (Editor_Error_Window, Gtk_Window(Network_Dialog));
         Set_Text
           (Editor_Error_Window.Label,
            "Throughput, Max Packet Size and Min Packet Size");
         Set_Text
           (Editor_Error_Window.Down_Label,
            " must be greater than zero !");
         Show_All (Editor_Error_Window);

      else
         if Id_Name_Is_Valid
           (Ada.Characters.Handling.To_Lower
              (Get_Text (Network_Dialog.Net_Name_Entry)))
         then
            Write_Parameters (Item, Gtk_Dialog (Network_Dialog));
            Mast.Processing_Resources.Lists.Add
              (Item.Res,
               The_System.Processing_Resources);
            Mast_Editor.Processing_Resources.Lists.Add
              (Item,
               Editor_System.Me_Processing_Resources);
            Set_Screen_Size (Item, Item.W, Item.H);
            Put (Proc_Res_Canvas, Item);
            Refresh_Canvas (Proc_Res_Canvas);
            Show_Item (Proc_Res_Canvas, Item);
            Draw_Drivers_In_Proc_Canvas (Item);
            Destroy (Network_Dialog);
         else
            Gtk_New (Editor_Error_Window, Gtk_Window(Network_Dialog));
            Set_Text (Editor_Error_Window.Label, "Identifier not Valid!!!");
            Show_All (Editor_Error_Window);
         end if;
      end if;
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
         Show_All (Editor_Error_Window);
         Destroy (Network_Dialog);
      when Already_Exists =>
         Gtk_New (Editor_Error_Window);
         Set_Text
           (Editor_Error_Window.Label,
            "The processing resource already exists !!!");
         Show_All (Editor_Error_Window);
         Destroy (Network_Dialog);
   end New_Network;

   -------------------------
   -- Show Network Dialog -- (Show network dialog with the default params)
   -------------------------
   procedure Show_Network_Dialog (Widget : access Gtk_Button_Record'Class) is
      Item           : ME_Processing_Resource_Ref            :=
        new ME_Packet_Based_Network;
      Reso           : Processing_Resource_Ref               :=
        new Mast.Processing_Resources.Network.Packet_Based_Network;
      Network_Dialog : Network_Dialog_Access;
      Data           : Processing_Resource_And_Dialog_Ref    :=
        new Processing_Resource_And_Dialog;
      Me_Data        : ME_Processing_Resource_And_Dialog_Ref :=
        new ME_Processing_Resource_And_Dialog;
   begin
      Item.W           := Net_Width;
      Item.H           := Net_Height;
      Item.Canvas_Name := To_Var_String ("Proc_Res_Canvas");
      Item.Color_Name  := Net_Color;
      Set_Max_Packet_Size (Packet_Based_Network (Reso.all), Large_Bit_Count);
      Set_Min_Packet_Size (Packet_Based_Network (Reso.all), Large_Bit_Count);
      Item.Res := Reso;

      Gtk_New (Network_Dialog);
      Set_Sensitive (Network_Dialog.New_Primary_Button, False);
      Read_Parameters (Item, Gtk_Dialog (Network_Dialog));
      Data.It     := Reso;
      Data.Dia    := Gtk_Dialog (Network_Dialog);
      Me_Data.It  := Item;
      Me_Data.Dia := Gtk_Dialog (Network_Dialog);

      Processing_Resource_And_Dialog_Cb.Connect
        (Network_Dialog.Add_Driver_Button,
         "clicked",
         Processing_Resource_And_Dialog_Cb.To_Marshaller (Add_Driver'Access),
         Data);

      Processing_Resource_And_Dialog_Cb.Connect
        (Network_Dialog.Remove_Driver_Button,
         "clicked",
         Processing_Resource_And_Dialog_Cb.To_Marshaller
           (Remove_Driver'Access),
         Data);

      Dialog_Callback.Object_Connect
        (Network_Dialog.Network_Cancel_Button,
         "clicked",
         Dialog_Callback.To_Marshaller
           (On_Network_Cancel_Button_Clicked'Access),
         Network_Dialog);

      Me_Processing_Resource_And_Dialog_Cb.Connect
        (Network_Dialog.Network_Ok_Button,
         "clicked",
         Me_Processing_Resource_And_Dialog_Cb.To_Marshaller
           (New_Network'Access),
         Me_Data);
   end Show_Network_Dialog;

   ----------------------
   -- Remove_Proc_Res  --
   ----------------------
   procedure Remove_Proc_Res
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Processing_Resource_Ref)
   is
      Res_Name            : Var_String;
      Res_Index           : Mast.Processing_Resources.Lists.Index;
      Item_Deleted        : Processing_Resource_Ref;
      Me_Res_Index        : Mast_Editor.Processing_Resources.Lists.Index;
      Me_Item_Deleted     : ME_Processing_Resource_Ref;
      Me_Timer_Iterator   : Mast_Editor.Timers.Lists.Iteration_Object;
      Me_Time_Ref         : Mast_Editor.Timers.ME_Timer_Ref;
      Me_Driver_Iterator  : Mast_Editor.Drivers.Lists.Iteration_Object;
      Me_Drive_Ref        : Mast_Editor.Drivers.ME_Driver_Ref;
      New_ME_Drivers_List : Mast_Editor.Drivers.Lists.List;
   begin
      Res_Name     := Name (Item);
      Res_Index    :=
        Mast.Processing_Resources.Lists.Find
        (Res_Name,
         The_System.Processing_Resources);
      Item_Deleted :=
        Mast.Processing_Resources.Lists.Item
        (Res_Index,
         The_System.Processing_Resources);
      -- Check if the scheduler is referenced by a scheduling server
      if Mast.Schedulers.List_References_Processing_Resource
        (Item_Deleted,
         The_System.Schedulers)
      then
         -- Processing resource cannot be deleted
         Gtk_New (Editor_Error_Window);
         Set_Text
           (Editor_Error_Window.Label,
            "PROCESSING RESOURCE IS REFERENCED BY A SCHEDULER");
         Show_All (Editor_Error_Window);
         Destroy (Item_Menu);
      else
	 if Message_Dialog
	   (Msg => " Do you really want to remove this object together with" &
              " its drivers and timers? ",
	    Dialog_Type => Confirmation,
	    Buttons => Button_Yes or Button_No,
	    Default_Button => Button_Yes,
	    Parent => Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window)) =
	   Button_Yes
         then
            -- Remove Timers and Drivers
            if Item.all in ME_Processor'Class then
               begin
                  -- we search the timer with processor name in Me_Timers list
                  Me_Timer_Iterator :=
                    Mast_Editor.Timers.Lists.Find
                    (Res_Name,
                     Editor_System.Me_Timers);
                  Me_Time_Ref       :=
                    Mast_Editor.Timers.Lists.Item
                    (Me_Timer_Iterator,
                     Editor_System.Me_Timers);
                  Mast_Editor.Timers.Lists.Delete
                    (Me_Timer_Iterator,
                     Me_Time_Ref,
                     Editor_System.Me_Timers);
                  Remove (Proc_Res_Canvas, Me_Time_Ref);
               exception
                  when Invalid_Index => -- timer not found in list
                     null; -- nothing to do
               end;
               begin
                  Mast_Editor.Drivers.Lists.Rewind
                    (Editor_System.Me_Drivers,
                     Me_Driver_Iterator);
                  for I in
                    1 ..
                    Mast_Editor.Drivers.Lists.Size
                    (Editor_System.Me_Drivers)
                  loop
                     Mast_Editor.Drivers.Lists.Get_Next_Item
                       (Me_Drive_Ref,
                        Editor_System.Me_Drivers,
                        Me_Driver_Iterator);
                     if Name (Me_Drive_Ref.Proc) = Res_Name then
                        Remove (Proc_Res_Canvas, Me_Drive_Ref);
                     else
                        Mast_Editor.Drivers.Lists.Add
                          (Me_Drive_Ref,
                           New_ME_Drivers_List);
                     end if;
                  end loop;
                  Editor_System.Me_Drivers := New_ME_Drivers_List;
               exception
                  when Exc : others =>
                     Gtk_New (Editor_Error_Window);
                     Set_Text
                       (Editor_Error_Window.Label,
                        "ERROR WHILE DELETING DRIVERS" &
                          Ada.Exceptions.Exception_Name (Exc));
                     Show_All (Editor_Error_Window);
                     Destroy (Item_Menu);
               end;
            elsif Item.all in ME_Network'Class then
               begin
                  Mast_Editor.Drivers.Lists.Rewind
                    (Editor_System.Me_Drivers,
                     Me_Driver_Iterator);
                  for I in
                    1 ..
                    Mast_Editor.Drivers.Lists.Size
                    (Editor_System.Me_Drivers)
                  loop
                     Mast_Editor.Drivers.Lists.Get_Next_Item
                       (Me_Drive_Ref,
                        Editor_System.Me_Drivers,
                        Me_Driver_Iterator);
                     if Name (Me_Drive_Ref.Net) = Res_Name then
                        Remove (Proc_Res_Canvas, Me_Drive_Ref);
                     else
                        Mast_Editor.Drivers.Lists.Add
                          (Me_Drive_Ref,
                           New_ME_Drivers_List);
                     end if;
                  end loop;
                  Editor_System.Me_Drivers := New_ME_Drivers_List;
               exception
                  when Exc : others =>
                     Gtk_New (Editor_Error_Window);
                     Set_Text
                       (Editor_Error_Window.Label,
                        "ERROR WHILE DELETING DRIVERS" &
                          Ada.Exceptions.Exception_Name (Exc));
                     Show_All (Editor_Error_Window);
                     Destroy (Item_Menu);
               end;
            end if;
            -- Remove Processing Resource
            Mast.Processing_Resources.Lists.Delete
              (Res_Index,
               Item_Deleted,
               The_System.Processing_Resources);
            Me_Res_Index    :=
              Mast_Editor.Processing_Resources.Lists.Find
              (Res_Name,
               Editor_System.Me_Processing_Resources);
            Me_Item_Deleted :=
              Mast_Editor.Processing_Resources.Lists.Item
              (Me_Res_Index,
               Editor_System.Me_Processing_Resources);
            Mast_Editor.Processing_Resources.Lists.Delete
              (Me_Res_Index,
               Me_Item_Deleted,
               Editor_System.Me_Processing_Resources);
            Remove (Proc_Res_Canvas, Item);
            Refresh_Canvas (Proc_Res_Canvas);
            Change_Control.Changes_Made;
            Destroy (Item_Menu);
         end if;
      end if;
   exception
      when Invalid_Index =>
         Gtk_New (Editor_Error_Window);
         Set_Text
           (Editor_Error_Window.Label,
            "ERROR IN PROCESSING RESOURCE REMOVAL !!!");
         Show_All (Editor_Error_Window);
         Destroy (Item_Menu);
   end Remove_Proc_Res;

   ----------------------
   -- Properties_Proc  --
   ----------------------
   procedure Properties_Proc
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Processing_Resource_Ref)
   is
      Processor_Dialog : Processor_Dialog_Access;
      Me_Data          : ME_Processing_Resource_And_Dialog_Ref :=
        new ME_Processing_Resource_And_Dialog;
   begin
      Gtk_New (Processor_Dialog);
      Read_Parameters (Item, Gtk_Dialog (Processor_Dialog));
      Me_Data.It  := Item;
      Me_Data.Dia := Gtk_Dialog (Processor_Dialog);

      Me_Processing_Resource_And_Dialog_Cb.Connect
        (Processor_Dialog.Proc_Dialog_Ok_Button,
         "clicked",
         Me_Processing_Resource_And_Dialog_Cb.To_Marshaller
           (Write_Processor'Access),
         Me_Data);

      Refresh_Canvas (Proc_Res_Canvas);
      Destroy (Item_Menu);
   end Properties_Proc;

   --------------------------
   -- Set_Items_Visibility --
   --------------------------
   procedure Set_Items_Visibility
     (Canvas : access Interactive_Canvas_Record'Class)
   is

      function Show_Internal
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class)
        return   Boolean
      is
      begin
         Set_Visibility (Item, True);
         return True;
      end Show_Internal;

   begin

      For_Each_Item (Canvas, Show_Internal'Unrestricted_Access);
      Refresh_Canvas (Canvas);

   end Set_Items_Visibility;

   ------------------
   -- Network_Copy --
   ------------------

   function Network_Copy
     (Net  : Packet_Based_Network)
     return Processing_Resource_Ref
   is
      Net_Copy    : Processing_Resource_Ref;
      Temp_Driver : Driver_Ref;
      Iterator    : Driver_Iteration_Object;
   begin
      Net_Copy := new Packet_Based_Network;
      Init (Net_Copy.all, Name (Net));
      Set_Speed_Factor (Net_Copy.all, Speed_Factor (Net));
      Set_Transmission_Mode
        (Packet_Based_Network (Net_Copy.all),
         Transmission_Mode (Net));
      Set_Max_Blocking
        (Packet_Based_Network (Net_Copy.all),
         Max_Blocking (Net));
      Set_Throughput (Packet_Based_Network (Net_Copy.all), Throughput (Net));
      Set_Max_Packet_Size
        (Packet_Based_Network (Net_Copy.all),
         Max_Packet_Size (Net));
      Set_Min_Packet_Size
        (Packet_Based_Network (Net_Copy.all),
         Min_Packet_Size (Net));

      Set_Max_Packet_Transmission_Time
        (Packet_Based_Network (Net_Copy.all),
         Max_Packet_Transmission_Time (Net));
      Set_Min_Packet_Transmission_Time
        (Packet_Based_Network (Net_Copy.all),
         Min_Packet_Transmission_Time (Net));

      Set_Scheduler_State (Net_Copy.all, Has_Scheduler (Net));

      Mast.Processing_Resources.Network.Rewind_Drivers (Net, Iterator);
      for I in 1 .. Mast.Processing_Resources.Network.Num_Of_Drivers (Net)
      loop
         Mast.Processing_Resources.Network.Get_Next_Driver
           (Net,
            Temp_Driver,
            Iterator);
         Add_Driver (Packet_Based_Network (Net_Copy.all), Temp_Driver);
      end loop;
      return Net_Copy;
   end Network_Copy;

   ----------------------
   -- Properties_Net  --
   ----------------------
   procedure Properties_Net
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Processing_Resource_Ref)
   is
      Net_Args       : Network_Args_Ref;
      Temp           : Processing_Resource_Ref;
      Network_Dialog : Network_Dialog_Access;
      Data           : Processing_Resource_And_Dialog_Ref :=
        new Processing_Resource_And_Dialog;
   begin
      if Item.Res /= null then
         if Item.Res.all'Tag = Packet_Based_Network'Tag then
            Temp := Network_Copy (Packet_Based_Network (Item.Res.all));
         end if;
      end if;

      Net_Args := new Network_Args;

      Gtk_New (Network_Dialog);
      Read_Parameters (Item, Gtk_Dialog (Network_Dialog));

      Data.It  := Temp;
      Data.Dia := Gtk_Dialog (Network_Dialog);

      Net_Args.Net := Temp;
      Net_Args.It  := Item;
      Net_Args.Dia := Gtk_Dialog (Network_Dialog);

      Processing_Resource_And_Dialog_Cb.Connect
        (Network_Dialog.Add_Driver_Button,
         "clicked",
         Processing_Resource_And_Dialog_Cb.To_Marshaller (Add_Driver'Access),
         Data);

      Processing_Resource_And_Dialog_Cb.Connect
        (Network_Dialog.Remove_Driver_Button,
         "clicked",
         Processing_Resource_And_Dialog_Cb.To_Marshaller
           (Remove_Driver'Access),
         Data);

      Canvas_Cb.Object_Connect
        (Network_Dialog.Network_Cancel_Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Set_Items_Visibility'Access),
         Proc_Res_Canvas);

      Dialog_Callback.Object_Connect
        (Network_Dialog.Network_Cancel_Button,
         "clicked",
         Dialog_Callback.To_Marshaller
           (On_Network_Cancel_Button_Clicked'Access),
         Network_Dialog);

      Network_Args_Cb.Connect
        (Network_Dialog.Network_Ok_Button,
         "clicked",
         Network_Args_Cb.To_Marshaller (Write_Network'Access),
         Net_Args);

      Destroy (Item_Menu);
   end Properties_Net;

   ---------------------
   -- On Button Click --
   ---------------------
   function On_Button_Click
     (Item  : access ME_Regular_Processor;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      Num_Button       : Guint;
      Event_Type       : Gdk_Event_Type;
      Processor_Dialog : Processor_Dialog_Access;
      Me_Data          : ME_Processing_Resource_And_Dialog_Ref :=
        new ME_Processing_Resource_And_Dialog;
   begin
      Event_Type := Event.The_Type;
      if Event_Type = Gdk_2Button_Press then
	 Num_Button := Event.Button;
	 if Num_Button = Guint (1) then
	    Gtk_New (Processor_Dialog);
	    Read_Parameters (Item, Gtk_Dialog (Processor_Dialog));
	    Me_Data.It  := ME_Processing_Resource_Ref (Item);
	    Me_Data.Dia := Gtk_Dialog (Processor_Dialog);
	    
	    Me_Processing_Resource_And_Dialog_Cb.Connect
	      (Processor_Dialog.Proc_Dialog_Ok_Button,
	       "clicked",
	       Me_Processing_Resource_And_Dialog_Cb.To_Marshaller
		 (Write_Processor'Access),
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
	       Button_Cb.To_Marshaller (Remove_Proc_Res'Access),
	       ME_Processing_Resource_Ref (Item));
	    Button_Cb.Connect
	      (Item_Menu.Properties,
	       "activate",
	       Button_Cb.To_Marshaller (Properties_Proc'Access),
	       ME_Processing_Resource_Ref (Item));
	    return True;
	 end if;
      end if;
      return False;
   end On_Button_Click;

   ---------------------
   -- On Button Click --
   ---------------------
   function On_Button_Click
     (Item  : access ME_Packet_Based_Network;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      Net_Args       : Network_Args_Ref;
      Temp           : Processing_Resource_Ref;
      Network_Dialog : Network_Dialog_Access;
      Data           : Processing_Resource_And_Dialog_Ref :=
        new Processing_Resource_And_Dialog;
      Num_Button     : Guint;
      Event_Type     : Gdk_Event_Type;
   begin
      Event_Type := Event.The_Type;
      if Event_Type = Gdk_2button_Press then
	 Num_Button := Event.Button;
	 if Num_Button = Guint (1) then

	    if Item.Res /= null then
	       if Item.Res.all'Tag = Packet_Based_Network'Tag then
		  Temp :=
		    Network_Copy (Packet_Based_Network (Item.Res.all));
	       end if;
	    end if;

	    Net_Args := new Network_Args;

	    Gtk_New (Network_Dialog);
	    Read_Parameters (Item, Gtk_Dialog (Network_Dialog));

	    Data.It  := Temp;
	    Data.Dia := Gtk_Dialog (Network_Dialog);

	    Net_Args.Net := Temp;
	    Net_Args.It  := ME_Processing_Resource_Ref (Item);
	    Net_Args.Dia := Gtk_Dialog (Network_Dialog);

	    Processing_Resource_And_Dialog_Cb.Connect
	      (Network_Dialog.Add_Driver_Button,
	       "clicked",
	       Processing_Resource_And_Dialog_Cb.To_Marshaller
		 (Add_Driver'Access),
	       Data);

	    Processing_Resource_And_Dialog_Cb.Connect
	      (Network_Dialog.Remove_Driver_Button,
	       "clicked",
	       Processing_Resource_And_Dialog_Cb.To_Marshaller
		 (Remove_Driver'Access),
	       Data);

	    Canvas_Cb.Object_Connect
	      (Network_Dialog.Network_Cancel_Button,
	       "clicked",
	       Canvas_Cb.To_Marshaller (Set_Items_Visibility'Access),
	       Proc_Res_Canvas);

	    Dialog_Callback.Object_Connect
	      (Network_Dialog.Network_Cancel_Button,
	       "clicked",
	       Dialog_Callback.To_Marshaller
		 (On_Network_Cancel_Button_Clicked'Access),
	       Network_Dialog);

	    Network_Args_Cb.Connect
	      (Network_Dialog.Network_Ok_Button,
	       "clicked",
	       Network_Args_Cb.To_Marshaller (Write_Network'Access),
	       Net_Args);
	    return True;
	 end if;
      elsif Event_Type = Button_Press then
	 Num_Button := Event.Button;
	 if Num_Button = Guint (3) then
	    Gtk_New (Item_Menu);
	    Button_Cb.Connect
	      (Item_Menu.Remove,
	       "activate",
	       Button_Cb.To_Marshaller (Remove_Proc_Res'Access),
	       ME_Processing_Resource_Ref (Item));
	    Button_Cb.Connect
	      (Item_Menu.Properties,
	       "activate",
	       Button_Cb.To_Marshaller (Properties_Net'Access),
	       ME_Processing_Resource_Ref (Item));
	    return True;
	 end if;
      end if;
      return False;
   end On_Button_Click;

   -----------------
   -- Simple Mode --
   -----------------
   procedure Simple_Mode (Canvas : access Interactive_Canvas_Record'Class) is
   begin
      Zoom (Canvas, Gdouble(Zoom_Levels (1))/100.0);
   end Simple_Mode;

   -----------------------
   -- Complete Mode Non --
   -----------------------
   procedure Complete_Mode_Non
     (Canvas : access Interactive_Canvas_Record'Class)
   is
   begin
      Zoom (Canvas, Gdouble(Zoom_Levels (2))/100.0);
   end Complete_Mode_Non;

   -----------------------
   -- Complete Mode Exp --
   -----------------------
   procedure Complete_Mode_Exp
     (Canvas : access Interactive_Canvas_Record'Class)
   is
   begin
      Zoom (Canvas, Gdouble(Zoom_Levels (3))/100.0);
   end Complete_Mode_Exp;

   -----------
   -- Print --
   -----------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      Item        : in out ME_Regular_Processor;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Mast_Editor.Processing_Resources.Print
        (File,
         ME_Processing_Resource (Item),
         Indentation);
      Put (File, " ");
      Put (File, "Me_Regular_Processor");
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

   -----------
   -- Print --
   -----------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      Item        : in out ME_Packet_Based_Network;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Mast_Editor.Processing_Resources.Print
        (File,
         ME_Processing_Resource (Item),
         Indentation);
      Put (File, " ");
      Put (File, "Me_Packet_Based_Network");
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

   ---------
   -- Run --
   ---------
   procedure Run (Frame : access Gtk.Frame.Gtk_Frame_Record'Class) is
      Box, Bbox, Cbox : Gtk_Box;
      Button          : Gtk_Button;
      Scrolled        : Gtk_Scrolled_Window;
      Vseparator      : Gtk_Vseparator;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);
      Gtk_New (Scrolled);
      Set_Border_Width (Scrolled, 5);
      Pack_Start (Box, Scrolled);
      Gtk_New_Hbox (Bbox, Homogeneous => False);
      Pack_Start (Box, Bbox, False, False, 4);

      Gtk_New_Hbox (Cbox, False, 10);
      Pack_Start (Bbox, Cbox, False, False, 4);

      Gtk_New (Proc_Res_Canvas);
      Configure
        (Proc_Res_Canvas,
         Grid_Size        => 0,
         Annotation_Font  => Pango.Font.From_String (Default_Annotation_Font),
         Arc_Link_Offset  => Default_Arc_Link_Offset,
         Arrow_Angle      => Default_Arrow_Angle,
         Arrow_Length     => Default_Arrow_Length,
         Motion_Threshold => Default_Motion_Threshold);

      Add (Scrolled, Proc_Res_Canvas);

      Gtk_New (Button, "    Simple   " & ASCII.LF & "     Mode   ");
      Pack_Start (Cbox, Button, False, True, 0);
      Canvas_Cb.Object_Connect
        (Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Simple_Mode'Access),
         Proc_Res_Canvas);

      Gtk_New (Button, " Complete Mode" & ASCII.LF & "(Non-Expanded)");
      Pack_Start (Cbox, Button, False, True, 0);
      Canvas_Cb.Object_Connect
        (Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Complete_Mode_Non'Access),
         Proc_Res_Canvas);

      Gtk_New (Button, "Complete Mode" & ASCII.LF & "  (Expanded) ");
      Pack_Start (Cbox, Button, False, True, 0);
      Canvas_Cb.Object_Connect
        (Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Complete_Mode_Exp'Access),
         Proc_Res_Canvas);

      Gtk_New_Vseparator (Vseparator);
      Pack_Start (Bbox, Vseparator, False, False, 5);

      Gtk_New (Button, "     New   " & ASCII.LF & " Processor ");
      Pack_Start (Bbox, Button, False, True, 4);
      Button_Callback.Connect
        (Button,
         "clicked",
         Button_Callback.To_Marshaller (Show_Processor_Dialog'Access));

      Gtk_New (Button, "    New   " & ASCII.LF & "  Network ");
      Pack_Start (Bbox, Button, False, True, 4);
      Button_Callback.Connect
        (Button,
         "clicked",
         Button_Callback.To_Marshaller (Show_Network_Dialog'Access));

      Gtk_New (Button, "New Primary" & ASCII.LF & " Scheduler");
      Pack_Start (Bbox, Button, False, True, 4);
      Button_Callback.Connect
        (Button,
         "clicked",
         Button_Callback.To_Marshaller (Show_Primary_Dialog'Access));

      Gtk_New (Button, "New Secondary" & ASCII.LF & "   Scheduler");
      Pack_Start (Bbox, Button, False, True, 4);
      Button_Callback.Connect
        (Button,
         "clicked",
         Button_Callback.To_Marshaller (Show_Secondary_Dialog'Access));

      Realize (Proc_Res_Canvas);

      Editor_Actions.Load_System_Font (Font, Font1);
      Font_Size:=Get_Size(Font);
      P_Layout := Frame.Create_Pango_Layout;
      Set_Font_Description (P_Layout, Font);
      Line_Height:=Gdouble(P_Layout.Get_Baseline/Pango_Scale);
      
      Show_All (Frame);
   end Run;
end Mast_Editor.Processing_Resources;
