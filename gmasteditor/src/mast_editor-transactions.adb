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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Tags;                use Ada.Tags;
with Callbacks_Mast_Editor; use Callbacks_Mast_Editor;
with Gtk.Alignment;         use Gtk.Alignment;
with Gdk.RGBA;      use Gdk.RGBA;
with Pango.Font;     use Pango.Font;
with Pango.Layout;   use Pango.Layout;
with Pango.Cairo; use Pango.Cairo;
with Pango.Enums; use Pango.Enums;
with Cairo;          use Cairo;
with Cairo.Region; use Cairo.Region;
with Gdk.Cairo;      use Gdk.Cairo;
with Utilities; use Utilities;
with Mast_Editor_Window_Pkg;   use Mast_Editor_Window_Pkg;
with Gtk.Box;               use Gtk.Box;
with Gtk.Button;            use Gtk.Button;
with Gtk.Frame;             use Gtk.Frame;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.GEntry;            use Gtk.GEntry;
with Gtk.Label;             use Gtk.Label;
with Gtk.Scrolled_Window;   use Gtk.Scrolled_Window;
with Gtk.Separator;         use Gtk.Separator;
with Gtk.Table;             use Gtk.Table;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Window;            use Gtk.Window;
with Gtkada.Canvas;         use Gtkada.Canvas;
with Pango.Font;            use Pango.Font;
with Gtkada.Dialogs;        use Gtkada.Dialogs;
with Gtk.Enums;

with List_Exceptions;            use List_Exceptions;
with Mast.Graphs;                use Mast.Graphs;
with Mast.Graphs.Event_Handlers; use Mast.Graphs.Event_Handlers;
with Mast.IO;                    use Mast.IO;
with Mast.Transactions;          use Mast.Transactions;
with Mast_Editor.Event_Handlers; use Mast_Editor.Event_Handlers;
with Mast_Editor.Links;          use Mast_Editor.Links;
with Editor_Error_Window_Pkg;    use Editor_Error_Window_Pkg;
with Item_Menu_Pkg;              use Item_Menu_Pkg;
with Trans_Dialog_Pkg;           use Trans_Dialog_Pkg;
with Mast_Editor.Event_Handlers; use Mast_Editor.Event_Handlers;
with Mast_Editor.Links;          use Mast_Editor.Links;
with Editor_Actions;             use Editor_Actions;
with Change_Control;
with Cut_Strings;                   use Cut_Strings;
with Mast;                          use Mast;
with Mast.Events;                   use Mast.Events;
with Mast.Timing_Requirements;      use Mast.Timing_Requirements;
with Mast.Graphs.Links;             use Mast.Graphs.Links;
with Callbacks_Mast_Editor;         use Callbacks_Mast_Editor;
with Var_Strings;                   use Var_Strings;
with Internal_Dialog_Pkg;           use Internal_Dialog_Pkg;
with Internal_Dialog_Pkg.Callbacks; use Internal_Dialog_Pkg.Callbacks;
with External_Dialog_Pkg;           use External_Dialog_Pkg;
with Add_Link_Dialog_Pkg;           use Add_Link_Dialog_Pkg;
with Select_Ref_Event_Dialog_Pkg;   use Select_Ref_Event_Dialog_Pkg;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;               use Ada.Strings.Unbounded;

package body Mast_Editor.Transactions is

   package Canvas_Cb is new Gtk.Handlers.Callback
     (Interactive_Canvas_Record);

   package Button_Cb is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Widget_Record,
      User_Type => ME_Transaction_Ref);


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

   --------------
   -- Name     --
   --------------
   function Name (Item : in ME_Transaction) return Var_String is
   begin
      return Name (Item.Tran);
   end Name;

   --------------
   -- Name     --
   --------------
   function Name (Item_Ref : in ME_Transaction_Ref) return Var_String is
   begin
      return Name (Item_Ref.Tran);
   end Name;

   -----------------
   -- Print       --
   -----------------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      Item        : in out ME_Transaction;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Ada.Text_IO.Set_Col (File, Ada.Text_IO.Count (Indentation));
      Ada.Text_IO.Put (File, "ME_Transaction");
   end Print;

   -----------------
   -- Print       --
   -----------------
   procedure Print
     (File        : Ada.Text_IO.File_Type;
      The_List    : in out Lists.List;
      Indentation : Positive)
   is
      Item_Ref : ME_Transaction_Ref;
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
   procedure Write_Parameters (Item : access ME_Regular_Transaction) is
      Tran_Ref : Transaction_Ref := ME_Transaction_Ref (Item).Tran;
   begin
      Change_Control.Changes_Made;
      Init
        (Tran_Ref.all,
         Var_Strings.To_Lower
           (To_Var_String (Get_Text (Item.Dialog.Trans_Name_Entry))));
   end Write_Parameters;

   ---------------------
   -- Read Parameters --
   ---------------------
   procedure Read_Parameters (Item : access ME_Regular_Transaction) is
      Tran_Ref  : Transaction_Ref := Item.Tran;
      Tran_Name : String          := To_String (Name (Tran_Ref));
   begin
      if Item.Dialog = null then
         Gtk_New (Item.Dialog);
         -- Since we create a new dialog, we should connect button signals
         --manually
         Button_Cb.Connect
           (Item.Dialog.Add_Ext_Button,
            "clicked",
            Button_Cb.To_Marshaller
              (Mast_Editor.Links.Show_External_Dialog'Access),
            ME_Transaction_Ref (Item));
         Button_Cb.Connect
           (Item.Dialog.Add_Int_Button,
            "clicked",
            Button_Cb.To_Marshaller
              (Mast_Editor.Links.Show_Internal_Dialog'Access),
            ME_Transaction_Ref (Item));
         Button_Cb.Connect
           (Item.Dialog.Add_Simple_Button,
            "clicked",
            Button_Cb.To_Marshaller
              (Mast_Editor.Event_Handlers.Show_Simple_Event_Handler_Dialog'
                 Access),
            ME_Transaction_Ref (Item));
         Button_Cb.Connect
           (Item.Dialog.Add_Minput_Button,
            "clicked",
            Button_Cb.To_Marshaller
            (Mast_Editor.Event_Handlers.Show_Multi_Input_Event_Handler_Dialog'
             Access),
            ME_Transaction_Ref (Item));
         Button_Cb.Connect
           (Item.Dialog.Add_Moutput_Button,
            "clicked",
            Button_Cb.To_Marshaller
            (Mast_Editor.Event_Handlers.Show_Multi_Output_Event_Handler_Dialog'
             Access),
            ME_Transaction_Ref (Item));
         -- Put all the elements of the graph in the dialog
         Mast_Editor.Transactions.Draw_External_Events_In_Tran_Canvas
           (ME_Transaction_Ref (Item));
         Mast_Editor.Transactions.Draw_Internal_Events_In_Tran_Canvas
           (ME_Transaction_Ref (Item));
         Mast_Editor.Transactions.Draw_Event_Handlers_In_Tran_Canvas
           (ME_Transaction_Ref (Item));
         Refresh_Canvas (ME_Transaction_Ref (Item).Dialog.Trans_Canvas);
         Editor_Actions.Show_TXT_Items
           (ME_Transaction_Ref (Item).Dialog.Trans_Canvas);

      end if;
      Set_Text (Item.Dialog.Trans_Name_Entry, Tran_Name);
      Show_All (Item.Dialog);
      -- Change size; for some reason the size of the canvas is not computed
      -- correctly the first time it is drawn.
      declare
         W,H : Gint;
      begin
         Get_Size(Item.Dialog,W,H);
         --Resize(Item.Dialog,W+10,H+10);
      end;
   end Read_Parameters;

   
   ----------------------
   -- Draw Transaction --
   ----------------------
   procedure Draw
     (Item         : access ME_Regular_Transaction;
      Cr   : Cairo_Context)
   is
      Rect      : constant Cairo.Region.Cairo_Rectangle_Int  := 
	Get_Coord (Item);
      W         : constant Gdouble        :=Gdouble(Rect.Width);
      H         : constant Gdouble        :=Gdouble(Rect.Height);
      Color,Black : Gdk_RGBA;
      Layout : Pango.Layout.Pango_Layout;
      Success : Boolean;
      Tran_Ref  : Transaction_Ref        := Item.Tran;
      Tran_Name : String                 := To_String (Name (Tran_Ref));
      Lin_Ref         : Mast.Graphs.Link_Ref;
      Lin_Iterator    : Mast.Transactions.Link_Iteration_Object;
      Eve_Ref         : Mast.Events.Event_Ref;
      Req_Ref         : Mast.Timing_Requirements.Timing_Requirement_Ref;
      --Pi, Pe  : Gint := 0;
      More_Data       : Boolean:=False;
      Line_Count      : Integer:=0;
      Max_Lines       : Integer;
      Deline_Max      : Time := 0.000;
      Jitter          : Time := 0.000;
      Type_Of_D       : String := "Hard deadline: D=";
      Comp_Req        : Composite_Timing_Req;
      Req             : Simple_Timing_Requirement_Ref;
      Iter            : Mast.Timing_Requirements.Iteration_Object;
      Text            : Unbounded_String;
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

      -- Simple Mode
      if Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (1) then
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(1)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(1)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
	 Max_Lines :=2;
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text
	   (Layout,
	    Cut (Tran_Name, "REGULAR TRANSACTION", "TRANSACTION", 22));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String("");
	 
         -- external events

         Mast.Transactions.Rewind_External_Event_Links
           (Item.Tran.all,Lin_Iterator);

         for I in
           1 .. Mast.Transactions.Num_Of_External_Event_Links (Item.Tran.all)
         loop

            Mast.Transactions.Get_Next_External_Event_Link
              (Item.Tran.all,
               Lin_Ref,
               Lin_Iterator);

            if Lin_Ref /= null and then Event_Of (Lin_Ref.all) /= null then
               Eve_Ref := Event_Of (Lin_Ref.all);
               if Eve_Ref.all in Mast.Events.Periodic_Event'class
               then
		  if Line_Count<Max_Lines then
		     Text:=Text&"Periodic: T=" &
		       Time_Image (Period (Periodic_Event (Eve_Ref.all)))&NL;
		  else
		     More_Data:=True;
		  end if;
                  Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Singular_Event'class
               then
		  if Line_Count<Max_Lines then
		     Text:=Text&"Singular"&NL;
		  else
		     More_Data:=True;
		  end if;
                  Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Sporadic_Event'class
               then
		  if Line_Count<Max_Lines then
		     Text:=Text&"Sporadic: MIT=" &
		       Time_Image (Min_Interarrival
				     (Sporadic_Event (Eve_Ref.all)))&NL;
		  else
		     More_Data:=True;
		  end if;
                  Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Unbounded_Event'class
               then
		  if Line_Count<Max_Lines then
		     Text:=Text&"Unbounded"&NL;
		  else
		     More_Data:=True;
		  end if;
                  Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Bursty_Event'class
               then
		  if Line_Count<Max_Lines then
		     Text:=Text&"Bursty"&NL;
		  else
		     More_Data:=True;
		  end if;
                  Line_Count := Line_Count + 1;
	       end if;
	       end if;
	       exit when Line_Count>Max_Lines;
         end loop;

         -- end external events

         -- internal events

         Mast.Transactions.Rewind_Internal_Event_Links
           (Item.Tran.all, Lin_Iterator);

         for I in
           1 .. Mast.Transactions.Num_Of_Internal_Event_Links (Item.Tran.all)
         loop
            Mast.Transactions.Get_Next_Internal_Event_Link
              (Item.Tran.all,
               Lin_Ref,
               Lin_Iterator);

            if Mast.Graphs.Links.Has_Timing_Requirements
              (Regular_Link (Lin_Ref.all))
            then
               if Lin_Ref /= null and then Event_Of (Lin_Ref.all) /= null then
                  Eve_Ref := Event_Of (Lin_Ref.all);
                  Req_Ref := Mast.Graphs.Links.Link_Timing_Requirements
                    (Regular_Link (Lin_Ref.all));
                  if Req_Ref.all in Deadline'Class then
		     if Line_Count<Max_Lines then
			Text:=Text&"D=" & Time_Image 
			  (The_Deadline
			     (Deadline (Req_Ref.all)))&NL;
		     else
			More_Data:=True;
		     end if;
                  Line_Count := Line_Count + 1;
                  elsif Req_Ref.all in Max_Output_Jitter_Req'class
                  then
		     if Line_Count<Max_Lines then
			Text:=Text&"J=" &
			  Time_Image (Max_Output_Jitter
					(Max_Output_Jitter_Req
					   (Req_Ref.all)))&NL;
		     else
			More_Data:=True;
		     end if;
                  Line_Count := Line_Count + 1;
                  elsif (Req_Ref.all in Composite_Timing_Req'class)
                  then
                     Deline_Max := 0.0;
                     Comp_Req := Composite_Timing_Req (Req_Ref.all);
                     MAST.Timing_Requirements.Rewind_Requirements
                       (Comp_Req, Iter);

                     -- Deline_max takes the maximum value from the list

                     for I in 1 .. Num_Of_Requirements (Comp_Req)
                     loop
                        Mast.Timing_Requirements.Get_Next_Requirement
                          (Comp_Req,
                           Req,
                           Iter);

                        if Req.all in Deadline'Class then

                           if Deline_max < The_Deadline (Deadline (Req.all))
                           then
                              Deline_max:= The_Deadline (Deadline (Req.all));
                           end if;

                        elsif Req.all in Max_Output_Jitter_Req'class then

                           Jitter := Max_Output_Jitter (Max_Output_Jitter_Req
                                                          (Req.all));
                        end if;
                     end loop;

                     if Deline_Max > 0.00 then
			if Line_Count<Max_Lines then
			   Text:=Text&"D=" & Time_Image (Deline_max)&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     elsif Jitter > 0.00 then
			if Line_Count<Max_Lines then
			   Text:=Text&"J=" & Time_Image (Jitter)&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     end if;
                  end if;
               end if;
            end if;
	    exit when Line_Count>Max_Lines;
         end loop;

         -- end internal event

         if (More_Data)  then
	    Text:=Text&"...";
         end if;
	 
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
         -- end Simple Mode

         -- Complete Mode

      elsif Guint(Get_Zoom (Item.Canvas)*100.0) = Zoom_Levels (2) then
	 
	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(2)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(2)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
         Max_Lines:=3;
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text
	   (Layout,
	    Cut (Tran_Name, "REGULAR TRANSACTION", "TRANSACTION", 30));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String("");

         -- external events

         Mast.Transactions.Rewind_External_Event_Links
           (Item.Tran.all, Lin_Iterator);

         for I in
           1 .. Mast.Transactions.Num_Of_External_Event_Links (Item.Tran.all)
         loop

            Mast.Transactions.Get_Next_External_Event_Link
              (Item.Tran.all,
               Lin_Ref,
               Lin_Iterator);


            if Lin_Ref /= null and then Event_Of (Lin_Ref.all) /= null then
               Eve_Ref := Event_Of (Lin_Ref.all);


               if Eve_Ref.all in Mast.Events.Periodic_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Periodic: T=" & 
		    Time_Image (Period
				  (Periodic_Event
				     (Eve_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Singular_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Singular"&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Sporadic_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Sporadic: MIT=" & Time_Image 
		    (Min_Interarrival
		       (Sporadic_Event
			  (Eve_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Unbounded_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Unbounded"&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Bursty_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Bursty: Int=" &
                          Time_Image (Bound_Interval
                                      (Bursty_Event (Eve_Ref.all))) &
                          ", N=" &
                          Integer'Image (Max_Arrivals
                                         (Bursty_Event (Eve_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               end if;
            end if;
	    exit when Line_Count>Max_Lines;
         end loop;
         -- end external events

         -- internal events

         Mast.Transactions.Rewind_Internal_Event_Links
           (Item.Tran.all, Lin_Iterator);

         for I in
           1 .. Mast.Transactions.Num_Of_Internal_Event_Links (Item.Tran.all)
         loop
            Mast.Transactions.Get_Next_Internal_Event_Link
              (Item.Tran.all,
               Lin_Ref,
               Lin_Iterator);

            if Mast.Graphs.Links.Has_Timing_Requirements
              (Regular_Link (Lin_Ref.all))
            then
               if Lin_Ref /= null and then Event_Of (Lin_Ref.all) /= null then
                  Eve_Ref := Event_Of (Lin_Ref.all);
                  Req_Ref :=   Mast.Graphs.Links.Link_Timing_Requirements
                    (Regular_Link (Lin_Ref.all));

                  if Req_Ref.all in Deadline'Class then
                     if (Req_Ref.all in Hard_Local_Deadline'class or
                            Req_Ref.all in Hard_Global_Deadline'class)
                     then
			if Line_Count<Max_Lines then
			Text:=Text&"Hard Deadline: D=" & Time_Image
                                (The_Deadline (Deadline (Req_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     elsif (Req_Ref.all in Soft_Local_Deadline'class or
                            Req_Ref.all in Soft_Global_Deadline'class)
                     then
			if Line_Count<Max_Lines then
			Text:=Text&
			  "Soft Deadline: D=" & Time_Image
			  (The_Deadline (Deadline (Req_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     end if;
                  elsif Req_Ref.all in Max_Output_Jitter_Req'class
                  then
			if Line_Count<Max_Lines then
		     Text:=Text&"Max Jitter: J=" &
                             Time_Image (Max_Output_Jitter
                                         (Max_Output_Jitter_Req
                                          (Req_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                  elsif (Req_Ref.all in Composite_Timing_Req'class)
                  then
                     Deline_Max := 0.0;
                     Comp_Req := Composite_Timing_Req (Req_Ref.all);
                     MAST.Timing_Requirements.Rewind_Requirements
                       (Comp_Req, Iter);

                     -- Deline_max takes the maximum value from the list

                     for I in 1 .. Num_Of_Requirements (Comp_Req)
                     loop
                        Mast.Timing_Requirements.Get_Next_Requirement
                          (Comp_Req,
                           Req,
                           Iter);

                        if Req.all in Deadline'Class then

                           if Deline_max < The_Deadline (Deadline (Req.all))
                           then
                              Deline_max:= The_Deadline (Deadline (Req.all));

                              if (Req.all in Hard_Local_Deadline'class or
                                    Req.all in Hard_Global_Deadline'class) then

                                 Type_Of_D := "Hard deadline: D=";

                              elsif (Req.all in Soft_Local_Deadline'class or
                                       Req.all in Soft_Global_Deadline'class)
                              then
                                 Type_Of_D := "Soft deadline: D=";
                              end if;
                           end if;

                        elsif (Req.all in Max_Output_Jitter_Req'class) then

                           Jitter := Max_Output_Jitter (Max_Output_Jitter_Req
                                                        (Req.all));
                        end if;
                     end loop;

                     if Deline_Max > 0.00 then
			if Line_Count<Max_Lines then
                        Text:=Text&Type_Of_D & Time_Image (Deline_max)&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     elsif Jitter > 0.00 then
			if Line_Count<Max_Lines then
			Text:=Text&"Max Jitter: J=" & Time_Image (Jitter)&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     end if;
                  end if;
               end if;
            end if;
	    exit when Line_Count>Max_Lines;
         end loop;

         -- end internal events

         if (More_Data)  then
	    Text:=Text&"...";
         end if;
	 
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

         -- End Complete Mode

         -- Mode Complete (Expanded)

      else

	 Set_Size(Font,Font_Size*100/Gint(Zoom_Levels(3)));
	 Set_Size(Font1,Font_Size*100/Gint(Zoom_Levels(3)));
	 Pango.Layout.Set_Width(Layout,Gint(W)*Pango_Scale);
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Center);
	 Pango.Layout.Set_Ellipsize(Layout,Ellipsize_End);
         Max_Lines:=5;
	 
	 Set_Font_Description (Layout, Font1);
	 Set_Text
	   (Layout,
	    Cut (Tran_Name, "REGULAR TRANSACTION", "TRANSACTION", 35));
	 Cairo.Move_To (Cr, 0.0, 2.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);
	 
	 Text:=To_Unbounded_String("");

         -- External Events

         Mast.Transactions.Rewind_External_Event_Links (Item.Tran.all,
                                                        Lin_Iterator);

         for I in 1 ..
           Mast.Transactions.Num_Of_External_Event_Links (Item.Tran.all)
         loop

            Mast.Transactions.Get_Next_External_Event_Link
              (Item.Tran.all,
               Lin_Ref,
               Lin_Iterator);

            if Lin_Ref /= null and then Event_Of (Lin_Ref.all) /= null then
               Eve_Ref := Event_Of (Lin_Ref.all);

               if Eve_Ref.all in Mast.Events.Periodic_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Periodic: T=" & Time_Image 
		    (Period
		       (Periodic_Event
			  (Eve_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Singular_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Singular"&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Sporadic_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Sporadic: MIT=" & Time_Image 
		    (Min_Interarrival
		       (Sporadic_Event
			  (Eve_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Unbounded_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Unbounded"&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               elsif Eve_Ref.all in Mast.Events.Bursty_Event'class
               then
			if Line_Count<Max_Lines then
		  Text:=Text&"Bursty: Int=" &
                            Time_Image (Bound_Interval
                                        (Bursty_Event (Eve_Ref.all))) &
                          ", Max_arr=" &
                          Integer'Image (Max_Arrivals
                                         (Bursty_Event (Eve_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
               end if;
            end if;
            exit when Line_Count>Max_Lines;
         end loop;

         -- End External Events

         -- Internal Events

         Mast.Transactions.Rewind_Internal_Event_Links (Item.Tran.all,
                                                        Lin_Iterator);
         for I in
           1 ..
           Mast.Transactions.Num_Of_Internal_Event_Links (Item.Tran.all)
         loop

            Mast.Transactions.Get_Next_Internal_Event_Link
              (Item.Tran.all,
               Lin_Ref,
               Lin_Iterator);

            if Mast.Graphs.Links.Has_Timing_Requirements(Regular_Link
                                                         (Lin_Ref.all))
            then

               if Lin_Ref /= null and then Event_Of (Lin_Ref.all) /= null then
                  Eve_Ref := Event_Of (Lin_Ref.all);
                  Req_Ref :=   Mast.Graphs.Links.Link_Timing_Requirements
                    (Regular_Link (Lin_Ref.all));


                  if Req_Ref.all in Deadline'Class then

                     if (Req_Ref.all in Hard_Local_Deadline'class or
                            Req_Ref.all in Hard_Global_Deadline'class)
                     then
			if Line_Count<Max_Lines then
			Text:=Text&"Hard Deadline: D=" &
                                Time_Image (The_Deadline
                                            (Deadline (Req_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     elsif (Req_Ref.all in Soft_Local_Deadline'class or
                            Req_Ref.all in Soft_Global_Deadline'class)
                     then
			if Line_Count<Max_Lines then
			Text:=Text&"Soft Deadline: D=" &
                                Time_Image (The_Deadline
                                            (Deadline (Req_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     end if;
                  elsif Req_Ref.all in Max_Output_Jitter_Req'class
                  then
			if Line_Count<Max_Lines then
		     Text:=Text&"Max Jitter: J=" &
                             Time_Image (Max_Output_Jitter
                                         (Max_Output_Jitter_Req
                                          (Req_Ref.all)))&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                  elsif (Req_Ref.all in Composite_Timing_Req'class)
                  then
                     Deline_Max := 0.0;
                     Comp_Req := Composite_Timing_Req (Req_Ref.all);
                     MAST.Timing_Requirements.Rewind_Requirements
                       (Comp_Req, Iter);

                     -- Deline_max takes the maximum value from the list

                     for I in 1 .. Num_Of_Requirements (Comp_Req)
                     loop
                        Mast.Timing_Requirements.Get_Next_Requirement
                          (Comp_Req,
                           Req,
                           Iter);

                        if Req.all in Deadline'Class then

                           if Deline_max < The_Deadline (Deadline (Req.all))
                           then
                              Deline_max:= The_Deadline (Deadline (Req.all));

                              if (Req.all in Hard_Local_Deadline'class or
                                  Req.all in Hard_Global_Deadline'class) then

                                 Type_Of_D := "Hard deadline: D=";

                              elsif (Req.all in Soft_Local_Deadline'class or
                                     Req.all in Soft_Global_Deadline'class)
                              then
                                 Type_Of_D := "Soft deadline: D=";
                              end if;

                           end if;

                        elsif (Req.all in Max_Output_Jitter_Req'class) then

                           Jitter := Max_Output_Jitter (Max_Output_Jitter_Req
                                                        (Req.all));
                        end if;
                     end loop;

                     if Deline_Max > 0.00 then
			if Line_Count<Max_Lines then
			Text:=Text&Type_Of_D & Time_Image (Deline_max)&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     elsif Jitter > 0.00 then
			if Line_Count<Max_Lines then
			Text:=Text&"Max Jitter: J=" & Time_Image (Jitter)&NL;
			else
			   More_Data:=True;
			end if;
			Line_Count := Line_Count + 1;
                     end if;
                  end if;
               end if;
            end if;
            exit when Line_Count>Max_Lines;
         end loop;

         -- End Internal Events

         if (More_Data)  then
	    Text:=Text&"...";
         end if;
	 Pango.Layout.Set_Alignment(Layout,Pango_Align_Left);
	 Set_Font_Description (Layout, Font);
	 Set_Text (Layout,To_String(Text));
	 Cairo.Move_To (Cr, 4.0, Line_Height+8.0);
	 Pango.Cairo.Show_Layout (Cr, Layout);

         -- End Complete Mode (Expanded)

      end if;
   end Draw;

   -----------------------------------------
   -- Draw_External_Events_In_Tran_Canvas --
   -----------------------------------------
   procedure Draw_External_Events_In_Tran_Canvas
     (Item         : ME_Transaction_Ref;
      Use_TXT_File : Boolean := False)
   is
      Lin_Ref      : Mast.Graphs.Link_Ref;
      Lin_Iterator : Mast.Transactions.Link_Iteration_Object;
      Me_Lin_Ref   : Mast_Editor.Links.ME_Link_Ref;
   begin
      Mast.Transactions.Rewind_External_Event_Links
        (Item.Tran.all,
         Lin_Iterator);
      for I in
        1 ..
        Mast.Transactions.Num_Of_External_Event_Links (Item.Tran.all)
      loop
         Mast.Transactions.Get_Next_External_Event_Link
           (Item.Tran.all,
            Lin_Ref,
            Lin_Iterator);
         -- we create a new ME_External_Link, draw it and add it to Me_Links
         --list
         Me_Lin_Ref             := new ME_External_Link;
         Me_Lin_Ref.Name        :=
           (Name (Lin_Ref) & Delimiter & Name (Item));
         Me_Lin_Ref.W           := Link_Width;
         Me_Lin_Ref.H           := Link_Height;
         Me_Lin_Ref.Color_Name  := Ext_Link_Color;
         Me_Lin_Ref.Canvas_Name := Name (Item);
         Me_Lin_Ref.Lin         := Lin_Ref;
         Me_Lin_Ref.ME_Tran     := Item;
         begin
            Mast_Editor.Links.Lists.Add (Me_Lin_Ref, Editor_System.Me_Links);
            Set_Screen_Size (Me_Lin_Ref, Me_Lin_Ref.W, Me_Lin_Ref.H);
            Put (Item.Dialog.Trans_Canvas, Me_Lin_Ref);
            Refresh_Canvas (Item.Dialog.Trans_Canvas);
            Show_Item (Item.Dialog.Trans_Canvas, Me_Lin_Ref);
         exception
            when Already_Exists =>
               if not Use_TXT_File then
                  Gtk_New 
		    (Editor_Error_Window, Gtk_Window(Item.Dialog));
                  Set_Text
                    (Editor_Error_Window.Label,
                     "External Event " &
                       To_String (Name (Lin_Ref)) &
                       " Already Exists !!!");
                  Show_All (Editor_Error_Window);
               else
                  null;
               end if;
         end;
      end loop;
   end Draw_External_Events_In_Tran_Canvas;

   -----------------------------------------
   -- Draw_Internal_Events_In_Tran_Canvas --
   -----------------------------------------
   procedure Draw_Internal_Events_In_Tran_Canvas
     (Item         : ME_Transaction_Ref;
      Use_TXT_File : Boolean := False)
   is
      Lin_Ref      : Mast.Graphs.Link_Ref;
      Lin_Iterator : Mast.Transactions.Link_Iteration_Object;
      Me_Lin_Ref   : Mast_Editor.Links.ME_Link_Ref;
   begin
      Mast.Transactions.Rewind_Internal_Event_Links
        (Item.Tran.all,
         Lin_Iterator);
      for I in
        1 ..
        Mast.Transactions.Num_Of_Internal_Event_Links (Item.Tran.all)
      loop
         Mast.Transactions.Get_Next_Internal_Event_Link
           (Item.Tran.all,
            Lin_Ref,
            Lin_Iterator);
         -- we create a new ME_Internal_Link, draw it and add it to Me_Links
         --list
         Me_Lin_Ref             := new ME_Internal_Link;
         Me_Lin_Ref.Name        :=
           (Name (Lin_Ref) & Delimiter & Name (Item));
         Me_Lin_Ref.W           := Link_Width;
         Me_Lin_Ref.H           := Link_Height;
         Me_Lin_Ref.Color_Name  := Int_Link_Color;
         Me_Lin_Ref.Canvas_Name := Name (Item);
         Me_Lin_Ref.Lin         := Lin_Ref;
         Me_Lin_Ref.ME_Tran     := Item;
         begin
            Mast_Editor.Links.Lists.Add (Me_Lin_Ref, Editor_System.Me_Links);
            Set_Screen_Size (Me_Lin_Ref, Me_Lin_Ref.W, Me_Lin_Ref.H);
            Put (Item.Dialog.Trans_Canvas, Me_Lin_Ref);
            Show_Item (Item.Dialog.Trans_Canvas, Me_Lin_Ref);
         exception
            when Already_Exists =>
               if not Use_TXT_File then
                  Gtk_New (Editor_Error_Window, Gtk_Window(Item.Dialog));
                  Set_Text
                    (Editor_Error_Window.Label,
                     "Internal Event " &
                       To_String (Name (Lin_Ref)) &
                       " Already Exists !!!");
                  Show_All (Editor_Error_Window);
               else
                  null;
               end if;
         end;
      end loop;
   end Draw_Internal_Events_In_Tran_Canvas;

   ----------------------------------------
   -- Draw_Event_Handlers_In_Tran_Canvas --
   ----------------------------------------
   procedure Draw_Event_Handlers_In_Tran_Canvas
     (Item         : ME_Transaction_Ref;
      Use_TXT_File : Boolean := False)
   is
      Han_Ref      : Mast.Graphs.Event_Handler_Ref;
      Han_Iterator : Mast.Transactions.Event_Handler_Iteration_Object;
      Me_Han_Iter  : Mast_Editor.Event_Handlers.Lists.Iteration_Object;
      Me_Han_Ref   : Mast_Editor.Event_Handlers.ME_Event_Handler_Ref;
      Lin_Ref      : Mast.Graphs.Link_Ref;
      Link_Iter    : Mast.Graphs.Event_Handlers.Iteration_Object;
      Me_Lin_Iter  : Mast_Editor.Links.Lists.Iteration_Object;
      Me_Lin_Ref   : Mast_Editor.Links.ME_Link_Ref;
   begin
      Mast.Transactions.Rewind_Event_Handlers (Item.Tran.all, Han_Iterator);
      for I in 1 .. Mast.Transactions.Num_Of_Event_Handlers (Item.Tran.all)
      loop
         Mast.Transactions.Get_Next_Event_Handler
           (Item.Tran.all,
            Han_Ref,
            Han_Iterator);
         -- we check Handler type and create a new ME_Event_Handler according
         --to it
         if Han_Ref.all in Mast.Graphs.Event_Handlers.Simple_Event_Handler'
           Class
         then
            Me_Han_Ref := new ME_Simple_Event_Handler;
         elsif Han_Ref.all in Mast.Graphs.Event_Handlers.Input_Event_Handler'
           Class
         then
            Me_Han_Ref := new ME_Multi_Input_Event_Handler;
         elsif Han_Ref.all in Mast.Graphs.Event_Handlers.Output_Event_Handler'
           Class
         then
            Me_Han_Ref := new ME_Multi_Output_Event_Handler;
         end if;
         Me_Han_Ref.Id          := I;
         Me_Han_Ref.Name        :=
           (Natural'Image (I) & Delimiter & Name (Item));
         Me_Han_Ref.W           := Handler_Width;
         Me_Han_Ref.H           := Handler_Height;
         Me_Han_Ref.Color_Name  := Handler_Color;
         Me_Han_Ref.Canvas_Name := Name (Item);
         Me_Han_Ref.Han         := Han_Ref;
         Me_Han_Ref.ME_Tran     := Item;
         -- we draw handler and add it to Me_Event_Handlers list
         begin
            Mast_Editor.Event_Handlers.Lists.Add
              (Me_Han_Ref,
               Editor_System.Me_Event_Handlers);
            Set_Screen_Size (Me_Han_Ref, Me_Han_Ref.W, Me_Han_Ref.H);
            Put (Item.Dialog.Trans_Canvas, Me_Han_Ref);
            --Refresh_Canvas (Item.Dialog.Trans_Canvas); 
            Show_Item (Item.Dialog.Trans_Canvas, Me_Han_Ref);
         exception
            when Already_Exists =>
               Me_Han_Iter :=
                 Mast_Editor.Event_Handlers.Lists.Find
                 ((Natural'Image (I) & Delimiter & Name (Item)),
                  Editor_System.Me_Event_Handlers);
               Me_Han_Ref  :=
                 Mast_Editor.Event_Handlers.Lists.Item
                 (Me_Han_Iter,
                  Editor_System.Me_Event_Handlers);
               Editor_Actions.Remove_Old_Links
                 (Item.Dialog.Trans_Canvas,
                  Me_Han_Ref,
                  True);
               Editor_Actions.Remove_Old_Links
                 (Item.Dialog.Trans_Canvas,
                  Me_Han_Ref,
                  False);
               if not Use_TXT_File then
                  Gtk_New (Editor_Error_Window, Gtk_Window(Item.Dialog));
                  Set_Text
                    (Editor_Error_Window.Label,
                     "Event Handler Already Exists !!!");
                  Show_All (Editor_Error_Window);
               else
                  null;
               end if;
         end;

         if Han_Ref.all in Mast.Graphs.Event_Handlers.Simple_Event_Handler'
           Class
         then
            -- We draw the arrows from input event and towards output event
            -- Input Link
            Lin_Ref :=
              Mast.Graphs.Event_Handlers.Input_Link
              (Mast.Graphs.Event_Handlers.Simple_Event_Handler (Han_Ref.all)
              );
            if Lin_Ref /= null then
               begin
                  Me_Lin_Iter :=
                    Mast_Editor.Links.Lists.Find
                    (Name (Lin_Ref) & Delimiter & Name (Item),
                     Editor_System.Me_Links);
                  Me_Lin_Ref  :=
                    Mast_Editor.Links.Lists.Item
                    (Me_Lin_Iter,
                     Editor_System.Me_Links);
                  if Me_Lin_Ref /= null then
                     Add_Canvas_Link
                       (Item.Dialog.Trans_Canvas,
                        Me_Lin_Ref,
                        Me_Han_Ref);
                  end if;
               exception
                  when Invalid_Index =>
                     null;
               end;
            end if;
            -- Output Link
            Lin_Ref :=
              Mast.Graphs.Event_Handlers.Output_Link
              (Mast.Graphs.Event_Handlers.Simple_Event_Handler
                 (Han_Ref.all));
            if Lin_Ref /= null then
               begin
                  Me_Lin_Iter :=
                    Mast_Editor.Links.Lists.Find
                    (Name (Lin_Ref) & Delimiter & Name (Item),
                     Editor_System.Me_Links);
                  Me_Lin_Ref  :=
                    Mast_Editor.Links.Lists.Item
                    (Me_Lin_Iter,
                     Editor_System.Me_Links);
                  if Me_Lin_Ref /= null then
                     Add_Canvas_Link
                       (Item.Dialog.Trans_Canvas,
                        Me_Han_Ref,
                        Me_Lin_Ref);
                  end if;
               exception
                  when Invalid_Index =>
                     null;
               end;
            end if;
         elsif Han_Ref.all in Mast.Graphs.Event_Handlers.Input_Event_Handler'
           Class
         then
            -- We draw the arrows from input events and towards output event
            -- Input Links
            Mast.Graphs.Event_Handlers.Rewind_Input_Links
              (Mast.Graphs.Event_Handlers.Input_Event_Handler (Han_Ref.all),
               Link_Iter);
            for I in
              1 ..
              Mast.Graphs.Event_Handlers.Num_Of_Input_Links
              (Mast.Graphs.Event_Handlers.Input_Event_Handler
                 (Han_Ref.all))
            loop
               Mast.Graphs.Event_Handlers.Get_Next_Input_Link
                 (Mast.Graphs.Event_Handlers.Input_Event_Handler (Han_Ref.all),
                  Lin_Ref,
                  Link_Iter);
               begin
                  Me_Lin_Iter :=
                    Mast_Editor.Links.Lists.Find
                    (Name (Lin_Ref) & Delimiter & Name (Item),
                     Editor_System.Me_Links);
                  Me_Lin_Ref  :=
                    Mast_Editor.Links.Lists.Item
                    (Me_Lin_Iter,
                     Editor_System.Me_Links);
                  if Me_Lin_Ref /= null then
                     Add_Canvas_Link
                       (Item.Dialog.Trans_Canvas,
                        Me_Lin_Ref,
                        Me_Han_Ref);
                  end if;
               exception
                  when Invalid_Index =>
                     null;
               end;
            end loop;
            -- Output Link
            Lin_Ref :=
              Mast.Graphs.Event_Handlers.Output_Link
              (Mast.Graphs.Event_Handlers.Input_Event_Handler
                 (Han_Ref.all));
            if Lin_Ref /= null then
               begin
                  Me_Lin_Iter :=
                    Mast_Editor.Links.Lists.Find
                    (Name (Lin_Ref) & Delimiter & Name (Item),
                     Editor_System.Me_Links);
                  Me_Lin_Ref  :=
                    Mast_Editor.Links.Lists.Item
                    (Me_Lin_Iter,
                     Editor_System.Me_Links);
                  if Me_Lin_Ref /= null then
                     Add_Canvas_Link
                       (Item.Dialog.Trans_Canvas,
                        Me_Han_Ref,
                        Me_Lin_Ref);
                  end if;
               exception
                  when Invalid_Index =>
                     null;
               end;
            end if;
         elsif Han_Ref.all in Mast.Graphs.Event_Handlers.Output_Event_Handler'
           Class
         then
            -- We draw the arrows from input event and towards output events
            -- Input Link
            Lin_Ref :=
              Mast.Graphs.Event_Handlers.Input_Link
              (Mast.Graphs.Event_Handlers.Output_Event_Handler
                 (Han_Ref.all));
            if Lin_Ref /= null then
               begin
                  Me_Lin_Iter :=
                    Mast_Editor.Links.Lists.Find
                    (Name (Lin_Ref) & Delimiter & Name (Item),
                     Editor_System.Me_Links);
                  Me_Lin_Ref  :=
                    Mast_Editor.Links.Lists.Item
                    (Me_Lin_Iter,
                     Editor_System.Me_Links);
                  if Me_Lin_Ref /= null then
                     Add_Canvas_Link
                       (Item.Dialog.Trans_Canvas,
                        Me_Lin_Ref,
                        Me_Han_Ref);
                  end if;
               exception
                  when Invalid_Index =>
                     null;
               end;
            end if;
            -- Output Links
            Mast.Graphs.Event_Handlers.Rewind_Output_Links
              (Mast.Graphs.Event_Handlers.Output_Event_Handler (Han_Ref.all),
               Link_Iter);
            for I in
              1 ..
              Mast.Graphs.Event_Handlers.Num_Of_Output_Links
              (Mast.Graphs.Event_Handlers.Output_Event_Handler
                 (Han_Ref.all))
            loop
               Mast.Graphs.Event_Handlers.Get_Next_Output_Link
                 (Mast.Graphs.Event_Handlers.Output_Event_Handler
                    (Han_Ref.all),
                  Lin_Ref,
                  Link_Iter);
               begin
                  Me_Lin_Iter :=
                    Mast_Editor.Links.Lists.Find
                    (Name (Lin_Ref) & Delimiter & Name (Item),
                     Editor_System.Me_Links);
                  Me_Lin_Ref  :=
                    Mast_Editor.Links.Lists.Item
                    (Me_Lin_Iter,
                     Editor_System.Me_Links);
                  if Me_Lin_Ref /= null then
                     Add_Canvas_Link
                       (Item.Dialog.Trans_Canvas,
                        Me_Han_Ref,
                        Me_Lin_Ref);
                  end if;
               exception
                  when Invalid_Index =>
                     null;
               end;
            end loop;
         end if;
      end loop;
   end Draw_Event_Handlers_In_Tran_Canvas;

   ----------------------
   -- Write Transaction
   -- (Writes the params of an existing transaction and refreshes the canvas)
   ----------------------
   procedure Write_Transaction
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Transaction_Ref)
   is
   begin
      if Id_Name_Is_Valid
        (Ada.Characters.Handling.To_Lower
           (Get_Text (Item.Dialog.Trans_Name_Entry)))
      then
         Write_Parameters (Item);
         Refresh_Canvas (Transaction_Canvas);
         Hide (Item.Dialog);
      else
         Gtk_New (Editor_Error_Window, Gtk_Window(Item.Dialog));
         Set_Text (Editor_Error_Window.Label, "Identifier not Valid!!!");
         Show_All (Editor_Error_Window);
      end if;
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
         Show_All (Editor_Error_Window);
         Hide (Item.Dialog);
   end Write_Transaction;

   ---------------------
   -- New_Transaction
   -- (Adds a new transaction to the canvas and to the lists of the systems)
   ---------------------
   procedure New_Transaction
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Transaction_Ref)
   is
   begin

      -- This handler is always connected to Ok_button.  Since we
      --  don't destroy the dialog, we should provide a mechanism to
      --  avoid adding the transaction to the list again (this
      --  would raise Already_Exists exception) and only call/execute
      --  Write_Transaction handler.  The easiest (and more
      --  transparent) way to make it is changing the button label
      --  from "OK" to "OK " when we want to connect
      --  Write_Transaction only, testing label's value in both
      --  handlers

      if Get_Label (Item.Dialog.Ok_Button) = "OK" then
         if Id_Name_Is_Valid
           (Ada.Characters.Handling.To_Lower
              (Get_Text (Item.Dialog.Trans_Name_Entry)))
         then
            Write_Parameters (Item);
            Mast.Transactions.Lists.Add (Item.Tran, The_System.Transactions);
            Mast_Editor.Transactions.Lists.Add
              (Item,
               Editor_System.Me_Transactions);
            Set_Screen_Size (Item, Item.W, Item.H);
            Put (Transaction_Canvas, Item);
            Refresh_Canvas (Transaction_Canvas);
            Show_Item (Transaction_Canvas, Item);
            Hide (Item.Dialog);
         else
            Gtk_New (Editor_Error_Window, Gtk_Window(Item.Dialog));
            Set_Text (Editor_Error_Window.Label, "Identifier not Valid!!!");
            Show_All (Editor_Error_Window);
         end if;
      end if;
   exception
      when Constraint_Error =>
         Gtk_New (Editor_Error_Window);
         Set_Text (Editor_Error_Window.Label, "Invalid Value !!!");
         Show_All (Editor_Error_Window);
         Hide (Item.Dialog);
      when Already_Exists =>
         Gtk_New (Editor_Error_Window);
         Set_Text
           (Editor_Error_Window.Label,
            "The Transaction Already Exists !!!");
         Show_All (Editor_Error_Window);
         Hide (Item.Dialog);
   end New_Transaction;

   -----------------------------
   -- Show Transaction Dialog -- (Show the transaction dialog)
   -----------------------------
   procedure Show_Transaction_Dialog
     (Widget : access Gtk_Button_Record'Class)
   is
      Item     : ME_Transaction_Ref := new ME_Regular_Transaction;
      Tran_Ref : Transaction_Ref    := new Regular_Transaction;
   begin
      Item.W           := Tran_Width;
      Item.H           := Tran_Height;
      Item.Canvas_Name := To_Var_String ("Transaction_Canvas");
      Item.Color_Name  := Tran_Color;
      Item.Tran        := Tran_Ref;
      Gtk_New (Item.Dialog);
      Read_Parameters (Item);

      Hide (Item.Dialog.Trans_Diagram_Label);
      Hide (Item.Dialog.Table);
      --Resize (Item.Dialog, -1, -1);

      Button_Cb.Connect
        (Item.Dialog.Add_Ext_Button,
         "clicked",
         Button_Cb.To_Marshaller
           (Mast_Editor.Links.Show_External_Dialog'Access),
         ME_Transaction_Ref (Item));
      Button_Cb.Connect
        (Item.Dialog.Add_Int_Button,
         "clicked",
         Button_Cb.To_Marshaller
           (Mast_Editor.Links.Show_Internal_Dialog'Access),
         ME_Transaction_Ref (Item));
      Button_Cb.Connect
        (Item.Dialog.Add_Simple_Button,
         "clicked",
         Button_Cb.To_Marshaller
           (Mast_Editor.Event_Handlers.Show_Simple_Event_Handler_Dialog'
              Access),
         ME_Transaction_Ref (Item));
      Button_Cb.Connect
        (Item.Dialog.Add_Minput_Button,
         "clicked",
         Button_Cb.To_Marshaller
           (Mast_Editor.Event_Handlers.Show_Multi_Input_Event_Handler_Dialog'
              Access),
         ME_Transaction_Ref (Item));
      Button_Cb.Connect
        (Item.Dialog.Add_Moutput_Button,
         "clicked",
         Button_Cb.To_Marshaller
           (Mast_Editor.Event_Handlers.Show_Multi_Output_Event_Handler_Dialog'
              Access),
         ME_Transaction_Ref (Item));

      Button_Cb.Connect
        (Item.Dialog.Ok_Button,
         "clicked",
         Button_Cb.To_Marshaller (New_Transaction'Access),
         ME_Transaction_Ref (Item));
   end Show_Transaction_Dialog;

   ------------------------
   -- Remove_Transaction --
   ------------------------
   procedure Remove_Transaction
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Transaction_Ref)
   is
      Me_Lin_Index    : Mast_Editor.Links.Lists.Index;
      Me_Lin          : ME_Link_Ref;
      Me_Han_Index    : Mast_Editor.Event_Handlers.Lists.Index;
      Me_Han          : ME_Event_Handler_Ref;
      Tran_Index      : Mast.Transactions.Lists.Index;
      Tran_Deleted    : Mast.Transactions.Transaction_Ref;
      Me_Tran_Index   : Mast_Editor.Transactions.Lists.Index;
      Me_Tran_Deleted : Mast_Editor.Transactions.ME_Transaction_Ref;
   begin
      if Message_Dialog
        (Msg => " Do you really want to remove this object? ",
         Dialog_Type => Confirmation,
         Buttons => Button_Yes or Button_No,
         Default_Button => Button_Yes,
	 Parent => Gtk_Window(Mast_Editor_Window_Pkg.Mast_Editor_Window)) =
        Button_Yes
      then
         -- Remove event handlers of transaction from
         --Editor_System.Me_Event_Handlers list
         Mast_Editor.Event_Handlers.Lists.Rewind
           (Editor_System.Me_Event_Handlers,
            Me_Han_Index);
         for I in
           1 ..
           Mast_Editor.Event_Handlers.Lists.Size
           (Editor_System.Me_Event_Handlers)
         loop
            Me_Han :=
              Mast_Editor.Event_Handlers.Lists.Item
              (Me_Han_Index,
               Editor_System.Me_Event_Handlers);
            if Name (Me_Han.ME_Tran) = Name (Item) then
               Mast_Editor.Event_Handlers.Lists.Delete
                 (Me_Han_Index,
                  Me_Han,
                  Editor_System.Me_Event_Handlers);
            end if;
            Mast_Editor.Event_Handlers.Lists.Get_Next_Item
              (Me_Han,
               Editor_System.Me_Event_Handlers,
               Me_Han_Index);
         end loop;

         -- Remove events of transaction from Editor_System.Me_Links list
         Mast_Editor.Links.Lists.Rewind
           (Editor_System.Me_Links,
            Me_Lin_Index);
         for I in 1 .. Mast_Editor.Links.Lists.Size (Editor_System.Me_Links)
         loop
            Me_Lin :=
              Mast_Editor.Links.Lists.Item
              (Me_Lin_Index,
               Editor_System.Me_Links);
            if Name (Me_Lin.ME_Tran) = Name (Item) then
               Mast_Editor.Links.Lists.Delete
                 (Me_Lin_Index,
                  Me_Lin,
                  Editor_System.Me_Links);
            end if;
            Mast_Editor.Links.Lists.Get_Next_Item
              (Me_Lin,
               Editor_System.Me_Links,
               Me_Lin_Index);
         end loop;

         -- Remove transaction
         Tran_Index   :=
           Mast.Transactions.Lists.Find
           (Name (Item),
            The_System.Transactions);
         Tran_Deleted :=
           Mast.Transactions.Lists.Item
           (Tran_Index,
            The_System.Transactions);
         Mast.Transactions.Lists.Delete
           (Tran_Index,
            Tran_Deleted,
            The_System.Transactions);
         Me_Tran_Index   :=
           Mast_Editor.Transactions.Lists.Find
           (Name (Item),
            Editor_System.Me_Transactions);
         Me_Tran_Deleted :=
           Mast_Editor.Transactions.Lists.Item
           (Me_Tran_Index,
            Editor_System.Me_Transactions);
         Mast_Editor.Transactions.Lists.Delete
           (Me_Tran_Index,
            Me_Tran_Deleted,
            Editor_System.Me_Transactions);
         Remove (Transaction_Canvas, Item);
         Refresh_Canvas (Transaction_Canvas);

         Change_Control.Changes_Made;
         Destroy (Item_Menu);
      end if;
   exception
      when others =>
         Gtk_New (Editor_Error_Window);
         Set_Text
           (Editor_Error_Window.Label,
            "ERROR IN TRANSACTION REMOVAL !!!");
         Show_All (Editor_Error_Window);
         Destroy (Item_Menu);
   end Remove_Transaction;

   ----------------------------
   -- Properties_Transaction  --
   ----------------------------
   procedure Properties_Transaction
     (Widget : access Gtk_Widget_Record'Class;
      Item   : ME_Transaction_Ref)
   is
   begin
      Read_Parameters (Item);
      Set_Sensitive (Item.Dialog.Add_Ext_Button, True);
      Set_Sensitive (Item.Dialog.Add_Int_Button, True);
      Set_Sensitive (Item.Dialog.Add_Simple_Button, True);
      Set_Sensitive (Item.Dialog.Add_Minput_Button, True);
      Set_Sensitive (Item.Dialog.Add_Moutput_Button, True);
      Set_Label (Item.Dialog.Ok_Button, "OK "); -- The only handler executed
                                                --is Write_Transaction
      Button_Cb.Connect
        (Item.Dialog.Ok_Button,
         "clicked",
         Button_Cb.To_Marshaller (Write_Transaction'Access),
         ME_Transaction_Ref (Item));
      Refresh_Canvas (Transaction_Canvas);
      Destroy (Item_Menu);
   end Properties_Transaction;

   ---------------------
   -- On Button Click --
   ---------------------
   function On_Button_Click
     (Item  : access ME_Regular_Transaction;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      Num_Button : Guint;
      Event_Type : Gdk_Event_Type;
   begin
         Event_Type := Event.The_Type;
         if Event_Type = Gdk_2button_Press then
            Num_Button := Event.Button;
            if Num_Button = Guint (1) then
               Read_Parameters (Item);

               Set_Sensitive (Item.Dialog.Add_Ext_Button, True);
               Set_Sensitive (Item.Dialog.Add_Int_Button, True);
               Set_Sensitive (Item.Dialog.Add_Simple_Button, True);
               Set_Sensitive (Item.Dialog.Add_Minput_Button, True);
               Set_Sensitive (Item.Dialog.Add_Moutput_Button, True);

               Set_Label (Item.Dialog.Ok_Button, "OK "); -- The only handler
                                                         --executed is
                                                         --Write_Transaction
               Button_Cb.Connect
                 (Item.Dialog.Ok_Button,
                  "clicked",
                  Button_Cb.To_Marshaller (Write_Transaction'Access),
                  ME_Transaction_Ref (Item));
	       return True;
            end if;
         elsif Event_Type = Button_Press then
            Num_Button := Event.Button;
            if Num_Button = Guint (3) then
               Gtk_New (Item_Menu);
               Button_Cb.Connect
                 (Item_Menu.Remove,
                  "activate",
                  Button_Cb.To_Marshaller (Remove_Transaction'Access),
                  ME_Transaction_Ref (Item));
               Button_Cb.Connect
                 (Item_Menu.Properties,
                  "activate",
                  Button_Cb.To_Marshaller (Properties_Transaction'Access),
                  ME_Transaction_Ref (Item));
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
      Item        : in out ME_Regular_Transaction;
      Indentation : Positive;
      Finalize    : Boolean := False)
   is
   begin
      Mast_Editor.Transactions.Print
        (File,
         ME_Transaction (Item),
         Indentation);
      Put (File, " ");
      Put (File, "Me_Regular_Transaction");
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
      F               : Gtk.Frame.Gtk_Frame;
      Scrolled        : Gtk_Scrolled_Window;
      Vseparator      : Gtk_Vseparator;
      Alignment       : Gtk_Alignment;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False);
      Add (Frame, Box);
      Gtk_New(F);
      Pack_Start(Box,F);
      Gtk_New (Scrolled);
      Set_Policy(Scrolled, Gtk.Enums.Policy_Automatic, 
		 Gtk.Enums.Policy_Automatic);
      Set_Border_Width (Scrolled, 5);
      Add(F,Scrolled);
      
      Gtk_New (Transaction_Canvas);
      Configure
        (Transaction_Canvas,
         Grid_Size        => 0,
         Annotation_Font  => Pango.Font.From_String (Default_Annotation_Font),
         Arc_Link_Offset  => Default_Arc_Link_Offset,
         Arrow_Angle      => Default_Arrow_Angle,
         Arrow_Length     => Default_Arrow_Length,
         Motion_Threshold => Default_Motion_Threshold);
      Add (Scrolled, Transaction_Canvas);
      Align_On_Grid (Transaction_Canvas, False);      
      
      Gtk_New_Hbox (Bbox, Homogeneous => False);
      Pack_Start (Box, Bbox, False, False, 4);

      Gtk_New_Hbox (Cbox, True, 10);
      Pack_Start (Bbox, Cbox, False, False, 4);

      Gtk_New (Button, "Simple" & ASCII.LF & "Mode");
      Pack_Start (Cbox, Button, False, True, 0);
      Canvas_Cb.Object_Connect
        (Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Simple_Mode'Access),
         Transaction_Canvas);

      Gtk_New (Button, "Complete Mode" & ASCII.LF & "(Non-Expanded)");
      Pack_Start (Cbox, Button, False, True, 0);
      Canvas_Cb.Object_Connect
        (Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Complete_Mode_Non'Access),
         Transaction_Canvas);
      
      Gtk_New (Button, "Complete Mode" & ASCII.LF & "(Expanded)");
      Pack_Start (Cbox, Button, False, True, 0);
      Canvas_Cb.Object_Connect
        (Button,
         "clicked",
         Canvas_Cb.To_Marshaller (Complete_Mode_Exp'Access),
         Transaction_Canvas);

      Gtk_New_Vseparator (Vseparator);
      Pack_Start (Bbox, Vseparator, False, False, 10);

      Gtk_New (Button, "New " & ASCII.LF & "Transaction");
      Pack_Start (Bbox, Button, True, True, 4);
      Button_Callback.Connect
        (Button,
         "clicked",
         Button_Callback.To_Marshaller (Show_Transaction_Dialog'Access));

      Gtk_New (Alignment, 0.5, 0.5, 1.0, 1.0);
      Pack_Start (Bbox, Alignment, True, True, 4);
      Gtk_New (Alignment, 0.5, 0.5, 1.0, 1.0);
      Pack_Start (Bbox, Alignment, True, True, 4);
      Gtk_New (Alignment, 0.5, 0.5, 1.0, 1.0);
      Pack_Start (Bbox, Alignment, True, True, 4);
      
      Realize (Transaction_Canvas);

      Editor_Actions.Load_System_Font (Font, Font1);
      Font_Size:=Get_Size(Font);
      P_Layout := Frame.Create_Pango_Layout;
      Set_Font_Description (P_Layout, Font);
      Line_Height:=Gdouble(P_Layout.Get_Baseline/Pango_Scale);
      
      Show_All (Frame);
   end Run;
end Mast_Editor.Transactions;
