-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2001-2024                     --
--                 Universidad de Cantabria, SPAIN                   --
--                                                                   --
-- Authors: Michael Gonzalez       mgh@unican.es                     --
--          Jose Javier Gutierrez  gutierjj@unican.es                --
--          Jose Carlos Palencia   palencij@unican.es                --
--          Jose Maria Drake       drakej@unican.es                  --
--          Yago Pereiro                                             --
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

with Mast.Transactions, Mast.IO, Mast.XMI, Ada.Strings,
  Ada.Strings.Fixed,Ada.Float_Text_IO, Var_Strings;
use Ada.Strings,Ada.Strings.Fixed,Ada.Float_Text_IO;

with Ada.Text_IO;
use Mast.Transactions;
use Ada.Text_IO;
use Var_Strings;

package body Mast.Timing_Requirements is

   use type Mast.Events.Event_Ref;
   use type Events.Lists.Index;

   ---------------------
   -- Add_Requirement --
   ---------------------

   procedure Add_Requirement
     (Comp_Req : in out Composite_Timing_Req;
      Req : Simple_Timing_Requirement_Ref)
   is
   begin
      Req_Lists.Add(Req,Comp_Req.Req_List);
   end Add_Requirement;

   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (Req : in out Timing_Requirement;
      Events_List : Events.Lists.List)
   is
   begin
      null; -- the default adjustment is to do nothing
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust
     (Req : in out Global_Deadline;
      Events_List : Events.Lists.List)
   is
      The_Index : Events.Lists.Index;
   begin
      if Req.Event/=null then
         The_Index:=Events.Lists.Find
           (Events.Name(Req.Event),Events_List);
         if The_Index=Events.Lists.Null_Index then
            Set_Exception_Message
              ("Global_Deadline reference to Event "&
               Var_Strings.To_String(Events.Name(Req.Event))&" not found");
            raise Object_Not_Found;
         else
            Req.Event:=Events.Lists.Item(The_Index,Events_List);
         end if;
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust
     (Req : in out Max_Output_Jitter_Req;
      Events_List : Events.Lists.List)
   is
      The_Index : Events.Lists.Index;
   begin
      if Req.Event/=null then
         The_Index:=Events.Lists.Find
           (Events.Name(Req.Event),Events_List);
         if The_Index=Events.Lists.Null_Index then
            Set_Exception_Message
              ("Max_Output_Jitter_Req reference to Event "&
               Var_Strings.To_String(Events.Name(Req.Event))&" not found");
            raise Object_Not_Found;
         else
            Req.Event:=Events.Lists.Item(The_Index,Events_List);
         end if;
      end if;
   end Adjust;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust
     (Req : in out Composite_Timing_Req;
      Events_List : Events.Lists.List)
   is
      Iterator : Req_Lists.Iteration_Object;
      Req_Ref : Simple_Timing_Requirement_Ref;
   begin
      Req_Lists.Rewind(Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Req.Req_List) loop
         Req_Lists.Get_Next_Item(Req_Ref,Req.Req_List,Iterator);
         Adjust(Req_Ref.all,Events_List);
      end loop;
   end Adjust;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Hard_Global_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Timing_Result'Class then
         Is_Met:= Mast.Results.Worst_Global_Response_Time(Res,Event(Req))
           <= The_Deadline(Req);
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Hard_Local_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Timing_Result'Class then
         Is_Met:= Mast.Results.Worst_Local_Response_Time(Res) <=
           The_Deadline(Req);
      else
         raise Incorrect_Object;
      end if;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Soft_Global_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         Is_Met:= Mast.Results.Avg_Global_Response_Time
           (Results.Simulation_Timing_Result(Res),Event(Req))
           <= The_Deadline(Req);
      elsif Res in Results.Timing_Result'Class then
         Is_Met:=True;
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Soft_Local_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         Is_Met:= Mast.Results.Avg_Local_Response_Time
           (Results.Simulation_Timing_Result(Res)) <=
           The_Deadline(Req);
      elsif Res in Results.Timing_Result'Class then
         Is_Met:=True;
      else
         raise Incorrect_Object;
      end if;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Global_Max_Miss_Ratio;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         Is_Met:= Mast.Results.Global_Miss_Ratio
           (Results.Simulation_Timing_Result(Res),
            The_Deadline(Req),Event(Req)) <=
           Ratio(Req);
      elsif Res in Results.Timing_Result'Class then
         Is_Met:=True;
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Deadline |
        Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Local_Max_Miss_Ratio;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         Is_Met:= Mast.Results.Local_Miss_Ratio
           (Results.Simulation_Timing_Result(Res),The_Deadline(Req))
           <= Ratio(Req);
      elsif Res in Results.Timing_Result'Class then
         Is_Met:=True;
      else
         raise Incorrect_Object;
      end if;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Max_Output_Jitter_Req;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
   begin
      if Res in Results.Timing_Result'Class then
         Is_Met:= Mast.Results.Jitter(Res,Event(Req)) <=
           Max_Output_Jitter(Req);
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Check;

   -----------
   -- Check --
   -----------

   overriding procedure Check
     (Req : in out Composite_Timing_Req;
      Res : Mast.Results.Timing_Result'Class;
      Is_Met : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Req.Req_List,Iterator);
         Check(Simple_Req.all,Res,Is_Met);
         exit when not Is_Met;
      end loop;
   end Check;


   -----------
   -- Clone --
   -----------

   function Clone
     (The_List : Req_Lists.List)
     return Req_Lists.List
   is
      The_Copy : Req_Lists.List;
      Iterator : Req_Lists.Iteration_Object;
      Req_Ref, Req_Ref_Copy : Simple_Timing_Requirement_Ref;
   begin
      Req_Lists.Rewind(The_List,Iterator);
      for I in 1..Req_Lists.Size(The_List) loop
         Req_Lists.Get_Next_Item(Req_Ref,The_List,Iterator);
         Req_Ref_Copy:=Simple_Timing_Requirement_Ref(Clone(Req_Ref.all));
         Req_Lists.Add(Req_Ref_Copy,The_Copy);
      end loop;
      return The_Copy;
   end Clone;


   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Req : Hard_Global_Deadline)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Hard_Global_Deadline'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Hard_Local_Deadline)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Hard_Local_Deadline'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Soft_Global_Deadline)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Soft_Global_Deadline'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Soft_Local_Deadline)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Soft_Local_Deadline'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Global_Max_Miss_Ratio)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Global_Max_Miss_Ratio'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Local_Max_Miss_Ratio)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Local_Max_Miss_Ratio'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Max_Output_Jitter_Req)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Max_Output_Jitter_Req'(Req);
      return Req_Ref;
   end Clone;

   -----------
   -- Clone --
   -----------
   overriding function Clone
     (Req : Composite_Timing_Req)
     return Timing_Requirement_Ref
   is
      Req_Ref : Timing_Requirement_Ref;
   begin
      Req_Ref:=new Composite_Timing_Req;
      Composite_Timing_Req(Req_Ref.all).Req_List:=Clone(Req.Req_List);
      return Req_Ref;
   end Clone;


   --------------
   -- Deadline --
   --------------

   function The_Deadline
     (Req : Deadline)
     return Time
   is
   begin
      return Req.The_Deadline;
   end The_Deadline;

   -----------
   -- Event --
   -----------

   function Event
     (Req : Global_Deadline)
     return Mast.Events.Event_Ref
   is
   begin
      return Req.Event;
   end Event;

   -----------
   -- Event --
   -----------

   function Event
     (Req : Max_Output_Jitter_Req)
     return Mast.Events.Event_Ref
   is
   begin
      return Req.Event;
   end Event;

   ------------------------------
   -- Find_Hard_Global_Deadline
   ------------------------------

   procedure Find_Hard_Global_Deadline
     (Comp_Req : Composite_Timing_Req;
      The_Dline : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Hard_Global_Deadline'Class
         then
            The_Dline:=The_Deadline(Deadline(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Hard_Global_Deadline;

   ------------------------------
   -- Find_Hard_Global_Deadline
   ------------------------------

   procedure Find_Hard_Global_Deadline
     (Comp_Req : Composite_Timing_Req;
      The_Event : Mast.Events.Event_Ref;
      The_Dline : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Hard_Global_Deadline'Class and then
           The_Event=Event(Global_Deadline(Simple_Req.all))
         then
            The_Dline:=The_Deadline(Deadline(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Hard_Global_Deadline;

   --------------------------
   -- Find_Hard_Local_Deadline
   --------------------------

   procedure Find_Hard_Local_Deadline
     (Comp_Req : Composite_Timing_Req;
      The_Dline : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Hard_Local_Deadline'Class
         then
            The_Dline:=The_Deadline(Deadline(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Hard_Local_Deadline;

   --------------------------
   -- Find_Soft_Global_Deadline
   --------------------------

   procedure Find_Soft_Global_Deadline
     (Comp_Req : Composite_Timing_Req;
      The_Event : Mast.Events.Event_Ref;
      The_Dline : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Soft_Global_Deadline'Class and then
           The_Event=Event(Global_Deadline(Simple_Req.all))
         then
            The_Dline:=The_Deadline(Deadline(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Soft_Global_Deadline;

   ---------------------------
   -- Find_Soft_Local_Deadline
   ---------------------------

   procedure Find_Soft_Local_Deadline
     (Comp_Req : Composite_Timing_Req;
      The_Dline : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Soft_Local_Deadline'Class
         then
            The_Dline:=The_Deadline(Deadline(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Soft_Local_Deadline;

   -----------------------------
   -- Find_Max_Output_Jitter_Req
   -----------------------------

   procedure Find_Max_Output_Jitter_Req
     (Comp_Req : Composite_Timing_Req;
      Max_Jitter : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Max_Output_Jitter_Req'Class
         then
            Max_Jitter:=Max_Output_Jitter
              (Max_Output_Jitter_Req(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Max_Output_Jitter_Req;

   -----------------------------
   -- Find_Max_Output_Jitter_Req
   -----------------------------

   procedure Find_Max_Output_Jitter_Req
     (Comp_Req : Composite_Timing_Req;
      The_Event : Mast.Events.Event_Ref;
      Max_Jitter : out Time;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Max_Output_Jitter_Req'Class and then
           The_Event=Event(Max_Output_Jitter_Req(Simple_Req.all))
         then
            Max_Jitter:=Max_Output_Jitter
              (Max_Output_Jitter_Req(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Max_Output_Jitter_Req;

   -----------------------------
   -- Find_Global_Max_Miss_Ratio
   -----------------------------

   procedure Find_Global_Max_Miss_Ratio
     (Comp_Req : Composite_Timing_Req;
      The_Event : Mast.Events.Event_Ref;
      The_Dline : Time;
      The_Ratio : out Percentage;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Global_Max_Miss_Ratio'Class and then
           The_Event=Event(Global_Deadline(Simple_Req.all)) and then
           The_Dline=The_Deadline(Deadline(Simple_Req.all))
         then
            The_Ratio:=Ratio(Global_Max_Miss_Ratio(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Global_Max_Miss_Ratio;

   ----------------------------
   -- Find_Local_Max_Miss_Ratio
   ----------------------------

   procedure Find_Local_Max_Miss_Ratio
     (Comp_Req : Composite_Timing_Req;
      The_Dline : Time;
      The_Ratio : out Percentage;
      Is_Present : out Boolean)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Comp_Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Comp_Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Comp_Req.Req_List,Iterator);
         if Simple_Req.all in Local_Max_Miss_Ratio'Class and then
           The_Dline=The_Deadline(Deadline(Simple_Req.all))
         then
            The_Ratio:=Ratio(Local_Max_Miss_Ratio(Simple_Req.all));
            Is_Present:=True;
            return;
         end if;
      end loop;
      Is_Present:=False;
   end Find_Local_Max_Miss_Ratio;

   --------------------------
   -- Get_Next_Requirement --
   --------------------------

   procedure Get_Next_Requirement
     (Comp_Req : Composite_Timing_Req;
      Req : out Simple_Timing_Requirement_Ref;
      Iterator : in out Iteration_Object)
   is
   begin
      Req_Lists.Get_Next_Item
        (Req,Comp_Req.Req_List,Req_Lists.Index(Iterator));
   end Get_Next_Requirement;


   --------------------------
   -- Has_Hard_Requirement --
   --------------------------
   function Has_Hard_Requirement
     (Tim_Req : Timing_Requirement'class)
     return Boolean
   is
      Is_Present : Boolean;
      Deadline : Time;
   begin
      if Tim_Req in
        Hard_Global_Deadline'Class
        or else Tim_Req in
        Hard_Local_Deadline'Class
        or else Tim_Req in
        Max_Output_Jitter_Req'Class
      then
         return True;
      elsif Tim_Req in Composite_Timing_Req'class
      then
         Find_Hard_Global_Deadline
           (Composite_Timing_Req(Tim_Req),Deadline,Is_Present);
         if Is_Present then
            return True;
         else
            Find_Hard_Local_Deadline
              (Composite_Timing_Req(Tim_Req),Deadline,Is_Present);
            if Is_Present then
               return True;
            else
               Find_Max_Output_Jitter_Req
                 (Composite_Timing_Req(Tim_Req),Deadline,Is_Present);
               if Is_Present then
                  return True;
               end if;
            end if;
         end if;
      end if;
      -- No hard requirement found
      return False;
   end Has_Hard_Requirement;


   --------------------------------
   -- Has_Hard_Local_Requirement --
   --------------------------------
   function Has_Hard_Local_Requirement
     (Tim_Req : Timing_Requirement'class)
     return Boolean
   is
      Is_Present : Boolean;
      Deadline : Time;
   begin
      if Tim_Req in Hard_Local_Deadline'Class
      then
         return True;
      elsif Tim_Req in Composite_Timing_Req'class
      then
         Find_Hard_Local_Deadline
           (Composite_Timing_Req(Tim_Req),Deadline,Is_Present);
         if Is_Present then
               return True;
         end if;
      end if;
      -- No hard local requirement found
      return False;
   end Has_Hard_Local_Requirement;


   -----------------------
   -- Max_Output_Jitter --
   -----------------------

   function Max_Output_Jitter
     (Req : Max_Output_Jitter_Req)
     return Time
   is
   begin
      return Req.Max_Output_Jitter;
   end Max_Output_Jitter;

   -------------------------
   -- Num_Of_Requirements --
   -------------------------

   function Num_Of_Requirements
     (Comp_Req : Composite_Timing_Req)
     return Natural
   is
   begin
      return Req_Lists.Size (Comp_Req.Req_List);
   end Num_Of_Requirements;

   -----------
   -- Print --
   -----------

   procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Timing_Requirement;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"(");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Simple_Timing_Requirement;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
   begin
      null;
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Hard_Global_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Hard_Global_Deadline",Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Deadline",
         IO.Time_Image(Res.The_Deadline),Indentation+2,Names_Length);
      if Res.Event/=null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Referenced_Event",
            IO.Name_Image(Mast.Events.Name(Res.Event)),
            Indentation+2,Names_Length);
      end if;
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Soft_Global_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Soft_Global_Deadline",Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Deadline",
         IO.Time_Image(Res.The_Deadline),Indentation+2,Names_Length);
      if Res.Event/=null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Referenced_Event",
            IO.Name_Image(Mast.Events.Name(Res.Event)),
            Indentation+2,Names_Length);
      end if;
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Hard_Local_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Hard_Local_Deadline",Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Deadline",
         IO.Time_Image(Res.The_Deadline),Indentation+2,Names_Length);
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Soft_Local_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Soft_Local_Deadline",Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Deadline",
         IO.Time_Image(Res.The_Deadline),Indentation+2,Names_Length);
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Global_Max_Miss_Ratio;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      Ratio_Image : String(1..20);
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Global_Max_Miss_Ratio",Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Deadline",
         IO.Time_Image(Res.The_Deadline),Indentation+2,Names_Length);
      if Res.Event/=null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Referenced_Event",
            IO.Name_Image(Mast.Events.Name(Res.Event)),
            Indentation+2,Names_Length);
      end if;
      Mast.IO.Print_Separator(File);
      Put(Ratio_Image,Res.Ratio,2,0);
      Mast.IO.Print_Arg
        (File,"Ratio",Trim(Ratio_Image,Both),Indentation+2,Names_Length);
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Local_Max_Miss_Ratio;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      Ratio_Image : String(1..20);
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Local_Max_Miss_Ratio",Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Deadline",
         IO.Time_Image(Res.The_Deadline),Indentation+2,Names_Length);
      Mast.IO.Print_Separator(File);
      Put(Ratio_Image,Res.Ratio,2,0);
      Mast.IO.Print_Arg
        (File,"Ratio",Trim(Ratio_Image,Both),
         Indentation+2,Names_Length);
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Max_Output_Jitter_Req;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 17;
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Max_Output_Jitter_Req",Indentation+2,Names_Length);
      if Res.Event/=null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Referenced_Event",
            IO.Name_Image(Mast.Events.Name(Res.Event)),
            Indentation+2,Names_Length);
      end if;
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Max_Output_Jitter",
         IO.Time_Image(Res.Max_Output_Jitter),
         Indentation+2,Names_Length);
      Ada.Text_IO.Put(File,")");
   end Print;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Res : in out Composite_Timing_Req;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 17;
      Res_Ref : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Print(File,Timing_Requirement(Res),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Composite",Indentation+2,Names_Length);
      if Req_Lists.Size(Res.Req_List) > 0 then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Requirements_List","",Indentation+2,Names_Length);
         Mast.IO.Print_Separator(File,Mast.IO.Nothing);
         Mast.IO.Print_List_Item(File,Mast.IO.Left_Paren,
                                 Indentation+5);
         Req_Lists.Rewind(Res.Req_List,Iterator);
         for I in 1..Req_Lists.Size(Res.Req_List) loop
            Req_Lists.Get_Next_Item(Res_Ref,Res.Req_List,Iterator);
            Print(File,Res_Ref.all,Indentation+7);
            if I = Req_Lists.Size(Res.Req_List) then
               Ada.Text_IO.Put(File,")");
            else
               Mast.IO.Print_Separator(File);
            end if;
         end loop;
      end if;
      Ada.Text_IO.Put(File,")");
   end Print;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Hard_Global_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Hard_Global_Deadline ");
      Ada.Text_IO.Put
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      if Res.Event/=null then
         Ada.Text_IO.Put
           (File," Referenced_Event=""" &
            IO.Name_Image(Mast.Events.Name(Res.Event)) & """");
      end if;
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------
   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Soft_Global_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Soft_Global_Deadline ");
      Ada.Text_IO.Put
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      if Res.Event/=null then
         Ada.Text_IO.Put
           (File," Referenced_Event=""" &
            IO.Name_Image(Mast.Events.Name(Res.Event)) & """");
      end if;
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Hard_Local_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Hard_Local_Deadline ");
      Ada.Text_IO.Put
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Soft_Local_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Soft_Local_Deadline ");
      Ada.Text_IO.Put
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;


   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Global_Max_Miss_Ratio;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      pragma Unreferenced (Names_Length);
      Ratio_Image : String(1..20);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Global_Max_Miss_Ratio ");
      Ada.Text_IO.Put
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      if Res.Event/=null then
         Ada.Text_IO.Put
           (File," Referenced_Event=""" &
            IO.Name_Image(Mast.Events.Name(Res.Event)) & """");
      end if;
      Put(Ratio_Image,Res.Ratio,2,0);
      Ada.Text_IO.Put(File," Ratio=""" & Trim(Ratio_Image,Both) & """");
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Local_Max_Miss_Ratio;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      pragma Unreferenced (Names_Length);
      Ratio_Image : String(1..20);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Local_Max_Miss_Ratio ");
      Ada.Text_IO.Put
        (File,"Deadline=""" &
         IO.Time_Image(Res.The_Deadline) & """");
      Put(Ratio_Image,Res.Ratio,2,0);
      Ada.Text_IO.Put(File," Ratio=""" & Trim(Ratio_Image,Both) & """");
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Max_Output_Jitter_Req;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 17;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Max_Output_Jitter_Req ");
      if Res.Event/=null then
         Ada.Text_IO.Put
           (File," Referenced_Event=""" &
            IO.Name_Image(Mast.Events.Name(Res.Event)) & """");
      end if;
      Ada.Text_IO.Put
        (File," Max_Output_Jitter=""" &
         IO.Time_Image(Res.Max_Output_Jitter) & """");
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Res : in out Composite_Timing_Req;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 17;
      pragma Unreferenced (Names_Length);
      Res_Ref : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put_Line(File,"<mast_mdl:Composite_Timing_Requirement>");
      Req_Lists.Rewind(Res.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Res.Req_List) loop
         Req_Lists.Get_Next_Item(Res_Ref,Res.Req_List,Iterator);
         Print_XML(File,Res_Ref.all,Indentation+3,False);
      end loop;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put_Line(File,"</mast_mdl:Composite_Timing_Requirement>");
   end Print_XML;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Hard_Global_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File,"xsi:type=""mast2:Hard_Global_Deadline""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      if Res.Event/=null then
         Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
         Ada.Text_IO.Put_Line
           (File, "Referenced_Event=""//@Element_List." &
         Mast.IO.Integer_Image(Mast.XMI.Get_Size_Element_List) &
         "/@Flow_Element_List." & 
         Mast.IO.Integer_Image
         (Find_Position(Mast.Events.Name(Res.Event))) & """");
      end if;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------
   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Soft_Global_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Soft_Global_Deadline""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      if Res.Event/=null then
         Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
         Ada.Text_IO.Put_Line
           (File, " Referenced_Event=""//@Element_List." &
         Mast.IO.Integer_Image(Mast.XMI.Get_Size_Element_List) &
         "/@Flow_Element_List." & 
         Mast.IO.Integer_Image
         (Find_Position(Mast.Events.Name(Res.Event))) & """");
      end if;
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Hard_Local_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Hard_Local_Deadline""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Soft_Local_Deadline;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Soft_Local_Deadline""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Global_Max_Miss_Ratio;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 16;
      pragma Unreferenced (Names_Length);
      Ratio_Image : String(1..20);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Global_Max_Miss_Ratio""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Put(Ratio_Image,Res.Ratio,2,0);
      Ada.Text_IO.Put_Line
        (File,"Ratio=""" & Trim(Ratio_Image,Both) & """");
      if Res.Event/=null then
         Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
         Ada.Text_IO.Put
           (File, " Referenced_Event=""//@Element_List." &
         Mast.IO.Integer_Image(Mast.XMI.Get_Size_Element_List) &
         "/@Flow_Element_List." & 
         Mast.IO.Integer_Image
         (Find_Position(Mast.Events.Name(Res.Event))) & """");
      end if;
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Local_Max_Miss_Ratio;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 8;
      pragma Unreferenced (Names_Length);
      Ratio_Image : String(1..20);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Local_Max_Miss_Ratio""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Deadline=""" & IO.Time_Image(Res.The_Deadline) & """");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Put(Ratio_Image,Res.Ratio,2,0);
      Ada.Text_IO.Put_Line
        (File,"Ratio=""" & Trim(Ratio_Image,Both) & """/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Max_Output_Jitter_Req;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 17;
      pragma Unreferenced (Names_Length);
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Max_Output_Jitter_Req""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line
        (File,"Max_Output_Jitter=""" & IO.Time_Image(Res.Max_Output_Jitter)
      & """");
      if Res.Event/=null then
         Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
         Ada.Text_IO.Put
           (File, " Referenced_Event=""//@Element_List." &
         Mast.IO.Integer_Image(Mast.XMI.Get_Size_Element_List) &
         "/@Flow_Element_List." & 
         Mast.IO.Integer_Image
         (Find_Position(Mast.Events.Name(Res.Event))) & """");
      end if;
      Ada.Text_IO.Put_Line(File,"/>");
   end Print_XMI;

   ---------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Res : in out Composite_Timing_Req;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Names_Length : constant Positive := 17;
      pragma Unreferenced (Names_Length);
      Res_Ref : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Observer_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File," xsi:type=""mast2:Composite_Observer"">");

      Req_Lists.Rewind(Res.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Res.Req_List) loop
         Req_Lists.Get_Next_Item(Res_Ref,Res.Req_List,Iterator);
         Print_XMI(File,Res_Ref.all,Indentation+2,False);
      end loop;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put_Line(File,"</Observer_List>");
   end Print_XMI;
   
   -----------
   -- Ratio --
   -----------

   function Ratio
     (Req : Global_Max_Miss_Ratio)
     return Mast.Percentage
   is
   begin
      return Req.Ratio;
   end Ratio;

   -----------
   -- Ratio --
   -----------

   function Ratio
     (Req : Local_Max_Miss_Ratio)
     return Mast.Percentage
   is
   begin
      return Req.Ratio;
   end Ratio;

   -------------------------
   -- References_Event    --
   -------------------------

   overriding function References_Event
     (Evnt : Mast.Events.Event_Ref;
      Req  : Deadline)
     return Boolean
   is
   begin
      return False; -- no referenced events
   end References_Event;

   -------------------------
   -- References_Event    --
   -------------------------

   overriding function References_Event
     (Evnt : Mast.Events.Event_Ref;
      Req  : Global_Deadline)
     return Boolean
   is
   begin
      return Req.Event=Evnt;
   end References_Event;

   -------------------------
   -- References_Event    --
   -------------------------

   overriding function References_Event
      (Evnt : Mast.Events.Event_Ref;
       Req  : Max_Output_Jitter_Req)
     return Boolean
   is
   begin
      return Req.Event=Evnt;
   end References_Event;

   -------------------------
   -- References_Event    --
   -------------------------

   overriding function References_Event
     (Evnt : Mast.Events.Event_Ref;
      Req  : Composite_Timing_Req)
     return Boolean
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
   begin
      Req_Lists.Rewind(Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Req.Req_List,Iterator);
         if References_Event(Evnt,Simple_Req.all) then
            return True;
         end if;
      end loop;
      return False;
   end References_Event;

   -------------------------
   -- Rewind_Requirements --
   -------------------------

   procedure Rewind_Requirements
     (Comp_Req : Composite_Timing_Req;
      Iterator : out Iteration_Object)
   is
   begin
      Req_Lists.Rewind (Comp_Req.Req_List,Req_Lists.Index(Iterator));
   end Rewind_Requirements;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Hard_Global_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Timing_Result'Class then
         The_Index := Float
           ((The_Deadline(Req)-
             Mast.Results.Worst_Global_Response_Time(Res,Event(Req)))/
            The_Deadline(Req));
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Schedulability_Index;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Hard_Local_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Timing_Result'Class then
         The_Index := Float
           ((The_Deadline(Req)-
             Mast.Results.Worst_Local_Response_Time(Res))/
            The_Deadline(Req));
      else
         raise Incorrect_Object;
      end if;
   end Schedulability_Index;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Soft_Global_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         The_Index := Float
           ((The_Deadline(Req)-
             Mast.Results.Avg_Global_Response_Time
             (Results.Simulation_Timing_Result(Res),Event(Req)))/
            The_Deadline(Req));
      elsif Res in Results.Timing_Result'Class then
         The_Index:=0.0;
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Schedulability_Index;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Soft_Local_Deadline;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         The_Index := Float
           ((The_Deadline(Req)-
             Mast.Results.Avg_Local_Response_Time
             (Results.Simulation_Timing_Result(Res)))/
            The_Deadline(Req));
      elsif Res in Results.Timing_Result'Class then
         The_Index:=0.0;
      else
         raise Incorrect_Object;
      end if;
   end Schedulability_Index;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Global_Max_Miss_Ratio;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         The_Index := (Ratio(Req)-
             Mast.Results.Global_Miss_Ratio
             (Results.Simulation_Timing_Result(Res),
              The_Deadline(Req),Event(Req)))/
            Ratio(Req);
      elsif Res in Results.Timing_Result'Class then
         The_Index:=0.0;
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Deadline |
        Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Schedulability_Index;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Local_Max_Miss_Ratio;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Simulation_Timing_Result'Class then
         The_Index := (Ratio(Req)-
             Mast.Results.Local_Miss_Ratio
             (Results.Simulation_Timing_Result(Res),The_Deadline(Req)))/
            Ratio(Req);
      elsif Res in Results.Timing_Result'Class then
         The_Index:=0.0;
      else
         raise Incorrect_Object;
      end if;
   end Schedulability_Index;

   --------------------------
   -- Schedulability_Index --
   --------------------------

   overriding procedure Schedulability_Index
     (Req : in out Max_Output_Jitter_Req;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
   begin
      if Res in Results.Timing_Result'Class then
         The_Index := Float
           ((Max_Output_Jitter(Req)-
             Mast.Results.Jitter(Res,Event(Req)))/
            Max_Output_Jitter(Req));
      else
         raise Incorrect_Object;
      end if;
   exception
      when Mast.Results.No_Results_For_Event =>
         raise Inconclusive;
   end Schedulability_Index;

   ----------------------
   -- Schedulability_Index --
   ----------------------

   overriding procedure Schedulability_Index
     (Req : in out Composite_Timing_Req;
      Res : Mast.Results.Timing_Result'Class;
      The_Index : out Float)
   is
      Simple_Req : Simple_Timing_Requirement_Ref;
      Iterator : Req_Lists.Index;
      Acum,Temp : Float := 0.0;
   begin
      Req_Lists.Rewind(Req.Req_List,Iterator);
      for I in 1..Req_Lists.Size(Req.Req_List) loop
         Req_Lists.Get_Next_Item(Simple_Req,Req.Req_List,Iterator);
         Schedulability_Index(Simple_Req.all,Res,Temp);
         Acum := Acum+Temp;
      end loop;
      The_Index := Acum/Float(Req_Lists.Size(Req.Req_List));
   end Schedulability_Index;

   ----------------------
   -- Set_The_Deadline --
   ----------------------

   procedure Set_The_Deadline
     (Req : in out Deadline;
      The_Deadline : Time)
   is
   begin
      Req.The_Deadline:=The_Deadline;
   end Set_The_Deadline;

   ---------------
   -- Set_Event --
   ---------------

   procedure Set_Event
     (Req : in out Global_Deadline;
      The_Event : Mast.Events.Event_Ref)
   is
   begin
      Req.Event:=The_Event;
   end Set_Event;

   ---------------
   -- Set_Event --
   ---------------

   procedure Set_Event
     (Req : in out Max_Output_Jitter_Req;
      The_Event : Mast.Events.Event_Ref)
   is
   begin
      Req.Event:=The_Event;
   end Set_Event;

   ---------------------------
   -- Set_Max_Output_Jitter --
   ---------------------------

   procedure Set_Max_Output_Jitter
     (Req : in out Max_Output_Jitter_Req;
      The_Max_Output_Jitter : Time)
   is
   begin
      Req.Max_Output_Jitter:=The_Max_Output_Jitter;
   end Set_Max_Output_Jitter;

   ---------------
   -- Set_Ratio --
   ---------------

   procedure Set_Ratio
     (Req : in out Global_Max_Miss_Ratio;
      The_Ratio : Percentage)
   is
   begin
      Req.Ratio:=The_Ratio;
   end Set_Ratio;

   ---------------
   -- Set_Ratio --
   ---------------

   procedure Set_Ratio
     (Req : in out Local_Max_Miss_Ratio;
      The_Ratio : Percentage)
   is
   begin
      Req.Ratio:=The_Ratio;
   end Set_Ratio;

end Mast.Timing_Requirements;
