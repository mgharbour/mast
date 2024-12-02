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
--          Julio Luis Medina      medinajl@unican.es                --
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

with Mast.Processing_Resources.Processor, Mast.IO, Mast.XMI;

package body Mast.Schedulers.Secondary is

   use type Mast.Scheduling_Policies.Scheduling_Policy_Ref;
   use type Mast.Scheduling_Servers.Scheduling_Server_Ref;
   use type Mast.Scheduling_Servers.Lists.Index;
   
   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (Sch : in out Secondary_Scheduler;
      Sched_Server_List : Scheduling_Servers.Lists.List)
   is
      The_Index : Scheduling_Servers.Lists.Index;
   begin
      if Sch.Server/=null then
         The_Index:=Scheduling_Servers.Lists.Find
           (Scheduling_Servers.Name(Sch.Server),Sched_Server_List);
         if The_Index=Scheduling_Servers.Lists.Null_Index then
            Set_Exception_Message
              ("Error in Secondary_Scheduler "&Var_Strings.To_String(Sch.Name)&
               ": Scheduling_Server "&
               Var_Strings.To_String(Scheduling_Servers.Name(Sch.Server))&
               " not found");
            raise Object_Not_Found;
         else
            Sch.Server:=Scheduling_Servers.Lists.Item
              (The_Index,Sched_Server_List);
         end if;
      end if;
   end Adjust;

   ------------
   -- Clone  --
   ------------

   overriding function Clone
     (Sch : Secondary_Scheduler)
     return  Scheduler_Ref
   is
      Sch_Ref : Scheduler_Ref;
   begin
      Sch_Ref:=new Secondary_Scheduler'(Sch);
      return Sch_Ref;
   end Clone;

   ------------------
   -- Max_Priority --
   ------------------

   overriding function Max_Priority
     (Sch : Secondary_Scheduler) return Priority
   is
   begin
      return Max_Priority
        (Scheduling_Servers.Server_Scheduler(Sch.Server.all).all);
   end Max_Priority;

   ------------------
   -- Min_Priority --
   ------------------

   overriding function Min_Priority
     (Sch : Secondary_Scheduler) return Priority
   is
   begin
      return Min_Priority
        (Scheduling_Servers.Server_Scheduler(Sch.Server.all).all);
   end Min_Priority;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Sch : in out Secondary_Scheduler;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Name_Length : constant := 15;
   begin
      Print(File, Scheduler(Sch),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Secondary_Scheduler",Indentation+3,Name_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Name",
         IO.Name_Image(Sch.Name),Indentation+3,Name_Length);
      if Sch.Server/=null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Server",
            IO.Name_Image(Mast.Scheduling_Servers.Name(Sch.Server)),
            Indentation+3,Name_Length);
      end if;
      if Sch.Policy /= null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Policy","",
            Indentation+3,Name_Length);
         Mast.Scheduling_Policies.Print(File,Sch.Policy.all,Indentation+6);
      end if;
      Mast.IO.Print_Separator(File,",",Finalize);
   end Print;

   ----------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Sch : in out Secondary_Scheduler;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      -- Name_Length : constant := 15;
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Secondary_Scheduler ");
      Ada.Text_IO.Put(File,"Name=""" & IO.Name_Image(Sch.Name) & """ ");
      if Sch.Server/=null then
         Ada.Text_IO.Put
           (File,"Host=""" &
            IO.Name_Image(Mast.Scheduling_Servers.Name(Sch.Server)) & """ ");
      end if;
      Ada.Text_IO.Put_Line(File," >");
      if Sch.Policy /= null then
         Mast.Scheduling_Policies.Print_XML
           (File,Sch.Policy.all,Indentation+3,False);
      end if;
      Ada.Text_IO.Put_Line(File,"</mast_mdl:Secondary_Scheduler> ");
   end Print_XML;

   ----------------
   -- Print_XMI --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Sch : in out Secondary_Scheduler;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      -- Name_Length : constant := 15;
   begin
      Mast.XMI.Add_Element_List_Item;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Element_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line(File,"xsi:type=""mast2:Secondary_Scheduler""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File,"name=""" & XMI.XMI_Name_Image(Sch.Name, "sch_") &
         """");
      if Sch.Server/=null then
         Ada.Text_IO.New_Line(File);
         Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
         if Sch.Server.Server_Processing_Resource.all in 
      Mast.Processing_Resources.Processor.Processor'Class 
    then 
            Ada.Text_IO.Put
         (File,"Host=""" &
       XMI.XMI_Name_Image(Mast.Scheduling_Servers.Name(Sch.Server),
                "th_") & """");
         else
            Ada.Text_IO.Put
         (File,"Host=""" &
       XMI.XMI_Name_Image(Mast.Scheduling_Servers.Name(Sch.Server),
                "com_") & """");     
         end if;
      end if;
      Ada.Text_IO.Put_Line(File,">");
      if Sch.Policy /= null then
         Mast.Scheduling_Policies.Print_XMI
           (File,Sch.Policy.all,Indentation+2,False);
      end if;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put_Line(File,"</Element_List>");   
   end Print_XMI;

   ----------------
   -- Set_Server --
   ----------------

   procedure Set_Server
     (Sch : in out Secondary_Scheduler;
      The_Server: Mast.Scheduling_Servers.Scheduling_Server_Ref)
   is
   begin
      if Sch.Server/=null then
         Scheduling_Servers.Set_Scheduler_State(Sch.Server.all,False);
      end if;
      Sch.Server := The_Server;
      Scheduling_Servers.Set_Scheduler_State(The_Server.all,True);
   end Set_Server;

   ------------
   -- Server --
   ------------

   function Server
     (Sch : Secondary_Scheduler)
     return Mast.Scheduling_Servers.Scheduling_Server_Ref
   is
   begin
      return Sch.Server;
   end Server;

   ------------
   -- Host --
   ------------

   overriding function Host
     (Sch : Secondary_Scheduler)
     return Mast.Processing_Resources.Processing_Resource_Ref
   is
   begin
      -- returns null, since secondary schedulers do not have a host
      return null;
   end Host;


end Mast.Schedulers.Secondary;
