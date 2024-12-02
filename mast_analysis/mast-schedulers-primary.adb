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

package body Mast.Schedulers.Primary is

   use type Mast.Scheduling_Policies.Scheduling_Policy_Ref;
   use type Mast.Processing_Resources.Processing_Resource_Ref;
   use type Mast.Processing_Resources.Lists.Index;
   use type Var_Strings.Var_String;
   
   ------------
   -- Adjust --
   ------------

   procedure Adjust
     (Sch : in out Primary_Scheduler;
      Proc_List : Processing_Resources.Lists.List)
   is
      The_Index : Processing_Resources.Lists.Index;
   begin
      if Sch.Host/=null then
         The_Index:=Processing_Resources.Lists.Find
           (Processing_Resources.Name(Sch.Host),
            Proc_List);
         if The_Index=Processing_Resources.Lists.Null_Index then
            Set_Exception_Message
              ("Error in Primary_Scheduler "&Var_Strings.To_String(Sch.Name)&
               ": Processing_Resource "&
               Var_Strings.To_String(Processing_Resources.Name(Sch.Host))&
               " not found");
            raise Object_Not_Found;
         else
            Sch.Host:=Processing_Resources.Lists.Item(The_Index,Proc_List);
         end if;
      end if;
   end Adjust;

   ------------------
   -- Max_Priority --
   ------------------

   overriding function Max_Priority
     (Sch : Primary_Scheduler) return Priority
   is
   begin
      if Sch.Policy.all in Scheduling_Policies.Fixed_Priority_Policy'Class
      then
         return Scheduling_Policies.Max_Priority
           (Scheduling_Policies.Fixed_Priority_Policy'Class(Sch.Policy.all));
      else
         return Priority'First;
      end if;
   end Max_Priority;

   ------------------
   -- Min_Priority --
   ------------------

   overriding function Min_Priority
     (Sch : Primary_Scheduler) return Priority
   is
   begin
      if Sch.Policy.all in Scheduling_Policies.Fixed_Priority_Policy'Class
      then
         return Scheduling_Policies.Min_Priority
           (Scheduling_Policies.Fixed_Priority_Policy'Class(Sch.Policy.all));
      else
         return Priority'First;
      end if;
   end Min_Priority;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host
     (Sch : in out Primary_Scheduler;
      The_Host: Mast.Processing_Resources.Processing_Resource_Ref)
   is
   begin
      if Sch.Host/=null then
         Processing_Resources.Set_Scheduler_State(Sch.Host.all,False);
      end if;
      Sch.Host := The_Host;
      Processing_Resources.Set_Scheduler_State(The_Host.all,True);
   end Set_Host;

   ----------
   -- Host --
   ----------

   overriding function Host
     (Sch : Primary_Scheduler)
     return Mast.Processing_Resources.Processing_Resource_Ref
   is
   begin
      return Sch.Host;
   end Host;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (File : Ada.Text_IO.File_Type;
      Sch : in out Primary_Scheduler;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      Name_Length : constant := 15;
   begin
      Print(File, Scheduler(Sch),Indentation);
      Mast.IO.Print_Arg
        (File,"Type",
         "Primary_Scheduler",Indentation+3,Name_Length);
      Mast.IO.Print_Separator(File);
      Mast.IO.Print_Arg
        (File,"Name",
         IO.Name_Image(Sch.Name),Indentation+3,Name_Length);
      if Sch.Host/=null then
         Mast.IO.Print_Separator(File);
         Mast.IO.Print_Arg
           (File,"Host",
            IO.Name_Image(Mast.Processing_Resources.Name(Sch.Host)),
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

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      Sch : in out Primary_Scheduler;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      -- Name_Length : constant := 15;
   begin
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<mast_mdl:Primary_Scheduler ");
      Ada.Text_IO.Put(File,"Name=""" & IO.Name_Image(Sch.Name) & """ ");
      if Sch.Host/=null then
         Ada.Text_IO.Put
           (File,"Host=""" &
            IO.Name_Image(Mast.Processing_Resources.Name(Sch.Host)) & """ ");
      end if;
      Ada.Text_IO.Put_Line(File," >");
      if Sch.Policy /= null then
         Mast.Scheduling_Policies.Print_XML
           (File,Sch.Policy.all,Indentation+3,False);
      end if;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put_Line(File,"</mast_mdl:Primary_Scheduler> ");
   end Print_XML;

   ---------------
   -- Print_XML --
   ---------------

   overriding procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      Sch : in out Primary_Scheduler;
      Indentation : Positive;
      Finalize    : Boolean:=False)
   is
      -- Name_Length : constant := 15;
   begin
      Mast.XMI.Add_Element_List_Item;
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation));
      Ada.Text_IO.Put(File,"<Element_List");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put_Line(File,"xsi:type=""mast2:Primary_Scheduler""");
      Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
      Ada.Text_IO.Put(File,"name=""" & XMI.XMI_Name_Image(Sch.Name, "sch_") &
         """");
      if Sch.Host/=null then
         Ada.Text_IO.New_Line(File);
         Ada.Text_IO.Set_Col(File,Ada.Text_IO.Count(Indentation+4));
         if Sch.Host.all in Mast.Processing_Resources.Processor.Processor'Class
    then 
            Ada.Text_IO.Put
         (File,"Host=""" &
       XMI.XMI_Name_Image(Mast.Processing_Resources.Name(Sch.Host), 
                "pr_") & """");
         else
            Ada.Text_IO.Put
         (File,"Host=""" &
       XMI.XMI_Name_Image(Mast.Processing_Resources.Name(Sch.Host), 
                "net_") & """");     
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

   ------------
   -- Clone  --
   ------------

   overriding function Clone
     (Sch : Primary_Scheduler)
     return  Scheduler_Ref
   is
      Sch_Ref : Scheduler_Ref;
   begin
      Sch_Ref:=new Primary_Scheduler'(Sch);
      return Sch_Ref;
   end Clone;
   
   --------------------
   -- Find_Scheduler --
   --------------------
   -- Finds and returns the primary scheduler present in The_System
   -- whose Host is Proc_Ref
   -- Returns null if not found
   function Find_Scheduler
     (Proc_Ref : Mast.Processing_Resources.Processing_Resource_Ref;
      The_System : Mast.Systems.System)
     return Primary_Scheduler_Ref
   is
      Sch_Ref : Scheduler_Ref;
      Pri_Sch_Ref : Primary_Scheduler_Ref;
      Iterator : Lists.Index;
   begin
      Lists.Rewind(The_System.Schedulers,Iterator);
      for I in 1..Lists.Size(The_System.Schedulers) loop
         Lists.Get_Next_Item(Sch_Ref,The_System.Schedulers,Iterator);
         if Sch_Ref.all in Primary_Scheduler'Class then
            Pri_Sch_Ref:=Primary_Scheduler_Ref(Sch_Ref);
            if Pri_Sch_Ref.Host.Name=Proc_Ref.Name then
               return Pri_Sch_Ref;
            end if;
         end if;
      end loop;
      return null;
   end Find_Scheduler;
    
   --------------------
   -- Find_Scheduler --
   --------------------
   -- Same as above, except that it takes a Processing resource, 
   -- instead of an access type. 
   -- Finds and returns the primary scheduler present in The_System
   -- whose Host is Proc
   -- Returns null if not found
   function Find_Scheduler
     (Proc : Mast.Processing_Resources.Processing_Resource;
      The_System : Mast.Systems.System)
     return Primary_Scheduler_Ref
   is
      Sch_Ref : Scheduler_Ref;
      Pri_Sch_Ref : Primary_Scheduler_Ref;
      Iterator : Lists.Index;
   begin
      Lists.Rewind(The_System.Schedulers,Iterator);
      
      for I in 1..Lists.Size(The_System.Schedulers) loop
         Lists.Get_Next_Item(Sch_Ref,The_System.Schedulers,Iterator);
         if Sch_Ref.all in Primary_Scheduler'Class then
            Pri_Sch_Ref:=Primary_Scheduler_Ref(Sch_Ref);
            if Pri_Sch_Ref.Host.Name=Proc.Name then
               Ada.Text_IO.Put_Line(Var_Strings.To_String(Proc.Name));
               return Pri_Sch_Ref;
            end if;
         end if;
      end loop;
      return null;
   end Find_Scheduler;
    
end Mast.Schedulers.Primary;
