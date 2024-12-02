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

with Ada.Text_IO,
  Mast.Processing_Resources,
  Mast.Schedulers,
  Mast.Shared_Resources,
  Mast.Operations,
  Mast.Transactions,
  Mast.Scheduling_Servers,
  Mast.Results,
  Mast.IO;
with Var_Strings; use Var_Strings;

package Mast.Systems is

   type PIP_Behaviour is (Strict, POSIX);
   -- POSIX priority inheritance is not strict and may lead to larger
   -- blocking times than the Strict inheritance behaviour

   type System is tagged record
      Model_Name           : Var_String:=Null_Var_String;
      Model_Date           : Date:="                   ";
      System_PIP_Behaviour : PIP_Behaviour:=Strict;
      Generation_Tool      : Var_String:=Null_Var_String;
      Generation_Profile   : Var_String:=Null_Var_String;
      Generation_Date      : Date:=Mast.IO.Today;
      Processing_Resources : Mast.Processing_Resources.Lists.List;
      Schedulers           : Mast.Schedulers.Lists.List;
      Shared_Resources     : Mast.Shared_Resources.Lists.List;
      Operations           : Mast.Operations.Lists.List;
      Transactions         : Mast.Transactions.Lists.List;
      Scheduling_Servers   : Mast.Scheduling_Servers.Lists.List;
      The_Slack_Result     : Results.Slack_Result_Ref;
      The_Trace_Result     : Results.Trace_Result_Ref;
      The_Processor_Analysis_Tool       : Mast.Analysis_Tools;
      Inconclusive_Analysis_Results : Boolean:=False; 
   end record;

   procedure Assign_Analysis_Tools_to_Processors
     (User_Analysis : Mast.General_Analysis_Tools;
      isLocal : Boolean := False;
      Sys : in out System;
      Verbose : Boolean := False);

   function Is_In_Use
     (Proc_Ref : Mast.Processing_Resources.Processing_Resource_Ref;
      Sys : System)
     return Boolean;

   procedure Set_Slack_Result
     (Sys : in out System; Res : Results.Slack_Result_Ref);

   function Slack_Result
     (Sys : System) return Results.Slack_Result_Ref;

   procedure Set_Trace_Result
     (Sys : in out System; Res : Results.Trace_Result_Ref);

   function Trace_Result
     (Sys : System) return Results.Trace_Result_Ref;

   function Has_Hard_Timing_Requirements
     (Sys : System; Verbose : Boolean)
     return Boolean;

   procedure Print
     (File : Ada.Text_IO.File_Type;
      The_System : in out System;
      Indentation : Positive:=1);

   procedure Print_Results
     (File : Ada.Text_IO.File_Type;
      The_System : in out System;
      Indentation : Positive:=1);

   procedure Print_XML
     (File : Ada.Text_IO.File_Type;
      The_System : in out System;
      Indentation : Positive:=1);
   
   procedure Print_XML_Results
     (File : Ada.Text_IO.File_Type;
      The_System : in out System;
      Indentation : Positive:=1);

   procedure Print_XMI
     (File : Ada.Text_IO.File_Type;
      The_System : in out System;
      Indentation : Positive:=1);

   procedure Print_XMI_Results
     (File : Ada.Text_IO.File_Type;
      The_System : in out System;
      Indentation : Positive:=1);

   function Clone (The_System : System) return System;
   -- The Clone operation creates a copy of the whole system,
   -- but not including any results

   procedure Adjust(The_System : in out System);

end Mast.Systems;
