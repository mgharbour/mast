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
--          Maria Cue              cuem@unican.es                    --
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

with Mast.Processing_Resources,Mast.Timers, Mast.Restrictions,
  Mast.Linear_Translation,  Mast.Tools.Schedulability_Index,
  Mast.Max_Numbers,
  Priority_Queues, Ada.Text_IO,
  Mast.Scheduling_Parameters,
  Mast.Scheduling_Policies,
  Mast.Tool_Exceptions,
  -- Mast.Linear_Translation,
  -- Mast.Processing_Resources,
  -- Mast.Max_Numbers,
  Mast.Scheduling_Servers,
  -- Mast.Scheduling_Parameters,
  Mast.Synchronization_Parameters,
  Mast.Transaction_Operations,
  Mast.Transactions,
  Mast.Graphs, Mast.Graphs.Event_Handlers, Mast.Graphs.Links,
  Mast.Timing_Requirements;
  -- Ada.Text_IO;

use type Mast.Timers.System_Timer_Ref;
use type Mast.Tools.Schedulability_Index.Index;
use Ada.Text_IO;
-- use Ada.Text_IO;
use type Mast.Scheduling_Parameters.Sched_Parameters_Ref;
use type Mast.Synchronization_Parameters.Synch_Parameters_Ref;
use type Mast.Scheduling_Servers.Lists.Index;


package body Mast.Monoprocessor_Tools is


   Debug : constant Boolean := False;
   My_Verbose : constant Boolean := False;

   ------------------------
   -- Priority_Assignment --
   ------------------------

   procedure Priority_Assignment
     (The_System : in out Mast.Systems.System;
      The_Tool: Mast.Tools.Worst_Case_Analysis_Tool;
      Verbose : Boolean:=True)
   is

      type Processor_ID is new Natural;
      type Transaction_ID is new Natural;
      type Task_ID is new Natural;

      Max_Processors:constant Processor_ID:=Processor_ID
        (Processing_Resources.Lists.Size
         (The_System.Processing_Resources));
      Max_Transactions:constant Transaction_ID:=Transaction_ID
        ((Mast.Max_Numbers.Calculate_Max_Transactions(The_System)));
      Max_Tasks_Per_Transaction:constant Task_ID:=Task_ID
        (Mast.Max_Numbers.Calculate_Max_Tasks_Per_Transaction(The_System));

      subtype Processor_ID_Type is Processor_ID
        range 0..Max_Processors;
      subtype Transaction_ID_Type is Transaction_ID
        range 0..Max_Transactions;
      subtype Task_ID_Type is Task_ID
        range 0..Max_Tasks_Per_Transaction;

      package Translation is new Linear_Translation
        (Processor_ID_Type, Transaction_ID_Type, Task_ID_Type,
         Max_Processors, Max_Transactions, Max_Tasks_Per_Transaction);

      use Translation;

      My_System : Translation.Linear_Transaction_System;

      Queue_Length : Natural;
      List_Of_Min_VP : Virtual_Priority;
      List_Of_Min_P, List_Of_Max_P : Priority;
      Optimization_In_Processor : Boolean := True;

      type List_For_Tasks is array
        (Task_ID_Type range 1..Max_Tasks_Per_Transaction)
        of Translation.Priority_Assignment_Vars;

      type List_For_Transactions is array
        (Transaction_ID_Type range 1..Max_Transactions)
        of List_For_Tasks;

      Pavs : List_For_Transactions;

      Sched_Index : Mast.Tools.Schedulability_Index.Index;

      ---------------
      -- Save_Pavs --
      ---------------

      procedure Save_Pavs is
      begin
         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            for J in Task_ID_Type range 1..Max_Tasks_Per_Transaction
            loop
               Pavs(I)(J) := My_System(I).The_Task(J).Pav;
            end loop;
         end loop;
      end Save_Pavs;

      ------------------
      -- Restore_Pavs --
      ------------------

      procedure Restore_Pavs is
      begin
         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            for J in Task_ID_Type range 1..Max_Tasks_Per_Transaction
            loop
               My_System(I).The_Task(J).Pav := Pavs(I)(J);
            end loop;
         end loop;
      end Restore_Pavs;

      ------------------------------------
      -- Deadlines_Greater_Than_Periods --
      ------------------------------------

      function Deadlines_Greater_Than_Periods return Boolean is
      begin
         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            exit when My_System(I).Ni=0;
            for J in 1..My_System(I).Ni
            loop
               if not My_System(I).The_Task(J).Pav.Hard_Prio and then
                 not My_System(I).The_Task(J).Pav.Is_Polling
               then
                  if My_System(I).The_Task(J).Dij >
                    My_System(I).The_Task(J).Tij 
                  then
                     return True;
                  end if;
               end if;
            end loop;
         end loop;
         return False;
      end Deadlines_Greater_Than_Periods;

      ----------------------
      -- Priority_Mapping --
      ----------------------

      procedure Priority_Mapping is

         type Priority_Element is record
            Transaction : Transaction_ID_Type;
            Action : Task_ID_Type;
         end record;

         package VP_Lists is new Priority_Queues
           (Element => Priority_Element,
            Priority => Virtual_Priority,
            ">" => ">",
            "=" => "=");

         -- use VP_Lists;

         package P_Lists is new Priority_Queues
           (Element => Priority_Element,
            Priority => Priority,
            ">" => ">",
            "=" => "=");

         -- use P_Lists;

         Lists_For_VP : VP_Lists.Queue;
         Lists_For_P : P_Lists.Queue;

         Item1, Item2 : Priority_Element;
         Max_P, Min_P : Priority;
         Max_VP, Min_VP, VP : Virtual_Priority;

      begin

         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            exit when My_System(I).Ni=0;
            for J in 1..My_System(I).Ni
            loop
               Item1 := (Transaction => I,
                         Action => J);
               if not My_System(I).The_Task(J).Pav.Hard_Prio and then
                 not My_System(I).The_Task(J).Pav.Is_Polling 
               then
                  if  My_System(I).The_Task(J).Pav.Preassigned then
                     P_Lists.Enqueue
                       (Item1,
                        My_System(I).The_Task(J).Pav.Preassigned_Prio,
                        Lists_For_P);
                  else
                     VP_Lists.Enqueue
                       (Item1,
                        My_System(I).The_Task(J).Pav.Virtual_Prio,
                        Lists_For_VP);
                  end if;
               end if;
            end loop;
         end loop;

         Min_P := List_Of_Max_P;
         Min_VP := Max_Virtual_Priority;
         -- Set minumum priorities to maximum values

         while not VP_Lists.Empty(Lists_For_VP)
         loop
            VP_Lists.Dequeue(Item2,VP,Lists_For_VP);
            if VP <= Min_VP then
               -- New stage
               Max_P := Min_P;
               Max_VP := Min_VP;
               loop
                  if not P_Lists.Empty(Lists_For_P) then
                     P_Lists.Dequeue(Item1,Min_P,Lists_For_P);
                     Min_P := Priority'Max(Min_P,List_Of_Min_P);
                     Min_VP :=
                       My_System(Item1.Transaction).
                       The_Task(Item1.Action).Pav.Virtual_Prio;
                     if VP > Min_VP then
                        exit;
                     end if;
                  else
                     Min_P := List_Of_Min_P;
                     Min_VP := List_Of_Min_VP;
                     exit;
                  end if;
               end loop;
            end if;
            if Debug then
               Put("   VP = "&Virtual_Priority'Image(VP));
               Put("   Max_P = "&Priority'Image(Max_P));
               Put("   Min_P = "&Priority'Image(Min_P));
               Put("   Max_VP = "&Virtual_Priority'Image(Max_VP));
               Put_Line("   Min_VP = "&Virtual_Priority'Image(Min_VP));
            end if;

            My_System(Item2.Transaction).The_Task(Item2.Action).Prioij :=
              Priority
              (Long_Float'Floor
               ((Long_Float(VP)-Long_Float(Min_VP))/
                (Long_Float(Max_VP)-Long_Float(Min_VP)+1.0)*
                (Long_Float(Max_P)-Long_Float(Min_P)+1.0)+
                Long_Float(Min_P)));

            if Debug then
               Put_Line("   Priority Assigned = "&
                        Priority'Image
                        (My_System(Item2.Transaction).
                         The_Task(Item2.Action).Prioij));
            end if;

         end loop;

      end Priority_Mapping;

      -----------------------------------
      -- Deadline_Monotonic_Assignment --
      -----------------------------------

      procedure Deadline_Monotonic_Assignment is

         type Deadline_Element is record
            Transaction : Transaction_ID_Type;
            Action : Task_ID_Type;
         end record;

         package Deadlines_Lists is new Priority_Queues
           (Element => Deadline_Element,
            Priority => Time,
            ">" => "<",
            "=" => "=");

         -- use Deadlines_Lists;

         List_For_Processor : Deadlines_Lists.Queue;
         Item : Deadline_Element;
         Next_Priority : Virtual_Priority;
         Deadline : Time;

      begin

         -- Save Preassigned_Prio

         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            exit when My_System(I).Ni=0;
            for J in 1..My_System(I).Ni
            loop
               My_System(I).The_Task(J).Pav.Preassigned_Prio :=
                 My_System(I).The_Task(J).Prioij;
            end loop;
         end loop;

         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            exit when My_System(I).Ni=0;
            for J in 1..My_System(I).Ni
            loop
               if not My_System(I).The_Task(J).Pav.Hard_Prio and then
                 not My_System(I).The_Task(J).Pav.Is_Polling
               then
                  Item := (Transaction => I,
                           Action => J);
                  Deadlines_Lists.Enqueue
                    (Item,
                     My_System(I).The_Task(J).Dij,
                     List_For_Processor);
               end if;
            end loop;
         end loop;

         declare
            J : Integer := 0;
            Background_Priority : Priority:= Priority'First;
         begin
            while not Deadlines_Lists.Empty(List_For_Processor)
            loop
               J := J+1;
               Deadlines_Lists.Dequeue
                 (Item,Deadline,List_For_Processor);
               if J=1 then
                  Next_Priority := Max_Virtual_Priority;
               else
                  Next_Priority := Virtual_Priority'Pred(Next_Priority);
               end if;
               My_System(Item.Transaction).
                 The_Task(Item.Action).Pav.Virtual_Prio := Next_Priority;
               if Debug then
                  Put(" Next_VP = "&
                      Virtual_Priority'Image(Next_Priority));
                  Put(" with Deadline = "&
                      Time'Image(Deadline));
                  Put(" at the position "&Integer'Image(J));
                  Put_Line(" Preasssigned="&Boolean'Image
                      (My_System(Item.Transaction).The_Task(Item.Action).
                       Pav.Preassigned));
               end if;
               if My_System(Item.Transaction).The_Task(Item.Action).
                 Pav.Preassigned 
               then
                  -- Check if the optimum priority assigment will be
                  -- performed for this processor
                  Optimization_In_Processor := False;
               end if;
               if My_System(Item.Transaction).The_Task(Item.Action).
                 Pav.S_P_Ref.all in
                 Scheduling_Parameters.Sporadic_Server_Policy'Class
               then
                  begin
                     if Background_Priority <=
                       Mast.Scheduling_Parameters.
                       Background_Priority
                       (Mast.Scheduling_Parameters.
                        Sporadic_Server_Policy
                        (My_System(Item.Transaction).
                         The_Task(Item.Action).Pav.S_P_Ref.all))
                     then
                        Background_Priority :=
                          Mast.Scheduling_Parameters.
                          Background_Priority
                          (Mast.Scheduling_Parameters.
                           Sporadic_Server_Policy
                           (My_System(Item.Transaction).
                            The_Task(Item.Action).Pav.S_P_Ref.all))+1;
                     end if;
                  exception
                     when Constraint_Error =>
                        Background_Priority :=
                          Mast.Scheduling_Parameters.
                          Background_Priority
                          (Mast.Scheduling_Parameters.
                           Sporadic_Server_Policy
                           (My_System(Item.Transaction).
                            The_Task(Item.Action).Pav.S_P_Ref.all));
                  end;
               end if;

            end loop;
            Queue_Length := J;
            -- Store the number of task for optimizing
            -- priority assignment
            List_Of_Min_VP := Next_Priority;
            -- Store the minimum virtual priority assigned for the
            -- processor
            if Background_Priority > Scheduling_Policies.Min_Priority
              (My_System(Item.Transaction).The_Task(Item.Action).
               Pav.S_Policy_Ref.all)
            then
               List_Of_Min_P := Background_Priority;
            else
               List_Of_Min_P := Scheduling_Policies.Min_Priority
                 (My_System(Item.Transaction).The_Task(Item.Action).
                  Pav.S_Policy_Ref.all);
            end if;
            -- Store the maximum priority for the processor
            List_Of_Max_P := Scheduling_Policies.Max_Priority
              (My_System(Item.Transaction).The_Task(Item.Action).
               Pav.S_Policy_Ref.all);
            -- Store the maximum priority for the processor
            if J > Integer (List_Of_Max_P-List_Of_Min_P+1)
            then
               -- Check if the optimum priority assigment will be
               -- performed for the processor
               Optimization_In_Processor := False;
            end if;
         end;

         Priority_Mapping;

      end Deadline_Monotonic_Assignment;

      ---------------------------------
      -- Optimum_Priority_Assignment --
      ---------------------------------

      procedure Optimum_Priority_Assignment is

         type Task_Element is record
            Transaction : Transaction_ID_Type;
            Action : Task_ID_Type;
         end record;

         type Lists_Of_Task is
           array(Integer range 1..Queue_Length)
           of Task_Element;
         List_Of_Task : Lists_Of_Task;
         Num_Of_Task : Integer := 0;
         Item : Task_Element;
         Prio : Priority;
         Failed : Boolean;


      begin

         -- Extract the tasks
         for I in Transaction_ID_Type range 1..Max_Transactions
         loop
            exit when My_System(I).Ni=0;
            for J in 1..My_System(I).Ni
            loop
               if not My_System(I).The_Task(J).Pav.Hard_Prio and then
                 not My_System(I).The_Task(J).Pav.Is_Polling
               then
                  Num_Of_Task := Num_Of_Task+1;
                  List_Of_Task(Num_Of_Task).Transaction := I;
                  List_Of_Task(Num_Of_Task).Action := J;
               end if;
            end loop;
         end loop;

         if Debug then
            Put_Line("Initial priorities.....");
            for I in 1..Num_Of_Task
            loop
               Put("trans : "&Transaction_ID_Type'Image
                   (List_Of_Task(I).Transaction));
               Put("task : "&Task_ID_Type'Image
                   (List_Of_Task(I).Action));
               Put("   deadline = "&Time'Image
                   (My_System(List_Of_Task(I).Transaction).
                    The_Task(List_Of_Task(I).Action).Dij));
               Put_Line("   prioridad = "&Priority'Image
                        (My_System(List_Of_Task(I).Transaction).
                         The_Task(List_Of_Task(I).Action).Prioij));
            end loop;
            New_Line;
         end if;

         -- Order the tasks
         for I in 2..(Num_Of_Task)
         loop
            for J in reverse I..(Num_Of_Task)
            loop
               if My_System(List_Of_Task(J).Transaction).
                 The_Task(List_Of_Task(J).Action).Prioij >
                 My_System(List_Of_Task(J-1).Transaction).
                     The_Task(List_Of_Task(J-1).Action).Prioij 
               then
                  Item := List_Of_Task(J);
                  List_Of_Task(J) := List_Of_Task(J-1);
                  List_Of_Task(J-1) := Item;
               end if;
            end loop;
         end loop;

         if Debug then
            Put_Line("Ordered priorities.....");
            for I in 1..Num_Of_Task
            loop
               Put("trans : "&Transaction_ID_Type'Image
                   (List_Of_Task(I).Transaction));
               Put("task : "&Task_ID_Type'Image
                   (List_Of_Task(I).Action));
               Put("   deadline = "&Time'Image
                   (My_System(List_Of_Task(I).Transaction).
                    The_Task(List_Of_Task(I).Action).Dij));
               Put_Line("   prioridad = "&Priority'Image
                        (My_System(List_Of_Task(I).Transaction).
                         The_Task(List_Of_Task(I).Action).Prioij));
            end loop;
            New_Line;
         end if;

         -- Apply the priority assignment algorithm
         for I in reverse 2..Num_Of_Task
         loop
            Failed := True;
            for J in reverse 1..I
            loop
               -- Exchange tasks I and J and their priorities
               Item := List_Of_Task(I);
               List_Of_Task(I) := List_Of_Task(J);
               List_Of_Task(J) := Item;
               Prio := My_System(List_Of_Task(I).Transaction).
                 The_Task(List_Of_Task(I).Action).Prioij;
               My_System(List_Of_Task(I).Transaction).
                 The_Task(List_Of_Task(I).Action).Prioij :=
                 My_System(List_Of_Task(J).Transaction).
                 The_Task(List_Of_Task(J).Action).Prioij;
               My_System(List_Of_Task(J).Transaction).
                 The_Task(List_Of_Task(J).Action).Prioij := Prio;

               if Debug then
                  Put_Line("Changing priorities.....");
                  for L in 1..Num_Of_Task
                  loop
                     Put("trans : "&Transaction_ID_Type'Image
                         (List_Of_Task(L).Transaction));
                     Put("task : "&Task_ID_Type'Image
                         (List_Of_Task(L).Action));
                     Put("   deadline = "&Time'Image
                         (My_System(List_Of_Task(L).Transaction).
                          The_Task(List_Of_Task(L).Action).Dij));
                     Put_Line("   prioridad = "&Priority'Image
                              (My_System(List_Of_Task(L).
                                         Transaction).
                               The_Task(List_Of_Task(L).Action).
                               Prioij));
                  end loop;
                  New_Line;
               end if;

               Translation.Translate_Priorities(My_System,The_System);
               Save_Pavs;

               -- Check if task I is schedulable

               Mast.Tools.Calculate_Ceilings_And_Levels
                 (The_System,My_Verbose);
               Translation.Clear_Time_Results(My_System,The_System);
               The_Tool(The_System,My_Verbose);
               Translation.Translate_Linear_System_With_Results
                 (The_System,My_System,My_Verbose);
               Restore_Pavs;
               if My_System(List_Of_Task(I).Transaction).
                 The_Task(List_Of_Task(I).Action).Dij >=
                 My_System(List_Of_Task(I).Transaction).
                     The_Task(List_Of_Task(I).Action).Rij 
               then
                  Failed := False;
                  exit;
               else
                  -- Exchange tasks I and J and their old priorities
                  Item := List_Of_Task(I);
                  List_Of_Task(I) := List_Of_Task(J);
                  List_Of_Task(J) := Item;
                  Prio := My_System(List_Of_Task(I).Transaction).
                    The_Task(List_Of_Task(I).Action).Prioij;
                  My_System(List_Of_Task(I).Transaction).
                    The_Task(List_Of_Task(I).Action).Prioij :=
                    My_System(List_Of_Task(J).Transaction).
                    The_Task(List_Of_Task(J).Action).Prioij;
                  My_System(List_Of_Task(J).Transaction).
                    The_Task(List_Of_Task(J).Action).Prioij := Prio;
                  Translation.Translate_Priorities(My_System,The_System);
                  Save_Pavs;
                  if Debug then
                     Put_Line("Returning to old priorities....");
                     for L in 1..Num_Of_Task
                     loop
                        Put("trans : "&Transaction_ID_Type'Image
                            (List_Of_Task(L).Transaction));
                        Put("task : "&Task_ID_Type'Image
                            (List_Of_Task(L).Action));
                        Put("   deadline = "&Time'Image
                            (My_System(List_Of_Task(L).
                                       Transaction).
                             The_Task(List_Of_Task(L).
                                      Action).Dij));
                        Put_Line("   prioridad = "&Priority'Image
                                 (My_System(List_Of_Task(L).
                                            Transaction).
                                  The_Task(List_Of_Task(L).Action).
                                  Prioij));
                     end loop;
                     New_Line;
                  end if;
               end if;
            end loop;
            exit when Failed;
         end loop;

         if Debug then
            Put_Line("Final assignment....");
            for I in 1..Num_Of_Task
            loop
               Put("trans : "&Transaction_ID_Type'Image
                   (List_Of_Task(I).Transaction));
               Put("   deadline = "&Time'Image
                   (My_System(List_Of_Task(I).Transaction).
                    The_Task(List_Of_Task(I).Action).Dij));
               Put_Line("   prioridad = "&Priority'Image
                        (My_System(List_Of_Task(I).Transaction).
                         The_Task(List_Of_Task(I).Action).Prioij));
            end loop;
            New_Line;
         end if;

      end Optimum_Priority_Assignment;


   begin
      Translation.Translate_Linear_System
        (The_System,My_System,My_Verbose);

      Deadline_Monotonic_Assignment;
      if Deadlines_Greater_Than_Periods and then Optimization_In_Processor then
         Optimum_Priority_Assignment;
      end if;

      Translation.Translate_Priorities(My_System,The_System);

      if Verbose then
         Mast.Tools.Calculate_Ceilings_And_Levels
           (The_System,My_Verbose);
         Translation.Clear_Time_Results(My_System,The_System);
         The_Tool(The_System,My_Verbose);
         Translation.Translate_Linear_System_With_Results
           (The_System,My_System,My_Verbose);
         Mast.Tools.Schedulability_Index.Calculate_Schedulability_Index
           (The_System,Sched_Index,My_Verbose);
         Put("Monoprocessor Priority Assignment with "&
             "Schedulability Index : ");
         Mast.Tools.Schedulability_Index.Print(Sched_Index);
      end if;

   end Priority_Assignment;

   -----------------
   -- RM_Analysis --
   -----------------

   procedure RM_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last)
   is
      type Processor_ID is new Natural;
      type Transaction_ID is new Natural;
      type Task_ID is new Natural;
      Max_Processors:constant Processor_ID:=Processor_ID
        (Processing_Resources.Lists.Size
         (The_System.Processing_Resources));
      Max_Transactions:constant Transaction_ID:=
        Transaction_ID((Mast.Max_Numbers.Calculate_Max_Transactions
                        (The_System)));
      Max_Tasks_Per_Transaction:constant Task_ID:=Task_ID
        (Mast.Max_Numbers.Calculate_Max_Tasks_Per_Transaction(The_System));


      subtype Processor_ID_Type is Processor_ID
        range 0..Max_Processors;
      subtype Transaction_ID_Type is Transaction_ID
        range 0..Max_Transactions;
      subtype Task_ID_Type is Task_ID
        range 0..Max_Tasks_Per_Transaction;

      package Translation is new Linear_Translation
        (Processor_ID_Type, Transaction_ID_Type, Task_ID_Type,
         Max_Processors, Max_Transactions, Max_Tasks_Per_Transaction);

      use Translation;

      Transaction : Linear_Transaction_System;
      Ci, Bi,Ri,Rbi, Rik,Wik, Wiknew, Ti, Ji, Di : Time;
      K : Transaction_ID;
      Ni : Task_ID_Type;
      Jeffect : constant array(Boolean) of Time:=(False=> 1.0, True=> 0.0);
      Unbounded_Time : Boolean;
      Analysis_Was_Aborted : Boolean;

   begin
      Translate_Linear_System(The_System,Transaction,Verbose);
      Translation.Clear_Time_Results(Transaction,The_System);
      if Verbose then
         Show_Linear_Translation(Transaction);
      end if;


      -- Loop for each transaction, I, under analysis
      for I in 1..Max_Transactions loop
         exit when Transaction(I).Ni=0;
         -- Calculate Rbi,Ci, Ji, Bi: only last task in the transaction is
         -- analyzed; other tasks in same transaction are considered
         -- independent
         Ni:=Transaction(I).Ni;
         Ci:=Transaction(I).The_Task(Ni).Cijown;
         Ti:=Transaction(I).The_Task(Ni).Tijown;
         Ji:=Transaction(I).The_Task(Ni).Jinit;
         Bi:=Transaction(I).The_Task(Ni).Bij;
         Di:=Transaction(I).The_Task(Ni).Dij;
         Rbi:=Transaction(I).The_Task(Ni).Cbijown;
         -- Calculate initial value for Wik
         Unbounded_Time:=False;
         Analysis_Was_Aborted:=False;
         Wik:=Bi+Ci;
         Ri:=0.0;
         -- Transaction(I).The_Task(Ni).Rbij:=Wik;
         for J in 1..Max_Transactions loop
            exit when Transaction(J).Ni=0;
            for Tsk in 1..Transaction(J).Ni loop
               if  Transaction(J).The_Task(Tsk).Prioij>=
                 Transaction(I).The_Task(Ni).Prioij and then
                 (J/=I or else Tsk/=Ni)
               then
                  Wik:=Wik+Transaction(J).The_Task(Tsk).Cij;
               end if;
            end loop;
         end loop;
         Transaction(I).The_Task(Ni).Rbij:=Rbi;
         -- Iterate over the jobs, K, in the busy period
         K:=1;
         loop
            -- Iterate until equation converges
            loop
               Wiknew:=Bi+Time(K)*Ci;
               -- add contributions of high priority tasks
               for J in 1..Max_Transactions loop
                  exit when Transaction(J).Ni=0;
                  for Tsk in 1..Transaction(J).Ni loop
                     if  Transaction(J).The_Task(Tsk).Prioij>=
                       Transaction(I).The_Task(Ni).Prioij and then
                       (J/=I or else Tsk/=Ni)
                     then
                        if Transaction(J).The_Task(Tsk).Model=
                          Unbounded_Effects
                        then
                           Wiknew:= Large_Time;
                           Wik:=Large_Time;
                           Unbounded_Time:=True;
                           exit;
                        else
                           Wiknew:=Wiknew+Ceiling
                             ((Wik+Transaction(J).The_Task(Tsk).Jinit*
                               Jeffect(Transaction(J).The_Task(Tsk).
                                       Jitter_Avoidance))/
                              Transaction(J).The_Task(Tsk).Tij)*
                             Transaction(J).The_Task(Tsk).Cij;
                        end if;
                     end if;
                  end loop;
                  exit when Unbounded_Time;
               end loop;
               exit when Unbounded_Time or else Wik=Wiknew;
               Wik:=Wiknew;
               --- Determine if response time is higher than deadline
               if (Stop_Factor_When_Not_Schedulable/=Positive'Last) and then
                 Wik-Ti*(Time(K)-1.0)+Ji>
                 Time(Stop_Factor_When_Not_Schedulable)*Di
               then
                  Analysis_Was_Aborted:=True;
                  exit;
               end if;
            end loop;
            exit when Unbounded_Time or Analysis_Was_Aborted;

            Rik:=Wik-Ti*(Time(K)-1.0);
            -- keep the worst case result
            if Rik>Ri then
               Ri:=Rik;
            end if;
            -- check if busy period is too long
            if Wik>Analysis_Bound then
               Unbounded_Time:=True;
               exit;
            end if;
            -- determine if busy period is over
            exit when Rik<=Ti;
            K:=K+1;
            Wik:=Wik+Ci;
         end loop;

         -- Store the worst-case response time obtained
         if Unbounded_Time or Analysis_Was_Aborted then
            Transaction(I).The_Task(Ni).Rij:=Large_Time;
            Transaction(I).The_Task(Ni).Jij:=Large_Time;
            if Verbose and then Analysis_Was_Aborted then
               Put_Line("Response time of task "&
                        Transaction_ID'Image(I)&
                          " exceeds the task deadline");
            end if;
         else
            Transaction(I).The_Task(Ni).Rij:=Ri+Ji;
            Transaction(I).The_Task(Ni).Jij:=Ri+Ji-Rbi;
         end if;
      end loop;
      Translate_Linear_Analysis_Results(Transaction,The_System);
   end RM_Analysis;

   ---------------------------------
   -- Varying_Priorities_Analysis --
   ---------------------------------

   procedure Varying_Priorities_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last)
   is
      type Processor_ID is new Natural;
      type Transaction_ID is new Natural;
      type Task_ID is new Natural;
      Max_Processors:constant Processor_ID:=Processor_ID
        (Processing_Resources.Lists.Size
         (The_System.Processing_Resources));
      Max_Transactions:constant Transaction_ID:=
        Transaction_ID((Mast.Max_Numbers.Calculate_Max_Transactions
                        (The_System)));
      Max_Tasks_Per_Transaction:constant Task_ID:=Task_ID
        (Mast.Max_Numbers.Calculate_Max_Tasks_Per_Transaction(The_System));

      subtype Processor_ID_Type is Processor_ID
        range 0..Max_Processors;
      subtype Transaction_ID_Type is Transaction_ID
        range 0..Max_Transactions;
      subtype Task_ID_Type is Task_ID
        range 0..Max_Tasks_Per_Transaction;

      package Translation is new Linear_Translation
        (Processor_ID_Type, Transaction_ID_Type, Task_ID_Type,
         Max_Processors, Max_Transactions, Max_Tasks_Per_Transaction);

      use Translation;

      procedure Find_Canonical_Form
        (Original  : Transaction_Data;
         Canonical : out Transaction_Data)
      is
      begin
         Canonical:=Original;
         for J in reverse 2..Canonical.Ni loop
            if Canonical.The_Task(J-1).Prioij>Canonical.The_Task(J).Prioij then
               Canonical.The_Task(J-1).Prioij:=Canonical.The_Task(J).Prioij;
            end if;
         end loop;
      end Find_Canonical_Form;

      type H_Set_Parameters is record
         Ci,Ji,Ti : Time;
         Pi : Priority;
         Trans : Transaction_ID;
      end record;

      type Time_Array is array(Transaction_ID_Type) of Time;
      type H_Set_Array is array(Transaction_ID_Type) of H_Set_Parameters;

      procedure Find_Sets
        (Transaction : Linear_Transaction_System;
         I           : Transaction_ID;
         Prio        : Priority;
         H           : out H_Set_Array;
         NumH        : out Transaction_ID;
         HL          : out Time_Array;
         NumHL       : out Transaction_ID;
         MaxLH       : out Time;
         Unbounded_Time : out Boolean)
      is
         Ci : Time;
         Is_H_Segment, Finished : Boolean;
         L_Segment : Task_ID_Type;
         Min_Prio : Priority;
         Jeffect : constant array(Boolean) of Time:=(False=> 1.0, True=> 0.0);
      begin
         Unbounded_Time:=False;
         NumH:=0;
         NumHL:=0;
         MaxLH:=0.0;
         -- loop for each transaction except the one under analysis
         for J in 1..Max_Transactions loop
            exit when Transaction(J).Ni=0;
            if J/=I then
               Ci:=0.0;
               Is_H_Segment:=True;
               L_Segment:=1;
               Min_Prio:=Priority'Last;
               -- Loop for all actions in the transaction
               for Tsk in 1..Transaction(J).Ni loop
                  if Transaction(J).The_Task(Tsk).Prioij<Min_Prio then
                     Min_Prio:=Transaction(J).The_Task(Tsk).Prioij;
                  end if;
                  if Transaction(J).The_Task(Tsk).Prioij>=Prio then
                     if Transaction(J).The_Task(Tsk).Model=Unbounded_Effects
                     then
                        Unbounded_Time:=True;
                        return;
                     end if;
                     Ci:=Ci+Transaction(J).The_Task(Tsk).Cij;
                  else
                     Is_H_Segment:=False;
                     if Tsk>1 then
                        NumHL:=NumHL+1;
                        HL(NumHL):=Ci;
                        L_Segment:=Tsk;
                     end if;
                     exit;
                  end if;
               end loop;
               if Is_H_Segment then
                  NumH:=NumH+1;
                  H(NumH).Ci:=Ci;
                  H(NumH).Ji:=Transaction(J).The_Task(1).Jinit*
                    Jeffect(Transaction(J).The_Task(1).Jitter_Avoidance);
                  for Tsk in 2..Transaction(J).Ni loop
                     H(NumH).Ji:=H(NumH).Ji+Transaction(J).The_Task(1).Jinit*
                       Jeffect(Transaction(J).The_Task(1).Jitter_Avoidance);
                  end loop;
                  H(NumH).Ti:=Transaction(J).The_Task(1).Tij;
                  H(NumH).Pi:=Min_Prio;
                  H(NumH).Trans:=J;
               else
                  -- Search for LH segments
                  Finished :=False;
                  while L_Segment<Transaction(J).Ni and then not Finished loop
                     Ci:=0.0;
                     Finished:=True;
                     for Tsk in L_Segment+1..Transaction(J).Ni loop
                        if Transaction(J).The_Task(Tsk).Prioij>=Prio then
                           Ci:=Ci+Transaction(J).The_Task(Tsk).Cij;
                        else
                           Finished:=False;
                           L_Segment:=Tsk;
                           exit;
                        end if;
                     end loop;
                     if Ci>MaxLH then
                        MaxLH:=Ci;
                     end if;
                  end loop;
               end if;
            end if;
         end loop;
      end Find_Sets;

      procedure Recalculate_Sets
        (Transaction : Linear_Transaction_System;
         Prio        : Priority;
         H           : H_Set_Array;
         NumH        : Transaction_ID;
         H_New       : out H_Set_Array;
         NumH_new    : out Transaction_ID;
         HL_new      : out Time_Array;
         NumHL_new   : out Transaction_ID;
         Diff        : out H_Set_Array;
         N_Diff      : out Transaction_ID)
      is
         Ci : Time;
         I : Transaction_ID;
      begin
         NumH_new:=0;
         NumHL_new:=0;
         N_Diff:=0;
         for K in 1..NumH loop
            if H(K).Pi>=Prio then
               -- Is H transaction
               NumH_new:=NumH_new+1;
               H_New(NumH_new):=H(K);
            else
               -- Is in the difference (was H transaction, and now it's not)
               N_Diff:=N_Diff+1;
               Diff(N_Diff):=H(K);
               I:=H(K).Trans;
               if Transaction(I).The_Task(1).Prioij>=Prio then
                  -- Is HL transaction
                  Ci:=Transaction(I).The_Task(1).Cij;
                  for Tsk in 2..Transaction(I).Ni loop
                     if Transaction(I).The_Task(Tsk).Prioij>=Prio then
                        Ci:=Ci+Transaction(I).The_Task(Tsk).Cij;
                     else
                        exit;
                     end if;
                  end loop;
                  NumHL_new:=NumHL_new+1;
                  HL_new(NumHL_new):=Ci;
               end if;
            end if;
         end loop;
      end Recalculate_Sets;

      Canonical : Transaction_Data;
      Transaction : Linear_Transaction_System;
      H1,H_Old,H_New,Diff : H_Set_Array;
      NumH1,NumH_Old,NumH_New,N_Diff : Transaction_ID;
      HL1,HL_New : Time_Array;
      NumHL1,NumHL_New : Transaction_ID;
      MaxLH : Time;
      Ni : Task_ID_Type;
      Prio_Level : Priority;
      S, Wik, Wiknew, Ti, Bi, Ci, Di, Ci1, Rij, Ri, Rik, Rbi : Time;
      Unbounded_Time : Boolean;
      Analysis_Was_Aborted : Boolean;
      PCP_Only : Boolean;
      NumJobs : Natural;

   begin
      Translate_Linear_System(The_System,Transaction,Verbose);
      Translation.Clear_Time_Results(Transaction,The_System);
      PCP_Only := Restrictions.PCP_Only(The_System,False);
      --show_linear_translation(Transaction);

      -- Loop for each transaction, I, under analysis
      for I in 1..Max_Transactions loop
         exit when Transaction(I).Ni=0;
         -- Calculate best-case response times
         Analysis_Was_Aborted:=False;
         Rbi:=0.0;
         for Tsk in 1..Transaction(I).Ni loop
            Rbi:=Rbi+Transaction(I).The_Task(Tsk).Cbijown;
            Transaction(I).The_Task(Tsk).Rbij:=Rbi;
         end loop;
         -- Find canonical form
         Find_Canonical_Form(Transaction(I),Canonical);
         Ni:=Canonical.Ni;
         Di:=Canonical.The_Task(Ni).Dij;
         Prio_Level:=Canonical.The_Task(1).Prioij;
         Find_Sets(Transaction,I,Prio_Level,H1,NumH1,
                   HL1,NumHL1,MaxLH,Unbounded_Time);
         if Unbounded_Time or Analysis_Was_Aborted then
            for J in 1..Ni loop
               Transaction(I).The_Task(J).Rij:=Large_Time;
            end loop;
         else
            Ti:=Canonical.The_Task(1).Tijown;

            -- Determine Ci and Bi by taking into account blocking times
            -- of all segments
            Bi:=0.0;
            Ci:=0.0;
            for J in 1..Ni loop
               Ci:=Ci+Canonical.The_Task(J).Cijown;
               if PCP_Only then
                  if Canonical.The_Task(J).Bij>MaxLH then
                     MaxLH:=Canonical.The_Task(J).Bij;
                  end if;
               else
                  Bi:=Bi+Canonical.The_Task(J).Bij;
               end if;
            end loop;

            -- Determine the length of the busy period
            S:=Bi+MaxLH;
            for K in 1..NumHL1 loop
               S:=S+HL1(K);
            end loop;
            Wik:=S+Ci;
            for K in 1..NumH1 loop
               Wik:=Wik+H1(K).Ci;
            end loop;
            NumJobs:=0;
            Ri:=0.0;
            loop
               NumJobs:=NumJobs+1;
               loop
                  Wiknew:=S+Time(NumJobs)*Ci;
                  for K in 1..NumH1 loop
                     Wiknew:=Wiknew+Ceiling((Wik+H1(K).Ji)/
                                                 H1(K).Ti)*H1(K).Ci;
                  end loop;

                  exit when Wik=Wiknew;
                  Wik:=Wiknew;
               end loop;
               Rik:=Wik-Ti*Time(NumJobs-1);
               --- Determine if response time is higher than deadline
               if Stop_Factor_When_Not_Schedulable/=Positive'Last and then
                 Rik+Transaction(I).The_Task(1).Jinit>
                 Time(Stop_Factor_When_Not_Schedulable)*Di
               then
                  Analysis_Was_Aborted:=True;
                  exit;
               end if;
               -- keep the worst case result
               if Rik>Ri then
                  Ri:=Rik;
               end if;
               -- check if busy period is too long
               if Wik>Analysis_Bound then
                  Unbounded_Time:=True;
                  exit;
               end if;
               -- determine if busy period is over
               exit when Rik<=Ti;
               Wik:=Wik+Ci;
            end loop;
            NumJobs:=Natural(Ceiling(Wik/Ti));

            -- Store the worst-case response time obtained
            if Unbounded_Time or Analysis_Was_Aborted then
               for J in 1..Ni loop
                  Transaction(I).The_Task(J).Rij:=Large_Time;
               end loop;
               if Verbose and then Analysis_Was_Aborted then
                  Put_Line("Response time of task "&
                             Transaction_ID'Image(I)&
                             " exceeds the task deadline");
               end if;
               Transaction(I).The_Task(Ni).Jij:=Large_Time;
            else
               -- Initialize the completion times
               for J in 1..Ni loop
                  Transaction(I).The_Task(J).Rij:=0.0;
               end loop;

               -- loop for all jobs in the busy period
               for Job in 1..NumJobs loop
                  -- Calculate response time of the job-th job of first action
                  Ci1:=Canonical.The_Task(1).Cijown;
                  S:=Bi+Ci1+Time(Job-1)*Ci+MaxLH;
                  for K in 1..NumHL1 loop
                     S:=S+HL1(K);
                  end loop;
                  Wik:=S;
                  for K in 1..NumH1 loop
                     Wik:=Wik+H1(K).Ci;
                  end loop;
                  loop
                     Wiknew:=S;
                     for K in 1..NumH1 loop
                        Wiknew:=Wiknew+Ceiling((Wik+H1(K).Ji)/H1(K).Ti)*
                          H1(K).Ci;
                     end loop;
                     exit when Wik=Wiknew;
                     Wik:=Wiknew;
                  end loop;
                  Rij:=Wik-Time(Job-1)*Ti;
                  if Rij > Transaction(I).The_Task(1).Rij and then
                    Prio_Level=Transaction(I).The_Task(1).Prioij
                  then
                     Transaction(I).The_Task(1).Rij:=Rij;
                  end if;
                  H_Old:=H1;
                  NumH_Old:=NumH1;
                  -- loop for all actions, j, in the canonical form
                  for J in 2..Ni loop
                     -- Reclassify event sequences relative to action aij
                     Prio_Level:=Canonical.The_Task(J).Prioij;
                     Recalculate_Sets
                       (Transaction,Prio_Level,H_Old,NumH_Old,H_New,
                        NumH_New,HL_New,NumHL_New,Diff,N_Diff);
                     -- Calculate response time of the job-th job of aij
                     S:=S+Canonical.The_Task(J).Cijown;
                     for K in 1..NumHL_New loop
                        S:=S+HL_New(K);
                     end loop;
                     for K in 1..N_Diff loop
                        S:=S+Ceiling((Wik+Diff(K).Ji)/
                                          Diff(K).Ti)*Diff(K).Ci;
                     end loop;
                     loop
                        Wiknew:=S;
                        for K in 1..NumH_New loop
                           Wiknew:=Wiknew+Ceiling
                             ((Wik+H_New(K).Ji)/H_New(K).Ti)*H_New(K).Ci;
                        end loop;
                        exit when Wik=Wiknew;
                        Wik:=Wiknew;
                     end loop;
                     Rij:=Wik-Time(Job-1)*Ti;
                     if Rij > Transaction(I).The_Task(J).Rij and then
                       Prio_Level=Transaction(I).The_Task(J).Prioij
                     then
                        Transaction(I).The_Task(J).Rij:=Rij;
                     end if;
                     H_Old:=H_New;
                     NumH_Old:=NumH_New;
                  end loop;
               end loop;
            end if;
         end if;
         -- Adjust the worst-case response times obtained
         if not (Unbounded_Time or Analysis_Was_Aborted) then
            for Tsk in 1..Transaction(I).Ni loop
               if Transaction(I).The_Task(Tsk).Rij>0.0 then
                  Transaction(I).The_Task(Tsk).Rij:=
                    Transaction(I).The_Task(Tsk).Rij+
                    Transaction(I).The_Task(1).Jinit;
                  Transaction(I).The_Task(Tsk).Jij:=
                    Transaction(I).The_Task(Tsk).Rij-
                    Transaction(I).The_Task(Tsk).Rbij;
               else
                  Transaction(I).The_Task(Tsk).Rbij:=0.0;
                  Transaction(I).The_Task(Tsk).Jij:=0.0;
               end if;
            end loop;
         end if;
      end loop;
      -- Put the results in place
      Translate_Linear_Analysis_Results(Transaction,The_System);
   end Varying_Priorities_Analysis;


   --------------------------------------------------------------
   -- We add the implementation of the package Mast-EDF_Tools: --
   -- the EDF analysis and deadline assignment procedures      --
   --------------------------------------------------------------

   ------------
   -- Min0 --
   ------------

   function Min0(X,Y:Time) return Time is
   begin
      return Time'Max(Time'Min(X,Y),0.0);
   end Min0;

   -------------------------
   -- Deadline_Assignment --
   -------------------------

   procedure Deadline_Assignment
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True)
   is
      Already_Assigned : Scheduling_Servers.Lists.List;

      procedure Assign_Deadline
        (Trans_Ref : Mast.Transactions.Transaction_Ref;
         The_Event_Handler_Ref : Mast.Graphs.Event_Handler_Ref)
      is
         pragma Unreferenced (Trans_Ref);
         A_Link_Ref : Mast.Graphs.Link_Ref;
         T_Req_Ref : Timing_Requirements.Timing_Requirement_Ref;
         The_Deadline : Time;
         Srvr_Ref : Scheduling_Servers.Scheduling_Server_Ref;
         S_Params_Ref : Scheduling_Parameters.Sched_Parameters_Ref;

      begin
         if The_Event_Handler_Ref.all in Graphs.Event_Handlers.Activity'Class
         then
            A_Link_Ref:=Graphs.Event_Handlers.Output_Link
              (Graphs.Event_Handlers.Activity'Class
               (The_Event_Handler_Ref.all));
            if Graphs.Has_Timing_Requirements(A_Link_Ref.all) then
               T_Req_Ref:=Graphs.Links.Link_Timing_Requirements
                 (Graphs.Links.Regular_Link'Class(A_Link_Ref.all));
               if T_Req_Ref.all in Timing_Requirements.Deadline'Class then
                  The_Deadline:=Timing_Requirements.The_Deadline
                    (Timing_Requirements.Deadline'Class(T_Req_Ref.all));
                  Srvr_Ref:=Graphs.Event_Handlers.Activity_Server
                    (Graphs.Event_Handlers.Activity'Class
                     (The_Event_Handler_Ref.all));
                  S_Params_Ref:=Scheduling_Servers.Server_Sched_Parameters
                    (Srvr_Ref.all);
                  if S_Params_Ref/=null and then
                    S_Params_Ref.all in
                    Scheduling_Parameters.EDF_Parameters'Class
                    and then
                    not Scheduling_Parameters.Preassigned
                    (Scheduling_Parameters.EDF_Parameters'Class
                     (S_Params_Ref.all))
                  then
                     if Scheduling_Servers.Lists.Find
                       (Scheduling_Servers.Name(Srvr_Ref),Already_Assigned)=
                       Scheduling_Servers.Lists.Null_Index
                     then
                        Scheduling_Parameters.Set_Deadline
                          (Scheduling_Parameters.EDF_Parameters'Class
                           (S_Params_Ref.all),The_Deadline);
                        Scheduling_Servers.Lists.Add
                          (Srvr_Ref,Already_Assigned);
                     else
                        Scheduling_Parameters.Set_Deadline
                          (Scheduling_Parameters.EDF_Parameters'Class
                           (S_Params_Ref.all),Time'Min
                           (The_Deadline,Scheduling_Parameters.Deadline
                            (Scheduling_Parameters.EDF_Parameters'Class
                             (S_Params_Ref.all))));
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end Assign_Deadline;

      procedure Null_Operation_For_Links
        (Trans_Ref : Mast.Transactions.Transaction_Ref;
         The_Link_Ref : Mast.Graphs.Link_Ref) is
      begin
         null;
      end Null_Operation_For_Links;

      procedure Iterate_Transaction_Paths is new
        Mast.Transaction_Operations.Traverse_Paths_From_Link_Once
        (Operation_For_Links  => Null_Operation_For_Links,
         Operation_For_Event_Handlers => Assign_Deadline);

      Trans_Ref : Mast.Transactions.Transaction_Ref;
      Iterator : Transactions.Lists.Index;
      Link_Iterator : Transactions.Link_Iteration_Object;
      A_Link_Ref : Mast.Graphs.Link_Ref;

      type Processor_ID is new Natural;
      type Transaction_ID is new Natural;
      type Task_ID is new Natural;
      Max_Processors:constant Processor_ID:=Processor_ID
        (Processing_Resources.Lists.Size
         (The_System.Processing_Resources));
      Max_Transactions:constant Transaction_ID:=
        Transaction_ID((Mast.Max_Numbers.Calculate_Max_Transactions
                        (The_System)));
      Max_Tasks_Per_Transaction:constant Task_ID:=Task_ID
        (Mast.Max_Numbers.Calculate_Max_Tasks_Per_Transaction(The_System));

      subtype Processor_ID_Type is Processor_ID
        range 0..Max_Processors;
      subtype Transaction_ID_Type is Transaction_ID
        range 0..Max_Transactions;
      subtype Task_ID_Type is Task_ID
        range 0..Max_Tasks_Per_Transaction;

      package Translation is new Linear_Translation
        (Processor_ID_Type, Transaction_ID_Type, Task_ID_Type,
         Max_Processors, Max_Transactions, Max_Tasks_Per_Transaction);

      use Translation;

      package Ordered_Queue is new Priority_Queues
        (Element  => Synchronization_Parameters.Synch_Parameters_Ref,
         Priority => Time,
           ">"      => "<", -- reverse order
           "="      => "=");

      type P_Level_List is array(Integer range <>) of
        Synchronization_Parameters.Synch_Parameters_Ref;
      Transaction : Linear_Transaction_System;
      PL_Order : Time;
      Current_Level, New_Level : Preemption_Level;
      Pl_Counter : Natural:=0;
      Ordered_List : Ordered_Queue.Queue;
      Synch_P_Ref : Synchronization_Parameters.Synch_Parameters_Ref;
      Srvr_Ref : Scheduling_Servers.Scheduling_Server_Ref;
      Srvr_Iterator : Scheduling_Servers.Lists.Iteration_Object;

   begin

      -- Assign timing requirements to scheduling deadlines
      -- it does so to each scheduling server referenced by an activity that
      -- verifies:
      --    - it has a timing requirement at its output event
      --    - it has an EDF scheduling server whose deadline is not preassigned
      -- If several timing requirements exist, the shortest deadline is chosen
      -- loop for all paths starting from input Links
      Mast.Transactions.Lists.Rewind(The_System.Transactions,Iterator);
      for I in 1..Mast.Transactions.Lists.Size(The_System.Transactions) loop
         Mast.Transactions.Lists.Get_Next_Item
           (Trans_Ref,The_System.Transactions,Iterator);
         Mast.Transactions.Rewind_External_Event_Links
           (Trans_Ref.all,Link_Iterator);
         for I in 1..Mast.Transactions.Num_Of_External_Event_Links
           (Trans_Ref.all)
         loop
            Mast.Transactions.Get_Next_External_Event_Link
              (Trans_Ref.all,A_Link_Ref,Link_Iterator);
            Iterate_Transaction_Paths (Trans_Ref,A_Link_Ref);
         end loop;
      end loop;

      -- Create synchronization objects for EDF scheduling servers

      Scheduling_Servers.Lists.Rewind
        (The_System.Scheduling_Servers,Srvr_Iterator);
      for I in 1..Scheduling_Servers.Lists.Size(The_System.Scheduling_Servers)
      loop
         Scheduling_Servers.Lists.Get_Next_Item
           (Srvr_Ref,The_System.Scheduling_Servers,Srvr_Iterator);
         if Scheduling_Servers.Server_Sched_Parameters(Srvr_Ref.all).all
           in Scheduling_Parameters.EDF_Parameters'Class and then
           Scheduling_Servers.Server_Synch_Parameters(Srvr_Ref.all)=null
         then
            Synch_P_Ref:=new Synchronization_Parameters.SRP_Parameters;
            Scheduling_Servers.Set_Server_Synch_Parameters
              (Srvr_Ref.all,Synch_P_Ref);
         end if;
      end loop;

      -- Calculate preemption levels for tasks, according to
      --   scheduling deadlines and jitter terms

      Translate_Linear_System(The_System,Transaction,Verbose);
      -- Loop for each transaction, I, under analysis
      for I in 1..Max_Transactions loop
         exit when Transaction(I).Ni=0;
         -- Loop for each task, tsk, in the transaction
         for Tsk in 1..Transaction(I).Ni loop
            if Transaction(I).The_Task(Tsk).Pav.Synch_P_Ref/=null and then
              Transaction(I).The_Task(Tsk).Pav.Synch_P_Ref.all in
              Synchronization_Parameters.SRP_Parameters'Class
            then
               PL_Order:=1.0/(Transaction(I).The_Task(Tsk).SDij-
                              Transaction(I).The_Task(Tsk).Jinit);
                  -- Add the parameters object and its PL to a priority queue
               Ordered_Queue.Enqueue
                 (Transaction(I).The_Task(Tsk).Pav.Synch_P_Ref,PL_Order,
                  Ordered_List);
               Pl_Counter:=Pl_Counter+1;
            end if;
         end loop;
      end loop;
      -- Obtain the preemption levels out of the priority queue
      declare
         Pl_List : P_Level_List(1..Pl_Counter);
      begin
         for I in 1..Pl_Counter loop
            Ordered_Queue.Dequeue(Synch_P_Ref,PL_Order,Ordered_List);
            Pl_List(I):=Synch_P_Ref;
         end loop;

         -- Assign levels and resolve preassigned levels
         Current_Level:=0;
         for I in 1..Pl_Counter loop
            if Synchronization_Parameters.Preassigned
              (Synchronization_Parameters.SRP_Parameters'Class
               (Pl_List(I).all))
            then
               New_Level:=Synchronization_Parameters.The_Preemption_Level
                 (Synchronization_Parameters.SRP_Parameters'Class
                  (Pl_List(I).all));
               if New_Level<Current_Level then
                  for J in reverse 1..I-1 loop
                     exit when
                       Synchronization_Parameters.The_Preemption_Level
                       (Synchronization_Parameters.SRP_Parameters'Class
                        (Pl_List(J).all))<=New_Level;
                     if Synchronization_Parameters.Preassigned
                       (Synchronization_Parameters.SRP_Parameters'Class
                        (Pl_List(J).all))
                     then
                        if Verbose then
                           Put_Line
                             ("Preassigned task preemption levels "&
                              "out of order");
                        end if;
                        Tool_Exceptions.Set_Tool_Failure_Message
                          ("Preassigned task preemption levels out of order");
                        raise Tool_Exceptions.Tool_Failure;
                     end if;
                     Synchronization_Parameters.Set_Preemption_Level
                       (Synchronization_Parameters.SRP_Parameters'Class
                        (Pl_List(J).all),New_Level);
                  end loop;
               else
                  Current_Level:=New_Level;
               end if;
            else
               Current_Level:=Current_Level+1;
               Synchronization_Parameters.Set_Preemption_Level
                 (Synchronization_Parameters.SRP_Parameters'Class
                  (Pl_List(I).all),Current_Level);
            end if;
         end loop;
      end;

   end Deadline_Assignment;

   --------------------------------
   -- EDF_Monoprocessor_Analysis --
   --------------------------------

   procedure EDF_Monoprocessor_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last)
   is
   begin
      -- May be implemented as a special case in the future,
      -- to optimize execution time
      EDF_Within_Priorities_Analysis
        (The_System,Verbose, Stop_Factor_When_Not_Schedulable);
   end EDF_Monoprocessor_Analysis;

   ------------------------------------
   -- EDF_Within_Priorities_Analysis --
   ------------------------------------

   procedure EDF_Within_Priorities_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last)
   is
      type Processor_ID is new Natural;
      type Transaction_ID is new Natural;
      type Task_ID is new Natural;
      Max_Processors:constant Processor_ID:=Processor_ID
        (Processing_Resources.Lists.Size
         (The_System.Processing_Resources));
      Max_Transactions:constant Transaction_ID:=
        Transaction_ID((Mast.Max_Numbers.Calculate_Max_Transactions
                        (The_System)));
      Max_Tasks_Per_Transaction:constant Task_ID:=Task_ID
        (Mast.Max_Numbers.Calculate_Max_Tasks_Per_Transaction(The_System));

      subtype Processor_ID_Type is Processor_ID
        range 0..Max_Processors;
      subtype Transaction_ID_Type is Transaction_ID
        range 0..Max_Transactions;
      subtype Task_ID_Type is Task_ID
        range 0..Max_Tasks_Per_Transaction;

      package Translation is new Linear_Translation
        (Processor_ID_Type, Transaction_ID_Type, Task_ID_Type,
         Max_Processors, Max_Transactions, Max_Tasks_Per_Transaction);

      use Translation;

      Transaction : Linear_Transaction_System;
      Ci, Bi,Ri,Rbi, Rik,Wik, Wiknew, Ti, Ji : Time;
      Di : Time;       --scheduling deadline (relative)
      Di_Treq : Time;       -- deadline (timing requirement)
      Si : Sched_Type; -- kind of scheduler (FP or EDF)
      L : Time;        -- length of busy period
      A, Da, E : Time;    -- phase of EDF task, and associated deadline
      Qi, Qf : Integer;
      Qti, Qtf : Time;
      P : Transaction_ID;
      Ni : Task_ID_Type;
      Jeffect : constant array(Boolean) of Time:=(False=> 1.0, True=> 0.0);
      Unbounded_Time : Boolean;
      Analysis_Was_Aborted : Boolean;

   begin
      Translate_Linear_System(The_System,Transaction,Verbose);
      Translation.Clear_Time_Results(Transaction,The_System);
      --show_linear_translation(Transaction);

      -- Loop for each transaction, I, under analysis
      for I in 1..Max_Transactions loop
         exit when Transaction(I).Ni=0;
         -- Calculate Rbi,Ci, Ji, Bi: only last task in the transaction is
         -- analyzed; other tasks in same transaction are considered
         -- independent
         Ni:=Transaction(I).Ni;
         Ci:=Transaction(I).The_Task(Ni).Cijown;
         Ti:=Transaction(I).The_Task(Ni).Tijown;
         Ji:=Transaction(I).The_Task(Ni).Jinit;
         Di:=Transaction(I).The_Task(Ni).SDij;
         Di_Treq:=Transaction(I).The_Task(Ni).Dij;
         Si:=Transaction(I).The_Task(Ni).Schedij;
         Bi:=Transaction(I).The_Task(Ni).Bij;
         Rbi:=Transaction(I).The_Task(Ni).Cbijown;
         if Si=FP then
            -- Calculate initial value for Wik
            Unbounded_Time:=False;
            Analysis_Was_Aborted:=False;
            Wik:=Bi+Ci;
            Ri:=0.0;
            for J in 1..Max_Transactions loop
               exit when Transaction(J).Ni=0;
               for Tsk in 1..Transaction(J).Ni loop
                  if  Transaction(J).The_Task(Tsk).Prioij>=
                    Transaction(I).The_Task(Ni).Prioij and then
                    (J/=I or else Tsk/=Ni)
                  then
                     Wik:=Wik+Transaction(J).The_Task(Tsk).Cij;
                  end if;
               end loop;
            end loop;
            Transaction(I).The_Task(Ni).Rbij:=Rbi;
            -- Iterate over the jobs, P, in the busy period
            P:=1;
            loop
               -- Iterate until equation converges
               loop
                  Wiknew:=Bi+Time(P)*Ci;
                  -- add contributions of high priority tasks
                  for J in 1..Max_Transactions loop
                     exit when Transaction(J).Ni=0;
                     for Tsk in 1..Transaction(J).Ni loop
                        if  Transaction(J).The_Task(Tsk).Prioij>=
                          Transaction(I).The_Task(Ni).Prioij and then
                          (J/=I or else Tsk/=Ni)
                        then
                           if Transaction(J).The_Task(Tsk).Model=
                             Unbounded_Effects
                           then
                              Wiknew:= Large_Time;
                              Wik:=Large_Time;
                              Unbounded_Time:=True;
                              exit;
                           else
                              Wiknew:=Wiknew+Ceiling
                                ((Wik+Transaction(J).The_Task(Tsk).Jinit*
                                  Jeffect(Transaction(J).The_Task(Tsk).
                                          Jitter_Avoidance))/
                                 Transaction(J).The_Task(Tsk).Tij)*
                                Transaction(J).The_Task(Tsk).Cij;
                           end if;
                        end if;
                     end loop;
                     exit when Unbounded_Time;
                  end loop;
                  exit when Unbounded_Time or else Wik=Wiknew;
                  Wik:=Wiknew;
                  if (Stop_Factor_When_Not_Schedulable/=Positive'Last) and then
                    Wik-Ti*(Time(P)-1.0)+Ji>
                    Time(Stop_Factor_When_Not_Schedulable)*Di_Treq
                  then
                     Analysis_Was_Aborted:=True;
                     exit;
                  end if;
               end loop;
               exit when Unbounded_Time;
               Rik:=Wik-Ti*(Time(P)-1.0);
               -- keep the worst case result
               if Rik>Ri then
                  Ri:=Rik;
               end if;
               -- check if busy period is too long
               if Wik>Analysis_Bound then
                  Unbounded_Time:=True;
                  exit;
               end if;
               -- determine if busy period is over
          -- busy period is over when the current response is 
          -- before the end of the period
               exit when Rik<=Ti;
               P:=P+1;
               Wik:=Wik+Ci;
            end loop;
            -- Store the worst-case response time obtained
            if Unbounded_Time or Analysis_Was_Aborted then
               Transaction(I).The_Task(Ni).Rij:=Large_Time;
               Transaction(I).The_Task(Ni).Jij:=Large_Time;
            else
               Transaction(I).The_Task(Ni).Rij:=Ri+Ji;
               Transaction(I).The_Task(Ni).Jij:=Ri+Ji-Rbi;
            end if;
         else -- EDF task under analysis
            -- Calculate length of busy period
            Unbounded_Time:=False;
            Analysis_Was_Aborted:=False;
            Wik:=Bi;
            L:=0.0;
       -- We start with the sum of the blocking time plus 
       -- the execution times
            for J in 1..Max_Transactions loop
               exit when Transaction(J).Ni=0;
               for Tsk in 1..Transaction(J).Ni loop
                  if  Transaction(J).The_Task(Tsk).Prioij>=
                    Transaction(I).The_Task(Ni).Prioij
                  then
                     Wik:=Wik+Transaction(J).The_Task(Tsk).Cij;
                  end if;
               end loop;
            end loop;
            -- Iterate until equation converges, to continue to calculate L
            loop
               Wiknew:=Bi;
               -- add contributions of high priority tasks
               for J in 1..Max_Transactions loop
                  exit when Transaction(J).Ni=0;
                  for Tsk in 1..Transaction(J).Ni loop
                     if  Transaction(J).The_Task(Tsk).Prioij>=
                       Transaction(I).The_Task(Ni).Prioij
                     then
                        if Transaction(J).The_Task(Tsk).Model=
                          Unbounded_Effects
                        then
                           Wiknew:= Large_Time;
                           Wik:=Large_Time;
                           Unbounded_Time:=True;
                           exit;
                        else
                           Wiknew:=Wiknew+Ceiling
                             ((Wik+Transaction(J).The_Task(Tsk).Jinit*
                               Jeffect(Transaction(J).The_Task(Tsk).
                                       Jitter_Avoidance))/
                              Transaction(J).The_Task(Tsk).Tij)*
                             Transaction(J).The_Task(Tsk).Cij;
                        end if;
                     end if;
                  end loop;
               end loop;
               exit when Unbounded_Time or else Wik=Wiknew;
               Wik:=Wiknew;
            end loop;
            L:=Wik;
            -- check if busy period is too long
            if L>Analysis_Bound then
               Unbounded_Time:=True;
            end if;

            if not (Unbounded_Time or Analysis_Was_Aborted) then
               -- Calculate worst-case response times
               Unbounded_Time:=False;
               Ri:=0.0;
               Transaction(I).The_Task(Ni).Rbij:=Rbi;
               -- Iterate over the jobs, P, in the busy period
               for P in 1..Integer(Ceiling((L+Ji)/Ti))
               loop
                  -- Iterate for all possible coincident deadlines
                  for K in 1..Max_Transactions loop
                     exit when Transaction(K).Ni=0;
                     for KTsk in 1..Transaction(K).Ni loop
                        if  Transaction(K).The_Task(KTsk).Prioij=
                          Transaction(I).The_Task(Ni).Prioij
                        then
            -- Obtain Qi and Qf, the limits of the job numbers
            -- to consider for the task with coincident
            -- deadline: transaction(K).task(KTsk)
            -- This obtains the applicable elements in Phi-star
                           E:=Time(P-1)*Ti-Ji+Di;
                           Qti:=Ceiling
                             ((E+Transaction(K).The_Task(KTsk).Jinit-
                               Transaction(K).The_Task(KTsk).SDij)/
                              Transaction(K).The_Task(KTsk).Tijown)+1.0;
                           if Qti>=Time(Integer'Last) then
                              Qi:=Integer'Last;
                           elsif Qti<=Time(Integer'First) then
                              Qi:=Integer'First;
                           else
                              Qi:=Integer'Max(Integer(Qti),1);
                           end if;
                           Qtf:=Ceiling
                             ((E+Ti+Transaction(K).The_Task(KTsk).Jinit-
                               Transaction(K).The_Task(KTsk).SDij)/
                              Transaction(K).The_Task(KTsk).Tijown);
                           if Qtf>=Time(Integer'Last) then
                              Qf:=Integer'Last;
                           elsif Qtf<=Time(Integer'First) then
                              Qf:=Integer'First;
                           else
                              Qf:=Integer
                                (Time'Min
                                 (Qtf,Ceiling
                                  ((L+Transaction(K).The_Task(KTsk).Jinit)/
                                   Transaction(K).The_Task(KTsk).Tijown)));
                           end if;
            -- Loop for all applicable jobs (Q) of the 
            -- task with coincident deadlines
                           for Q in Qi..Qf loop
               --Changed: Now using p*Ci instead of just Ci
               Wik:=Bi+Time(P)*Ci;
               --Da is phi_x in paper
                              Da:=(Time(Q)-1.0)*
                                Transaction(K).The_Task(KTsk).Tijown-
                                Transaction(K).The_Task(KTsk).Jinit+
                                Transaction(K).The_Task(KTsk).SDij;
               A:=Da-(Time(P-1)*Ti-Ji+Di);
                              -- Iterate until equation converges
                              loop
                                 Wiknew:=Bi+Time(P)*Ci;
                                 -- add contributions of high or equal
                                 -- priority tasks
                                 for J in 1..Max_Transactions loop
                                    exit when Transaction(J).Ni=0;
                                    for Tsk in 1..Transaction(J).Ni loop
                                       -- Higher priority (FP) tasks
                                       if  Transaction(J).The_Task(Tsk).Prioij>
                                         Transaction(I).The_Task(Ni).Prioij
                                         and then
                                         (J/=I or else Tsk/=Ni)
                                       then
                                          if Transaction(J).The_Task(Tsk).Model
                                            =Unbounded_Effects
                                          then
                                             Wiknew:= Large_Time;
                                             Wik:=Large_Time;
                                             Unbounded_Time:=True;
                                             exit;
                                          else
                                             Wiknew:=Wiknew+Ceiling
                                               ((Wik+Transaction(J).
                                                 The_Task(Tsk).Jinit*
                                                 Jeffect(Transaction(J).
                                                         The_Task(Tsk).
                                                         Jitter_Avoidance))/
                                                Transaction(J).
                                                The_Task(Tsk).Tij)*
                                               Transaction(J).
                                               The_Task(Tsk).Cij;
                                          end if;
                                       elsif Transaction(J).
                                         The_Task(Tsk).Prioij=
                                         Transaction(I).The_Task(Ni).Prioij
                                         and then
                                         (J/=I or else Tsk/=Ni)
                                       then
                                          -- Equal priority (EDF) tasks
                                          if Transaction(J).
                                            The_Task(Tsk).Model=
                                            Unbounded_Effects
                                          then
                                             Wiknew:= Large_Time;
                                             Wik:=Large_Time;
                                             Unbounded_Time:=True;
                                             exit;
                                          else
                                             Wiknew:=Wiknew+Min0
                      -- Changed: Now using Wiknew
                      -- instead of wik
                      -- mgh: revert change
                                               (Ceiling
                                                ((Wik+Transaction(J).
                                                  The_Task(Tsk).Jinit*
                                                  Jeffect(Transaction(J).
                                                          The_Task(Tsk).
                                                          Jitter_Avoidance))/
                                                 Transaction(J).
                                                 The_Task(Tsk).Tij),
                                                Floor
                                                ((Transaction(J).
                                                  The_Task(Tsk).Jinit*
                                                  Jeffect(Transaction(J).
                       The_Task(Tsk).
                                                          Jitter_Avoidance)+Da-
                      Transaction(J).
                      -- changed: Old value was
                      -- Dij instead of SDij
                                                  The_Task(Tsk).SDij)/
                                                 Transaction(J).
                                                 The_Task(Tsk).Tij)+1.0)*
                                               Transaction(J).
                                               The_Task(Tsk).Cij;
                                          end if;
                                       end if;
                                    end loop;
                                    exit when Unbounded_Time;
                                 end loop;
                                 exit when Unbounded_Time or else Wik=Wiknew;
                                 Wik:=Wiknew;
                                 if Stop_Factor_When_Not_Schedulable/=
                                       Positive'Last
                                 then
                                    exit when
                  Wik-A+Ji-Ti*(Time(P)-1.0)>
                                       Time(Stop_Factor_When_Not_Schedulable)*
                  Di_Treq;
                                 end if;
                              end loop; -- workload convergence
                              exit when Unbounded_Time;
               Rik:=Wik-A+Ji-Ti*(Time(P)-1.0);
               
                              -- keep the worst case result
                              if Rik>Ri then
                                 Ri:=Rik;
                              end if;
                              -- check if busy period is too long
                              if Wik>Analysis_Bound then
                                 Unbounded_Time:=True;
                                 exit;
                              end if;
                              Wik:=Wik+Ci;
                           end loop; -- Coincident deadline q
                           exit when Unbounded_Time;
                        end if; -- Pi=Pk
                     end loop; -- task Ktsk
                     exit when Unbounded_Time;
                  end loop; -- transaction k
                  exit when Unbounded_Time;
               end loop; -- job p
            end if;
            -- Store the worst-case response time obtained
            if Unbounded_Time or Analysis_Was_Aborted then
               Transaction(I).The_Task(Ni).Rij:=Large_Time;
               Transaction(I).The_Task(Ni).Jij:=Large_Time;
            else
               -- Changed: We should not add the jitter again
               Transaction(I).The_Task(Ni).Rij:=Ri;
               Transaction(I).The_Task(Ni).Jij:=Ri-Rbi;
            end if;
         end if;
      end loop;
      Translate_Linear_Analysis_Results(Transaction,The_System);
   end EDF_Within_Priorities_Analysis;

end Mast.Monoprocessor_Tools;
