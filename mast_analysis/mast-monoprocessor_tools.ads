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

with Mast.Systems,Mast.Tools;
package Mast.Monoprocessor_Tools is

   procedure RM_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last);

   procedure Varying_Priorities_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last);

   procedure Priority_Assignment
     (The_System : in out Mast.Systems.System;
      The_Tool: Mast.Tools.Worst_Case_Analysis_Tool;
      Verbose : Boolean:=True);

   procedure EDF_Monoprocessor_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last);

   procedure EDF_Within_Priorities_Analysis
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True;
      Stop_Factor_When_Not_Schedulable : Positive:=Positive'Last);

   procedure Deadline_Assignment
     (The_System : in out Mast.Systems.System;
      Verbose : Boolean:=True);

end Mast.Monoprocessor_Tools;


