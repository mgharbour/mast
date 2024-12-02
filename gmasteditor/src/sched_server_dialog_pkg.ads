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
with Gtk.Dialog;    use Gtk.Dialog;
with Gtk.Box;       use Gtk.Box;
with Gtk.Button;    use Gtk.Button;
with Gtk.Table;     use Gtk.Table;
with Gtk.Label;     use Gtk.Label;
with Gtk.GEntry;    use Gtk.GEntry;
with Glib.Unicode;  use Glib.Unicode;
with Gtk.Combo_Box_Text;     use Gtk.Combo_Box_Text;
with Gtk.Window;    use Gtk.Window;

package Sched_Server_Dialog_Pkg is

   type Sched_Server_Dialog_Record is new Gtk_Dialog_Record with record
      Hbox66                 : Gtk_Hbox;
      Server_Ok_Button       : Gtk_Button;
      Server_Cancel_Button   : Gtk_Button;
      Vbox5                  : Gtk_Vbox;
      Table1                 : Gtk_Table;
      Label109               : Gtk_Label;
      Label110               : Gtk_Label;
      Label111               : Gtk_Label;
      Label112               : Gtk_Label;
      Label113               : Gtk_Label;
      Server_Name_Entry      : Gtk_Entry;
      Server_Type_Combo      : Gtk_Combo_Box_Text;
      Sched_Combo            : Gtk_Combo_Box_Text;
      Sync_Type_Combo        : Gtk_Combo_Box_Text;
      Srp_Table              : Gtk_Table;
      Label114               : Gtk_Label;
      Label115               : Gtk_Label;
      Preemption_Level_Entry : Gtk_Entry;
      Pre_Level_Combo        : Gtk_Combo_Box_Text;
      Policy_Type_Table      : Gtk_Table;
      Label375               : Gtk_Label;
      Label376               : Gtk_Label;
      Policy_Type_Combo      : Gtk_Combo_Box_Text;
      Priority_Table         : Gtk_Table;
      Label377               : Gtk_Label;
      Label378               : Gtk_Label;
      Server_Priority_Entry  : Gtk_Entry;
      Pre_Prior_Combo        : Gtk_Combo_Box_Text;
      Polling_Table          : Gtk_Table;
      Label116               : Gtk_Label;
      Polling_Period_Entry   : Gtk_Entry;
      Polling_Wor_Over_Entry : Gtk_Entry;
      Polling_Bes_Over_Entry : Gtk_Entry;
      Polling_Avg_Over_Entry : Gtk_Entry;
      Label117               : Gtk_Label;
      Label118               : Gtk_Label;
      Label119               : Gtk_Label;
      Sporadic_Table         : Gtk_Table;
      Label120               : Gtk_Label;
      Back_Prior_Entry       : Gtk_Entry;
      Init_Capa_Entry        : Gtk_Entry;
      Reple_Period_Entry     : Gtk_Entry;
      Max_Pend_Reple_Entry   : Gtk_Entry;
      Label122               : Gtk_Label;
      Label123               : Gtk_Label;
      Label121               : Gtk_Label;
      Edf_Table              : Gtk_Table;
      Label369               : Gtk_Label;
      Label370               : Gtk_Label;
      Deadline_Entry         : Gtk_Entry;
      Deadline_Combo         : Gtk_Combo_Box_Text;
   end record;
   type Sched_Server_Dialog_Access is access all Sched_Server_Dialog_Record'
     Class;

   procedure Gtk_New (Sched_Server_Dialog : out Sched_Server_Dialog_Access);
   procedure Initialize
     (Sched_Server_Dialog : access Sched_Server_Dialog_Record'Class);

   procedure Gtk_New 
     (Sched_Server_Dialog : out Sched_Server_Dialog_Access; 
      Parent : Gtk.Window.Gtk_Window);
   procedure Initialize
     (Sched_Server_Dialog : access Sched_Server_Dialog_Record'Class; 
      Parent : Gtk.Window.Gtk_Window);

end Sched_Server_Dialog_Pkg;
