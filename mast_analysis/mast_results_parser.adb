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
with Mast, Mast.Operations, Mast.Shared_Resources, 
  Mast.Processing_Resources, Mast.Processing_Resources.Processor,
  Mast.Scheduling_Servers, Mast.IO,
  Mast.Transactions, 
  Mast.Systems, 
  Mast.Scheduling_Parameters, Mast.Synchronization_Parameters,
  Mast.Results,
  Mast.Events, Mast.Graphs, Mast.Graphs.Links,
  List_Exceptions;
with Symbol_Table; 
with MAST_Results_Lex; use MAST_Results_Lex;
with Var_Strings; use Var_Strings;
with Mast_Results_Parser_Tokens, Mast_Results_Parser_Shift_Reduce,
  Mast_Results_Parser_Goto,Mast_Results_Parser_Error_Report,
  mast_results_lex_io, mast_results_lex_dfa;
with Ada.Text_IO;
with Ada; use Ada;
use Mast_Results_Parser_Tokens, Mast_Results_Parser_Shift_Reduce,
  Mast_Results_Parser_Goto,mast_results_lex_io;
use Mast;
procedure MAST_Results_Parser (MAST_System : in out Mast.Systems.System) is

      procedure YYError (S : String) is  
      begin
      Mast_Results_Parser_Error_Report.Report_Continuable_Error
                 (Yy_Line_Number, Yy_Begin_Column, Yy_End_Column, S, True);
      end YYError;

      PR_Ref                : Processing_Resources.Processing_Resource_Ref;
      SR_Ref                : Shared_Resources.Shared_Resource_Ref; 
      Op_Ref                : Operations.Operation_Ref;
      SS_Ref                : Scheduling_Servers.Scheduling_Server_Ref;
      Sched_Params_Ref      : Mast.Scheduling_Parameters.Sched_Parameters_Ref;
      Synch_Params_Ref      : 
            Mast.Synchronization_Parameters.Synch_Parameters_Ref;
      Tr_Ref                : Transactions.Transaction_Ref;
      The_Event_Ref         : Mast.Events.Event_Ref;
      A_Deadline            : Time;
 
      The_trace_Res : Mast.Results.Trace_Result_Ref;
      The_Slack_Res : Mast.Results.Slack_Result_Ref;
      The_Utilization_res : Mast.Results.Utilization_Result_Ref;
      The_Queue_Size_Res : Mast.Results.Queue_Size_Result_Ref;
      The_Ceiling_Res : Mast.Results.Ceiling_Result_Ref;
      The_Preemption_Level_Res : Mast.Results.Preemption_Level_Result_Ref;
      The_SP_Res : Mast.Results.Sched_Params_Result_Ref;
      The_SynchP_Res : Mast.Results.Synch_Params_Result_Ref;
      The_Timing_Res : Mast.Results.Timing_Result_Ref;
     
      preassigned_field_present : Boolean:=False;
   pragma Warnings (Off, "use clause for private type * defined at named_lists.ads:39, instance at mast-operations.ads:135 has no effect", Reason => "TBD");

      use type Operations.Lists.Index;
   pragma Warnings (On, "use clause for private type * defined at named_lists.ads:39, instance at mast-operations.ads:135 has no effect");
   pragma Warnings (Off, "use clause for private type * defined at named_lists.ads:39, instance at mast-processing_resources.ads:130 has no effect", Reason => "TBD");
      use type Processing_Resources.Lists.Index;
   pragma Warnings (On, "use clause for private type * defined at named_lists.ads:39, instance at mast-processing_resources.ads:130 has no effect");
   pragma Warnings (Off, "use clause for private type * defined at named_lists.ads:39, instance at mast-shared_resources.ads:121 has no effect", Reason => "TBD");
      use type Shared_Resources.Lists.Index;
   pragma Warnings (On, "use clause for private type * defined at named_lists.ads:39, instance at mast-shared_resources.ads:121 has no effect");
   pragma Warnings (Off, "use clause for private type * defined at named_lists.ads:39, instance at mast-scheduling_servers.ads:165 has no effect", Reason => "TBD");
      use type Scheduling_Servers.Lists.Index;
   pragma Warnings (On, "use clause for private type * defined at named_lists.ads:39, instance at mast-scheduling_servers.ads:165 has no effect");

procedure YYParse is

   -- Rename User Defined Packages to Internal Names.
      -- package yy_goto_tables         renames
      -- Mast_Results_Parser_Goto;
      package yy_shift_reduce_tables renames
      Mast_Results_Parser_Shift_Reduce;
      pragma Unreferenced (yy_shift_reduce_tables);
      package yy_tokens              renames
      Mast_Results_Parser_Tokens;
      -- UMASS CODES :
      package yy_error_report renames
      Mast_Results_Parser_Error_Report;
      -- END OF UMASS CODES.


   procedure yyerrok;
      pragma Unreferenced (yyerrok);
   procedure yyclearin;
      pragma Unreferenced (yyclearin);

-- UMASS CODES :
--   One of the extension of ayacc. Used for
--   error recovery and error reporting.

   package yyparser_input is
   --
   -- TITLE
   --   yyparser input.
   --
   -- OVERVIEW
   --   In Ayacc, parser get the input directly from lexical scanner.
   --   In the extension, we have more power in error recovery which will
   --   try to replace, delete or insert a token into the input
   --   stream. Since general lexical scanner does not support
   --   replace, delete or insert a token, we must maintain our
   --   own input stream to support these functions. It is the
   --   purpose that we introduce yyparser_input. So parser no
   --   longer interacts with lexical scanner, instead, parser
   --   will get the input from yyparser_input. Yyparser_Input
   --   get the input from lexical scanner and supports
   --   replacing, deleting and inserting tokens.
   --

   type string_ptr is access string;

   type tokenbox is record
   --
   -- OVERVIEW
   --    Tokenbox is the type of the element of the input
   --    stream maintained in yyparser_input. It contains
   --    the value of the token, the line on which the token
   --    resides, the line number on which the token resides.
   --    It also contains the begin and end column of the token.
      token         : yy_tokens.Token;
      lval          : YYstype;
      line          : string_ptr;
      line_number   : natural := 1;
      token_start   : natural := 1;
      token_end     : natural := 1;
   end record;

   type boxed_token is access tokenbox;

   procedure unget(tok : boxed_token);
   -- push a token back into input stream.

   function get return boxed_token;
   -- get a token from input stream

   procedure reset_peek;
   function peek return boxed_token;
   -- During error recovery, we will lookahead to see the
   -- affect of the error recovery. The lookahead does not
   -- means that we actually accept the input, instead, it
   -- only means that we peek the future input. It is the
   -- purpose of function peek and it is also the difference
   -- between peek and get. We maintain a counter indicating
   -- how many token we have peeked and reset_peek will
   -- reset that counter.

   function tbox (token : yy_tokens.Token ) return boxed_token;
   -- Given the token got from the lexical scanner, tbox
   -- collect other information, such as, line, line number etc.
   -- to construct a boxed_token.

   input_token    : yyparser_input.boxed_token;
   previous_token : yyparser_input.boxed_token;
   -- The current and previous token processed by parser.

   end yyparser_input;

   package yyerror_recovery is
   --
   -- TITLE
   --
   --   Yyerror_Recovery.
   --
   -- OVERVIEW
   --   This package contains all of errro recovery staff,
   --   in addition to those of Ayacc.

   previous_action : integer;
   -- This variable is used to save the previous action the parser made.

   previous_error_flag : natural := 0;
   -- This variable is used to save the previous error flag.

   valuing : Boolean := True;
   -- Indicates whether to perform semantic actions. If exception
   -- is raised during semantic action after error recovery, we
   -- set valuing to False which causes no semantic actions to
   -- be invoked any more.

         procedure flag_token ( error : Boolean := True );
    --  This procedure will point out the position of the
    --  current token.

   procedure finale;
   -- This procedure prepares the final report for error report.

   procedure try_recovery;
   -- It is the main procedure for error recovery.

   line_number : integer := 0;
   -- Indicates the last line having been outputed to the error file.

   procedure put_new_line;
   -- This procedure outputs the whole line on which input_token
   -- resides along with line number to the file for error reporting.
   end yyerror_recovery;

   use yyerror_recovery;

   package user_defined_errors is
   --
   -- TITLE
   --    User Defined Errors.
   --
   -- OVERVIEW
   --    This package is used to facilite the error reporting.

   procedure parser_error(Message : String );
   procedure parser_warning(Message : String );
         pragma Unreferenced (parser_warning);

   end user_defined_errors;

-- END OF UMASS CODES.

   package yy is

         -- the size of the value and state stacks
         stack_size : constant Natural := 300;

         -- subtype rule         is natural;
         subtype parse_state  is natural;
         -- subtype nonterminal  is integer;

         -- encryption constants
         default           : constant := -1;
         first_shift_entry : constant :=  0;
         accept_code       : constant := -3001;
         error_code        : constant := -3000;

         -- stack data used by the parser
         tos                : natural := 0;
         value_stack        : array(0..stack_size) of yy_tokens.YYstype;
         pragma Unreferenced (value_stack);
         state_stack        : array(0..stack_size) of parse_state;

         -- current input symbol and action the parser is on
         action             : integer;
         rule_id            : Rule;
         input_symbol       : yy_tokens.Token;


         -- error recovery flag
         error_flag : natural := 0;
         -- indicates  3 - (number of valid shifts after an error occurs)

         look_ahead : boolean := true;
         index      : integer;

         -- Is Debugging option on or off
         DEBUG : constant boolean := FALSE;

      end yy;


      function goto_state
      (state : yy.parse_state;
       sym   : Nonterminal) return yy.parse_state;

      function parse_action
      (state : yy.parse_state;
       t     : yy_tokens.Token) return integer;

      pragma inline(goto_state, parse_action);


      function goto_state(state : yy.parse_state;
                        sym   : Nonterminal) return yy.parse_state is
         index : integer;
      begin
         index := GOTO_OFFSET(state);
         while  integer(Goto_Matrix(index).Nonterm) /= sym loop
            index := index + 1;
         end loop;
         return integer(Goto_Matrix(index).Newstate);
      end goto_state;


      function parse_action(state : yy.parse_state;
                          t     : yy_tokens.Token) return integer is
         index      : integer;
         tok_pos    : integer;
         default    : constant integer := -1;
      begin
         tok_pos := yy_tokens.Token'pos(t);
         index   := SHIFT_REDUCE_OFFSET(state);
         while integer(Shift_Reduce_Matrix(index).T) /= tok_pos and then
              integer(Shift_Reduce_Matrix(index).T) /= default
        loop
            index := index + 1;
         end loop;
         return integer(Shift_Reduce_Matrix(index).Act);
      end parse_action;

-- error recovery stuff

      procedure handle_error is
      temp_action : integer;
      begin

      if yy.error_flag = 3 then -- no shift yet, clobber input.
      if yy.DEBUG then
               Text_IO.Put_Line("Ayacc.YYParse: Error Recovery Clobbers " &
                   yy_tokens.Token'image(yy.input_symbol));
-- UMASS CODES :
               yy_error_report.Put_Line("Ayacc.YYParse: Error Recovery Clobbers " &
                                    yy_tokens.Token'image(yy.input_symbol));
-- END OF UMASS CODES.
      end if;
            if yy.input_symbol = yy_tokens.End_Of_Input then  -- don't discard,
               if yy.DEBUG then
            Text_IO.Put_Line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
-- UMASS CODES :
            yy_error_report.Put_Line("Ayacc.YYParse: Can't discard END_OF_INPUT, quiting...");
-- END OF UMASS CODES.
               end if;
-- UMASS CODES :
               yyerror_recovery.finale;
-- END OF UMASS CODES.
               raise yy_tokens.Syntax_Error;
            end if;

            yy.look_ahead := true;   -- get next token
            return;                  -- and try again...
         end if;

         if yy.error_flag = 0 then -- brand new error
            YYError("Syntax Error");
-- UMASS CODES :
            yy_error_report.Put_Line ( "Skipping..." );
            yy_error_report.Put_Line ( "" );
-- END OF UMASS CODES.
         end if;

         yy.error_flag := 3;

         -- find state on stack where error is a valid shift --

         if yy.DEBUG then
            Text_IO.Put_Line("Ayacc.YYParse: Looking for state with error as valid shift");
-- UMASS CODES :
            yy_error_report.Put_Line("Ayacc.YYParse: Looking for state with error as valid shift");
-- END OF UMASS CODES.
         end if;

         loop
            if yy.DEBUG then
               Text_IO.Put_Line("Ayacc.YYParse: Examining State " &
               yy.parse_state'image(yy.state_stack(yy.tos)));
-- UMASS CODES :
               yy_error_report.Put_Line("Ayacc.YYParse: Examining State " &
                                   yy.parse_state'image(yy.state_stack(yy.tos)));
-- END OF UMASS CODES.
            end if;
            temp_action := parse_action(yy.state_stack(yy.tos), Error);

            if temp_action >= yy.first_shift_entry then
               if yy.tos = yy.stack_size then
                  Text_IO.Put_Line(" Stack size exceeded on state_stack");
-- UMASS CODES :
                  yy_error_report.Put_Line(" Stack size exceeded on state_stack");
                  yyerror_recovery.finale;
-- END OF UMASS CODES.
                  raise yy_tokens.Syntax_Error;
               end if;
               yy.tos := yy.tos + 1;
               yy.state_stack(yy.tos) := temp_action;
               exit;
            end if;

            Decrement_Stack_Pointer :
        begin
               yy.tos := yy.tos - 1;
            exception
               when Constraint_Error =>
            yy.tos := 0;
            end Decrement_Stack_Pointer;

            if yy.tos = 0 then
               if yy.DEBUG then
            Text_IO.Put_Line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
-- UMASS CODES :
            yy_error_report.Put_Line("Ayacc.YYParse: Error recovery popped entire stack, aborting...");
-- END OF UMASS CODES.
               end if;
-- UMASS CODES :
               yyerror_recovery.finale;
-- END OF UMASS CODES.
               raise yy_tokens.Syntax_Error;
            end if;
         end loop;

         if yy.DEBUG then
            Text_IO.Put_Line("Ayacc.YYParse: Shifted error token in state " &
              yy.parse_state'image(yy.state_stack(yy.tos)));
-- UMASS CODES :
            yy_error_report.Put_Line("Ayacc.YYParse: Shifted error token in state " &
                                 yy.parse_state'image(yy.state_stack(yy.tos)));
-- END OF UMASS CODES.
         end if;

      end handle_error;

   -- print debugging information for a shift operation
   procedure shift_debug(state_id: yy.parse_state; lexeme: yy_tokens.Token) is
   begin
         Text_IO.Put_Line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
               yy_tokens.Token'image(lexeme) );
-- UMASS CODES :
         yy_error_report.Put_Line("Ayacc.YYParse: Shift "& yy.parse_state'image(state_id)&" on input symbol "&
                                yy_tokens.Token'image(lexeme) );
-- END OF UMASS CODES.
   end shift_debug;

   -- print debugging information for a reduce operation
   procedure reduce_debug(rule_id: Rule; state_id: yy.parse_state) is
   begin
         Text_IO.Put_Line("Ayacc.YYParse: Reduce by rule "&Rule'image(rule_id)&" goto state "&
               yy.parse_state'image(state_id));
-- UMASS CODES :
         yy_error_report.Put_Line("Ayacc.YYParse: Reduce by rule "&Rule'image(rule_id)&" goto state "&
                                yy.parse_state'image(state_id));
-- END OF UMASS CODES.
   end reduce_debug;

   -- make the parser believe that 3 valid shifts have occured.
   -- used for error recovery.
   procedure yyerrok is
   begin
         yy.error_flag := 0;
   end yyerrok;

   -- called to clear input symbol that caused an error.
   procedure yyclearin is
   begin
         -- yy.input_symbol := yylex;
         yy.look_ahead := true;
   end yyclearin;

-- UMASS CODES :
--   Bodies of yyparser_input, yyerror_recovery, user_define_errors.

package body yyparser_input is

   input_stream_size : constant integer := 10;
   -- Input_stream_size indicates how many tokens can
   -- be hold in input stream.

   input_stream : array (0..input_stream_size-1) of boxed_token;

   index : integer := 0;           -- Indicates the position of the next
   --                                 buffered token in the input stream.
   peek_count : integer := 0;      -- # of tokens seen by peeking in the input stream.
   buffered : integer := 0;        -- # of buffered tokens in the input stream.

   function tbox(token : yy_tokens.Token) return boxed_token is
            boxed : boxed_token;
            line : string ( 1 .. 1024 );
            line_length : integer;
   begin
            boxed := new tokenbox;
            boxed.token := token;
            boxed.lval := YYLVal;
            boxed.line_number := Yy_Line_Number;
            Yy_Get_Token_Line(line, line_length);
            boxed.line := new String ( 1 .. line_length );
            boxed.line ( 1 .. line_length ) := line ( 1 .. line_length );
            boxed.token_start := Yy_Begin_Column;
            boxed.token_end := Yy_End_Column;
            return boxed;
   end tbox;

   function get return boxed_token is
            t : boxed_token;
   begin
            if buffered = 0 then
       -- No token is buffered in the input stream
       -- so we get input from lexical scanner and return.
               return tbox(YYlex);
            else
       -- return the next buffered token. And remove
       -- it from input stream.
               t := input_stream(index);
               YYLVal := t.lval;
       -- Increase index and decrease buffered has
       -- the affect of removing the returned token
       -- from input stream.
               index := (index + 1 ) mod input_stream_size;
               buffered := buffered - 1;
               if peek_count > 0 then
         -- Previously we were peeking the tokens
         -- from index - 1 to index - 1 + peek_count.
         -- But now token at index - 1 is returned
         -- and remove, so this token is no longer
         -- one of the token being peek. So we must
         -- decrease the peek_count. If peek_count
         -- is 0, we remains peeking 0 token, so we
         -- do nothing.
         peek_count := peek_count - 1;
               end if;
               return t;
            end if;
   end get;

   procedure reset_peek is
   -- Make it as if we have not peeked anything.
   begin
      peek_count := 0;
   end reset_peek;

   function peek return boxed_token is
      t : boxed_token;
   begin
      if peek_count = buffered then
        -- We have peeked all the buffered tokens
        -- in the input stream, so next peeked
        -- token should be got from lexical scanner.
        -- Also we must buffer that token in the
        -- input stream. It is the difference between
        -- peek and get.
               t := tbox(YYlex);
               input_stream((index + buffered) mod input_stream_size) := t;
               buffered := buffered + 1;
               if buffered > input_stream_size then
                  Text_IO.Put_Line ( "Warning : input stream size exceed."
                     & " So token is lost in the input stream." );
               end if;

      else
        -- We have not peeked all the buffered tokens,
        -- so we peek next buffered token.

               t := input_stream((index+peek_count) mod input_stream_size);
      end if;

      peek_count := peek_count + 1;
      return t;
   end peek;

   procedure unget (tok : boxed_token) is
   begin
      -- First decrease the index.
      if index = 0 then
               index := input_stream_size - 1;
      else
               index := index - 1;
      end if;
      input_stream(index) := tok;
      buffered := buffered + 1;
      if buffered > input_stream_size then
               Text_IO.Put_Line ( "Warning : input stream size exceed."
                   & " So token is lost in the input stream." );
      end if;

      if peek_count > 0 then
        -- We are peeking tokens, so we must increase
        -- peek_count to maintain the correct peeking position.
               peek_count := peek_count + 1;
      end if;
   end unget;

   end yyparser_input;


      package body user_defined_errors is

      procedure parser_error(Message : String) is
      begin
            yy_error_report.Report_Continuable_Error
         (yyparser_input.input_token.line_number,
          yyparser_input.input_token.token_start,
          yyparser_input.input_token.token_end,
          Message,
          True);
            yy_error_report.Total_Errors := yy_error_report.Total_Errors + 1;
      end parser_error;

      procedure parser_warning(Message : String) is
      begin
            yy_error_report.Report_Continuable_Error
         (yyparser_input.input_token.line_number,
          yyparser_input.input_token.token_start,
          yyparser_input.input_token.token_end,
          Message,
          False);
            yy_error_report.Total_Warnings := yy_error_report.Total_Warnings + 1;
      end parser_warning;

      end user_defined_errors;


      package body yyerror_recovery is

         max_forward_moves : constant integer := 5;
         -- Indicates how many tokens we will peek at most during error recovery.

         type change_type is ( replace, insert, delete );
         -- Indicates what kind of change error recovery does to the input stream.

         type correction_type is record
       -- Indicates the correction error recovery does to the input stream.
       change    :   change_type;
       score     :   integer;
       tokenbox  :   yyparser_input.boxed_token;
    end record;

         procedure put_new_line is
      line_number_string : constant string :=
         integer'image( yyparser_input.input_token.line_number );
         begin
      yy_error_report.Put(line_number_string);
      for i in 1 .. 5 - integer ( line_number_string'length ) loop
               yy_error_report.Put(" ");
      end loop;
      yy_error_report.Put(yyparser_input.input_token.line.all);
         end put_new_line;


         procedure finale is
         begin
      if yy_error_report.Total_Errors > 0 then
               yy_error_report.Put_Line("");
               yy_error_report.Put("Ayacc.YYParse : " & natural'image(yy_error_report.Total_Errors));
               if yy_error_report.Total_Errors = 1 then
                  yy_error_report.Put_Line(" syntax error found.");
               else
                  yy_error_report.Put_Line(" syntax errors found.");
               end if;
               yy_error_report.Finish_Output;
               raise yy_error_report.Syntax_Error;
      elsif yy_error_report.Total_Warnings > 0 then
               yy_error_report.Put_Line("");
               yy_error_report.Put("Ayacc.YYParse : " & natural'image(yy_error_report.Total_Warnings));
               if yy_error_report.Total_Warnings = 1 then
                  yy_error_report.Put_Line(" syntax warning found.");
               else
                  yy_error_report.Put_Line(" syntax warnings found.");
               end if;

               yy_error_report.Finish_Output;
               raise yy_error_report.Syntax_Warning;
      end if;
      yy_error_report.Finish_Output;
         end finale;

         procedure flag_token ( error : Boolean := True ) is
    --
    -- OVERVIEW
    --    This procedure will point out the position of the
    --    current token.
    --
         begin
            if yy.error_flag > 0 then
         -- We have not seen 3 valid shift yet, so we
         -- do not need to report this error.
         return;
            end if;

            if error then
         yy_error_report.Put("Error"); -- 5 characters for line number.
            else
         yy_error_report.Put("OK   ");
            end if;

            for i in 1 .. yyparser_input.input_token.token_start - 1 loop
         if yyparser_input.input_token.line(i) = Ascii.ht then
                  yy_error_report.Put(Ascii.ht);
         else
                  yy_error_report.Put(" ");
         end if;
            end loop;
            yy_error_report.Put_Line("^");
         end flag_token;


         procedure print_correction_message (correction : correction_type) is
    --
    -- OVERVIEW
    --    This is a local procedure used to print out the message
    --    about the correction error recovery did.
    --
         begin
      if yy.error_flag > 0 then
        -- We have not seen 3 valid shift yet, so we
        -- do not need to report this error.
               return;
      end if;

      flag_token;
      case correction.change is
            when delete =>
               yy_error_report.Put("token delete " );
               user_defined_errors.parser_error("token delete " );

            when replace =>
               yy_error_report.Put("token replaced by " &
                     yy_tokens.Token'image(correction.tokenbox.token));
               user_defined_errors.parser_error("token replaced by " &
                     yy_tokens.Token'image(correction.tokenbox.token));

            when insert =>
               yy_error_report.Put("inserted token " &
                     yy_tokens.Token'image(correction.tokenbox.token));
               user_defined_errors.parser_error("inserted token " &
                     yy_tokens.Token'image(correction.tokenbox.token));
      end case;

      if yy.DEBUG then
               yy_error_report.Put_Line("... Correction Score is" &
                                 integer'image(correction.score));
      else
               yy_error_report.Put_Line("");
      end if;
      yy_error_report.Put_Line("");
         end print_correction_message;

         procedure install_correction(correction : correction_type) is
            -- This is a local procedure used to install the correction.
         begin
            case correction.change is
            when delete  => null;
                          -- Since error found for current token,
                          -- no state is changed for current token.
                          -- If we resume Parser now, Parser will
                          -- try to read next token which has the
                          -- affect of ignoring current token.
                          -- So for deleting correction, we need to
                          -- do nothing.
            when replace => yyparser_input.unget(correction.tokenbox);
            when insert  => yyparser_input.unget(yyparser_input.input_token);
               yyparser_input.input_token := null;
               yyparser_input.unget(correction.tokenbox);
            end case;
         end install_correction;


         function simulate_moves return integer is
    --
    -- OVERVIEW
    --    This is a local procedure simulating the Parser work to
    --    evaluate a potential correction. It will look at most
    --    max_forward_moves tokens. It behaves very similarly as
    --    the actual Parser except that it does not invoke user
    --    action and it exits when either error is found or
    --    the whole input is accepted. Simulate_moves also
    --    collects and returns the score. Simulate_Moves
    --    do the simulation on the copied state stack to
    --    avoid changing the original one.

            -- the score for each valid shift.
            shift_increment : constant integer := 20;
            -- the score for each valid reduce.
            reduce_increment : constant integer := 10;
            -- the score for accept action.
            accept_increment : constant integer := 14 * max_forward_moves;
            -- the decrement for error found.
            -- error_decrement : integer := -10 * max_forward_moves;

       -- Indicates how many reduces made between last shift
       -- and current shift.
            current_reduces : integer := 0;

            -- Indicates how many reduces made till now.
            total_reduces : integer := 0;

            -- Indicates how many tokens seen so far during simulation.
            tokens_seen : integer := 0;

            score : integer := 0; -- the score of the simulation.

            The_Copied_Stack : array (0..yy.stack_size) of yy.parse_state;
            The_Copied_Tos   : integer;
            The_Copied_Input_Token : yyparser_input.boxed_token;
            Look_Ahead : Boolean := True;

         begin

            -- First we copy the state stack.
            for i in 0 .. yy.tos loop
         The_Copied_Stack(i) := yy.state_stack(i);
            end loop;
            The_Copied_Tos := yy.tos;
            The_Copied_Input_Token := yyparser_input.input_token;
       -- Reset peek_count because each simulation
       -- starts a new process of peeking.
            yyparser_input.reset_peek;

            -- Do the simulation.
            loop
         -- We peek at most max_forward_moves tokens during simulation.
         exit when tokens_seen = max_forward_moves;

         -- The following codes is very similar the codes in Parser.
         yy.index := SHIFT_REDUCE_OFFSET(yy.state_stack(yy.tos));
         if integer(Shift_Reduce_Matrix(yy.index).T) = yy.default then
            yy.action := integer(Shift_Reduce_Matrix(yy.index).Act);
               else
            if Look_Ahead then
                     Look_Ahead   := false;
              -- Since it is in simulation, we peek the token instead of
              -- get the token.
                     The_Copied_Input_Token  := yyparser_input.peek;
            end if;
            yy.action :=
             parse_action(The_Copied_Stack(The_Copied_Tos), The_Copied_Input_Token.token);
               end if;

               if yy.action >= yy.first_shift_entry then  -- SHIFT
            if yy.DEBUG then
                     shift_debug(yy.action, The_Copied_Input_Token.token);
            end if;

            -- Enter new state
            The_Copied_Tos := The_Copied_Tos + 1;
            The_Copied_Stack(The_Copied_Tos) := yy.action;

            -- Advance lookahead
            Look_Ahead := true;

            score := score + shift_increment + current_reduces * reduce_increment;
            current_reduces := 0;
            tokens_seen := tokens_seen + 1;

               elsif yy.action = yy.error_code then       -- ERROR
            score := score - total_reduces * reduce_increment;
            exit; -- exit the loop for simulation.

               elsif yy.action = yy.accept_code then
            score := score + accept_increment;
            exit; -- exit the loop for simulation.

               else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Don't Execute User Action

            -- Pop RHS states and goto next state
            The_Copied_Tos      := The_Copied_Tos - Rule_Length(yy.rule_id) + 1;
            The_Copied_Stack(The_Copied_Tos) := goto_state(The_Copied_Stack(The_Copied_Tos-1) ,
                                 Get_LHS_Rule(yy.rule_id));

            -- Leave value stack alone

            if yy.DEBUG then
                     reduce_debug(yy.rule_id,
                    goto_state(The_Copied_Stack(The_Copied_Tos - 1),
                               Get_LHS_Rule(yy.rule_id)));
            end if;

            -- reduces only credited to score when a token can be shifted
            -- but no more than 3 reduces can count between shifts
            current_reduces := current_reduces + 1;
            total_reduces := total_reduces + 1;

               end if;

      end loop; -- loop for simulation;

      yyparser_input.reset_peek;

      return score;
   end simulate_moves;



         procedure primary_recovery ( best_correction : in out correction_type;
                                 stop_score      : integer ) is
    --
    -- OVERVIEW
    --    This is a local procedure used by try_recovery. This
    --    procedure will try the following corrections :
    --      1. Delete current token.
    --      2. Replace current token with any token acceptible
    --         from current state, or,
    --         Insert any one of the tokens acceptible from current state.
    --
      token_code      : integer;
      new_score       : integer;
      the_boxed_token : yyparser_input.boxed_token;
         begin

      -- First try to delete current token.
      if yy.DEBUG then
               yy_error_report.Put_Line("trying to delete " &
                      yy_tokens.Token'image(yyparser_input.input_token.token));
      end if;

      best_correction.change := delete;
      -- try to evaluate the correction. NOTE : simulating the Parser
      -- from current state has affect of ignoring current token
      -- because error was found for current token and no state
      -- was pushed to state stack.
      best_correction.score := simulate_moves;
      best_correction.tokenbox := null;

      -- If the score is less than stop_score, we try
      -- the 2nd kind of corrections, that is, replace or insert.
      if best_correction.score < stop_score then
               for i in SHIFT_REDUCE_OFFSET(yy.state_stack(yy.tos))..
                 (SHIFT_REDUCE_OFFSET(yy.state_stack(yy.tos)+1) - 1) loop
          -- We try to use the acceptible token from current state
          -- to replace current token or try to insert the acceptible token.
                  token_code := integer(Shift_Reduce_Matrix(i).T);
                  -- yy.default is not a valid token, we must exit.
                  exit when token_code = yy.default;

                  the_boxed_token := yyparser_input.tbox(yy_tokens.Token'val(token_code));
                  for change in replace .. insert loop
            -- We try replacing and the inserting.
            case change is
               when replace => yyparser_input.unget(the_boxed_token);
                               -- put the_boxed_token into the input stream
                               -- has the affect of replacing current token
                               -- because current token has been retrieved
                               -- but no state was change because of the error.
                        if yy.DEBUG then
                           yy_error_report.Put_Line("trying to replace "
                                          & yy_tokens.Token'image
                                             (yyparser_input.input_token.token)
                                          & " with "
                                          & yy_tokens.Token'image(the_boxed_token.token));
                        end if;
               when insert  => yyparser_input.unget(yyparser_input.input_token);
                        yyparser_input.unget(the_boxed_token);
                        if yy.DEBUG then
                           yy_error_report.Put_Line("trying to insert "
                                           & yy_tokens.Token'image(the_boxed_token.token)
                                           & " before "
                                           & yy_tokens.Token'image(
                                                yyparser_input.input_token.token));
                        end if;
            end case;

            -- Evaluate the correction.
            new_score := simulate_moves;

            if new_score > best_correction.score then
                        -- We find a higher score, so we overwrite the old one.
                        best_correction := (change, new_score, the_boxed_token);
            end if;

            -- We have change the input stream when we do replacing or
            -- inserting. So we must undo the affect.
            declare
               ignore_result : yyparser_input.boxed_token;
            begin
               case change is
                        when replace => ignore_result := yyparser_input.get;
                        when insert  => ignore_result := yyparser_input.get;
                                 ignore_result := yyparser_input.get;
               end case;
            end;

            -- If we got a score higher than stop score, we
            -- feel it is good enough, so we exit.
            exit when best_correction.score > stop_score;

                  end loop;  -- change in replace .. insert

          -- If we got a score higher than stop score, we
          -- feel it is good enough, so we exit.
                  exit when best_correction.score > stop_score;

               end loop;  -- i in shift_reduce_offset...

      end if; -- best_correction.score < stop_score;

         end primary_recovery;


         procedure try_recovery is
    --
    -- OVERVIEW
    --   This is the main procedure doing error recovery.
    --   During the process of error recovery, we use score to
    --   evaluate the potential correction. When we try a potential
    --   correction, we will peek some future tokens and simulate
    --   the work of Parser. Any valid shift, reduce or accept action
    --   in the simulation leading from a potential correction
    --   will increase the score of the potential correction.
    --   Any error found during the simulation will decrease the
    --   score of the potential correction and stop the simulation.
    --   Since we limit the number of tokens being peeked, the
    --   simulation will stop no matter what the correction is.
    --   If the score of a potential correction is higher enough,
    --   we will accept that correction and install and let the Parser
    --   continues. During the simulation, we will do almost the
    --   same work as the actual Parser does, except that we do
    --   not invoke any user actions and we collect the score.
    --   So we will use the state_stack of the Parser. In order
    --   to avoid change the value of state_stack, we will make
    --   a copy of the state_stack and the simulation is done
    --   on the copy. Below is the outline of sequence of corrections
    --   the error recovery algorithm tries:
    --      1. Delete current token.
    --      2. Replace current token with any token acceptible
    --         from current state, or,
    --         Insert any one of the tokens acceptible from current state.
    --      3. If previous parser action is shift, back up one state,
    --         and try the corrections in 1 and 2 again.
    --      4. If none of the scores of the corrections above are highed
    --         enough, we invoke the handle_error in Ayacc.
    --
      correction : correction_type;
      backed_up  : boolean := false; -- indicates whether or not we backed up
      --                                during error recovery.
      -- scoring : evaluate a potential correction with a number. high is good
      min_ok_score : constant integer := 70;       -- will rellluctantly use
      stop_score   : constant integer := 100;      -- this or higher is best.
         begin

      -- First try recovery without backing up.
      primary_recovery(correction, stop_score);

      if correction.score < stop_score then
        -- The score of the correction is not high enough,
        -- so we try to back up and try more corrections.
        -- But we can back up only if previous Parser action
        -- is shift.
               if previous_action >= yy.first_shift_entry then
                  -- Previous action is a shift, so we back up.
                  backed_up := true;

          -- we put back the input token and
          -- roll back the state stack and input token.
                  yyparser_input.unget(yyparser_input.input_token);
                  yyparser_input.input_token := yyparser_input.previous_token;
                  yy.tos := yy.tos - 1;

                  -- Then we try recovery again
                  primary_recovery(correction, stop_score);
               end if;
      end if;  -- correction_score < stop_score

      -- Now we have try all possible correction.
      -- The highest score is in correction.
      if correction.score >= min_ok_score then
        -- We accept this correction.

        -- First, if the input token resides on the different line
        -- of previous token and we have not backed up, we must
        -- output the new line before we printed the error message.
        -- If we have backed up, we do nothing here because
        -- previous line has been output.
               if not backed_up and then
                 ( line_number <
                    yyparser_input.input_token.line_number ) 
               then
                  put_new_line;
                  line_number := yyparser_input.input_token.line_number;
               end if;

               print_correction_message(correction);
               install_correction(correction);

      else
        -- No score is high enough, we try to invoke handle_error
        -- First, if we backed up during error recovery, we now must
        -- try to undo the affect of backing up.
               if backed_up then
                  yyparser_input.input_token := yyparser_input.get;
                  yy.tos := yy.tos + 1;
               end if;

        -- Output the new line if necessary because the
        -- new line has not been output yet.
               if line_number <
                 yyparser_input.input_token.line_number 
               then
                  put_new_line;
                  line_number := yyparser_input.input_token.line_number;
               end if;

               if yy.DEBUG then
                  if not backed_up then
            yy_error_report.Put_Line("can't back yp over last token...");
                  end if;
                  yy_error_report.Put_Line("1st level recovery failed, going to 2nd level...");
               end if;

               -- Point out the position of the token on which error occurs.
               flag_token;

        -- count it as error if it is a new error. NOTE : if correction is accepted, total_errors
        -- count will be increase during error reporting.
               if yy.error_flag = 0 then -- brand new error
                  yy_error_report.Total_Errors := yy_error_report.Total_Errors + 1;
               end if;

               -- Goes to 2nd level.
               handle_error;

      end if; -- correction.score >= min_ok_score

      -- No matter what happen, let the parser move forward.
      yy.look_ahead := true;

         end try_recovery;


      end yyerror_recovery;


-- END OF UMASS CODES.

begin
      -- initialize by pushing state 0 and getting the first input symbol
      yy.state_stack(yy.tos) := 0;
-- UMASS CODES :
      yy_error_report.Initialize_Output;
      -- initialize input token and previous token
      yyparser_input.input_token := new yyparser_input.tokenbox;
      yyparser_input.input_token.line_number := 0;
-- END OF UMASS CODES.


      loop

         yy.index := SHIFT_REDUCE_OFFSET(yy.state_stack(yy.tos));
         if integer(Shift_Reduce_Matrix(yy.index).T) = yy.default then
            yy.action := integer(Shift_Reduce_Matrix(yy.index).Act);
         else
            if yy.look_ahead then
               yy.look_ahead   := false;
-- UMASS CODES :
--    Let Parser get the input from yyparser_input instead of lexical
--    scanner and maintain previous_token and input_token.
               yyparser_input.previous_token := yyparser_input.input_token;
               yyparser_input.input_token := yyparser_input.get;
               yy.input_symbol := yyparser_input.input_token.token;
-- END OF UMASS CODES.

            end if;
            yy.action :=
             parse_action(yy.state_stack(yy.tos), yy.input_symbol);
         end if;

-- UMASS CODES :
--   If input_token is not on the line yyerror_recovery.line_number,
--   we just get to a new line. So we output the new line to
--   file of error report. But if yy.action is error, we
--   will not output the new line because we will do error
--   recovery and during error recovery, we may back up
--   which may cause error reported on previous line.
--   So if yy.action is error, we will let error recovery
--   to output the new line.
         if ( yyerror_recovery.line_number <
             yyparser_input.input_token.line_number ) and then
           yy.action /= yy.error_code 
         then
            put_new_line;
            yyerror_recovery.line_number := yyparser_input.input_token.line_number;
         end if;
-- END OF UMASS CODES.

         if yy.action >= yy.first_shift_entry then  -- SHIFT

            if yy.DEBUG then
               shift_debug(yy.action, yy.input_symbol);
            end if;

            -- Enter new state
            if yy.tos = yy.stack_size then
               Text_IO.Put_Line(" Stack size exceeded on state_stack");
               raise yy_tokens.Syntax_Error;
            end if;
            yy.tos := yy.tos + 1;
            yy.state_stack(yy.tos) := yy.action;
-- UMASS CODES :
--   Set value stack only if valuing is True.
            if yyerror_recovery.valuing then
-- END OF UMASS CODES.
               yy.value_stack(yy.tos) := YYLVal;
-- UMASS CODES :
            end if;
-- END OF UMASS CODES.

            if yy.error_flag > 0 then  -- indicate a valid shift
            yy.error_flag := yy.error_flag - 1;
            end if;

            -- Advance lookahead
            yy.look_ahead := true;

         elsif yy.action = yy.error_code then       -- ERROR
-- UMASS CODES :
            try_recovery;
-- END OF UMASS CODES.


         elsif yy.action = yy.accept_code then
            if yy.DEBUG then
               Text_IO.Put_Line("Ayacc.YYParse: Accepting Grammar...");
-- UMASS CODES :
               yy_error_report.Put_Line("Ayacc.YYParse: Accepting Grammar...");
-- END OF UMASS CODES.
            end if;
            exit;

         else -- Reduce Action

            -- Convert action into a rule
            yy.rule_id  := -1 * yy.action;

            -- Execute User Action
            -- user_action(yy.rule_id);

-- UMASS CODES :
--   Only invoke semantic action if valuing is True.
--   And if exception is raised during semantic action
--   and total_errors is not zero, we set valuing to False
--   because we assume that error recovery causes the exception
--   and we no longer want to invoke any semantic action.
            if yyerror_recovery.valuing then
               begin
-- END OF UMASS CODES.

                  case yy.rule_id is

when  18 =>
--#line  163

   null;
      

when  19 =>
--#line  169

                     null;
      

when  20 =>
--#line  175

   MAST_System.Generation_Tool:=YYVal.text;
      

when  21 =>
--#line  181

   MAST_System.Generation_Profile:=YYVal.text;
      

when  22 =>
--#line  187

   MAST_System.Generation_Date:=YYVal.date;
                     if not Mast.IO.Is_Date_OK(MAST_System.Generation_Date) then
                        user_defined_errors.parser_error
                 ("Error in date value");
                     end if;
      

when  28 =>
--#line  208

                     The_Slack_Res:=new Mast.Results.Slack_Result;
   Systems.Set_Slack_Result(MAST_System,The_Slack_Res);
      

when  29 =>
--#line  213

   Mast.Results.Set_Slack(The_Slack_Res.all,Float(YYVal.float_num));
      

when  30 =>
--#line  219

                     The_trace_Res:=new Mast.Results.Trace_Result;
   Systems.Set_Trace_Result(MAST_System,The_trace_Res);
      

when  31 =>
--#line  224

   Mast.Results.Set_Pathname(The_trace_Res.all,To_String(YYVal.text));
      

when  37 =>
--#line  246

   --find the processing resource
                     declare
                        The_Index : Processing_Resources.Lists.Index;
                     begin
                        The_Index:=Processing_Resources.Lists.Find
       (Symbol_Table.Item(YYVal.name_index),
             MAST_System.Processing_Resources);
                        PR_Ref:=Processing_Resources.Lists.Item
          (The_Index,MAST_System.Processing_Resources);             
                     exception
                        when List_Exceptions.Invalid_Index =>
                           -- create dummy processing resource
                           PR_Ref:=new Processing_Resources.Processor.Regular_Processor;
                           user_defined_errors.parser_error
                  (To_String("Processing Resource "&
                   Symbol_Table.Item(YYVal.name_index)&" not found"));
                     end;
      

when  44 =>
--#line  280

                     The_Slack_Res:=new Mast.Results.Slack_Result;
   Processing_Resources.Set_Slack_Result(PR_Ref.all,The_Slack_Res);
      

when  45 =>
--#line  285

   Mast.Results.Set_Slack(The_Slack_Res.all,Float(YYVal.float_num));
      

when  48 =>
--#line  295

                     The_Utilization_res:=new Mast.Results.Utilization_Result;
                     Processing_Resources.Set_Utilization_Result
            (PR_Ref.all,The_Utilization_res);
      

when  50 =>
--#line  304

                     The_Utilization_res:=new Mast.Results.Detailed_Utilization_Result;
                     Processing_Resources.Set_Utilization_Result
            (PR_Ref.all,The_Utilization_res);
      

when  59 =>
--#line  324

   Mast.Results.Set_Total
          (Mast.Results.Utilization_Result(The_Utilization_res.all),
           Float(YYVal.float_num));
      

when  60 =>
--#line  332

   Mast.Results.Set_Application
          (Mast.Results.Detailed_Utilization_Result(The_Utilization_res.all),
           Float(YYVal.float_num));
      

when  61 =>
--#line  340

   Mast.Results.Set_Context_Switch
          (Mast.Results.Detailed_Utilization_Result(The_Utilization_res.all),
           Float(YYVal.float_num));
      

when  62 =>
--#line  348

   Mast.Results.Set_Timer
          (Mast.Results.Detailed_Utilization_Result(The_Utilization_res.all),
           Float(YYVal.float_num));
      

when  63 =>
--#line  356

   Mast.Results.Set_Driver
          (Mast.Results.Detailed_Utilization_Result(The_Utilization_res.all),
           Float(YYVal.float_num));
      

when  64 =>
--#line  365

                     The_Queue_Size_Res:=new Mast.Results.Ready_Queue_Size_Result;
   Processing_Resources.Set_Ready_Queue_Size_Result
           (PR_Ref.all,
            Mast.Results.Ready_Queue_Size_Result_Ref(The_Queue_Size_Res));
      

when  65 =>
--#line  372

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Max_Num should be integer value");
                     end if;
   Mast.Results.Set_Max_Num
           (The_Queue_Size_Res.all,YYVal.num);
      

when  71 =>
--#line  399

   --find the shared resource
                     declare
                        The_Index : Shared_Resources.Lists.Index;
                     begin
                        The_Index:=Shared_Resources.Lists.Find
       (Symbol_Table.Item(YYVal.name_index),
             MAST_System.Shared_Resources);
                        SR_Ref:=Shared_Resources.Lists.Item
          (The_Index,MAST_System.Shared_Resources);             
                     exception
                        when List_Exceptions.Invalid_Index =>
                           -- create dummy shared resource
                           SR_Ref:=new Shared_Resources.Immediate_Ceiling_Resource;
                           user_defined_errors.parser_error
                  (To_String("Shared Resource "&
                   Symbol_Table.Item(YYVal.name_index)&" not found"));
                     end;
      

when  79 =>
--#line  434

                     The_Ceiling_Res:=new Mast.Results.Ceiling_Result;
   Shared_Resources.Set_Ceiling_Result(SR_Ref.all,The_Ceiling_Res);
      

when  80 =>
--#line  439

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Ceiling should be integer value");
                     end if;
                     begin
                        Mast.Results.Set_Ceiling(The_Ceiling_Res.all,Priority(YYVal.num));
                     exception
                        when Constraint_Error =>
                           user_defined_errors.parser_error
                 ("Priority ceiling value out of range");
                     end;
      

when  81 =>
--#line  455

                     The_Preemption_Level_Res:=new Mast.Results.Preemption_Level_Result;
   Shared_Resources.Set_Preemption_Level_Result
          (SR_Ref.all,The_Preemption_Level_Res);
      

when  82 =>
--#line  461

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Preemption Level should be integer value");
                     end if;
                     begin
                        Mast.Results.Set_Preemption_Level
              (The_Preemption_Level_Res.all,Mast.Preemption_Level(YYVal.num));
                     exception
                        when Constraint_Error =>
                           user_defined_errors.parser_error
                 ("Preemption level value out of range");
                     end;
      

when  83 =>
--#line  478

                     The_Utilization_res:=new Mast.Results.Utilization_Result;
                     Shared_Resources.Set_Utilization_Result
            (SR_Ref.all,The_Utilization_res);
      

when  84 =>
--#line  484

   Mast.Results.Set_Total
          (The_Utilization_res.all,Float(YYVal.float_num));
      

when  85 =>
--#line  491

                     The_Queue_Size_Res:=new Mast.Results.Queue_Size_Result;
   Shared_Resources.Set_Queue_Size_Result
           (SR_Ref.all,The_Queue_Size_Res);
      

when  86 =>
--#line  497

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Max_Num should be integer value");
                     end if;
   Mast.Results.Set_Max_Num
           (The_Queue_Size_Res.all,YYVal.num);
      

when  92 =>
--#line  524

   --find the operation
                     declare
                        The_Index : Operations.Lists.Index;
                     begin
                        The_Index:=Operations.Lists.Find
       (Symbol_Table.Item(YYVal.name_index),
             MAST_System.Operations);
                        Op_Ref:=Operations.Lists.Item
          (The_Index,MAST_System.Operations);             
                     exception
                        when List_Exceptions.Invalid_Index =>
                           -- create dummy operation
                           Op_Ref:=new Operations.Simple_Operation;
                           user_defined_errors.parser_error
                  (To_String("Operation "&
                   Symbol_Table.Item(YYVal.name_index)&" not found"));
                     end;
      

when  97 =>
--#line  556

                     The_Slack_Res:=new Mast.Results.Slack_Result;
   Operations.Set_Slack_Result(Op_Ref.all,The_Slack_Res);
      

when  98 =>
--#line  561

   Mast.Results.Set_Slack(The_Slack_Res.all,Float(YYVal.float_num));
      

when  105 =>
--#line  581

                     Sched_Params_Ref:=
              new Mast.Scheduling_Parameters.Fixed_Priority_Policy;
                     preassigned_field_present:=False;
      

when  114 =>
--#line  605

                     preassigned_field_present:=True;
                     if Sched_Params_Ref.all in 
             Mast.Scheduling_Parameters.Interrupt_FP_Policy
          then          
                        Mast.Scheduling_Parameters.Set_Preassigned
               (Mast.Scheduling_Parameters.Fixed_Priority_Parameters'Class
                    (Sched_Params_Ref.all), True);
                        if not YYVal.flag then
                           user_defined_errors.parser_error
                  ("Preassigned field in Interrupt Scheduler cannot be 'No'");
                        end if;
                     else
                        Mast.Scheduling_Parameters.Set_Preassigned
               (Mast.Scheduling_Parameters.Fixed_Priority_Parameters'Class
                    (Sched_Params_Ref.all), YYVal.flag);
                     end if;
      

when  115 =>
--#line  626

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Priority should be integer value");
                     end if;
                     begin
                        Mast.Scheduling_Parameters.Set_The_Priority
             (Mast.Scheduling_Parameters.Fixed_Priority_Parameters'Class
                    (Sched_Params_Ref.all),
              Mast.Priority(YYVal.num));
                        if not preassigned_field_present then
                           Mast.Scheduling_Parameters.Set_Preassigned
                (Mast.Scheduling_Parameters.Fixed_Priority_Parameters'Class
                     (Sched_Params_Ref.all), True);
                        end if;
                     exception
                        when Constraint_Error =>
                           user_defined_errors.parser_error
                 ("Priority value out of range");
                     end;
      

when  116 =>
--#line  650

                     Sched_Params_Ref:=
               new Mast.Scheduling_Parameters.Non_Preemptible_FP_Policy;
                     preassigned_field_present:=False;
      

when  125 =>
--#line  674

                     Sched_Params_Ref:=
               new Mast.Scheduling_Parameters.Interrupt_FP_Policy;
                     preassigned_field_present:=False;
                     Mast.Scheduling_Parameters.Set_Preassigned
             (Mast.Scheduling_Parameters.Fixed_Priority_Parameters'Class
                  (Sched_Params_Ref.all), True);
      

when  134 =>
--#line  701

                     Sched_Params_Ref:=new Mast.Scheduling_Parameters.Polling_Policy;
                     preassigned_field_present:=False;
      

when  145 =>
--#line  724

                     Mast.Scheduling_Parameters.Set_Polling_Period
             (Mast.Scheduling_Parameters.Polling_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Time(YYVal.float_num));
      

when  146 =>
--#line  733

                     Mast.Scheduling_Parameters.Set_Polling_Worst_Overhead
             (Mast.Scheduling_Parameters.Polling_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Normalized_Execution_Time(YYVal.float_num));
      

when  147 =>
--#line  742

                     Mast.Scheduling_Parameters.Set_Polling_Best_Overhead
             (Mast.Scheduling_Parameters.Polling_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Normalized_Execution_Time(YYVal.float_num));
      

when  148 =>
--#line  751

                     Mast.Scheduling_Parameters.Set_Polling_Avg_Overhead
             (Mast.Scheduling_Parameters.Polling_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Normalized_Execution_Time(YYVal.float_num));
      

when  149 =>
--#line  760

                     Sched_Params_Ref:=
             new Mast.Scheduling_Parameters.Sporadic_Server_Policy;
                     preassigned_field_present:=False;
      

when  160 =>
--#line  785

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Priority should be integer value");
                     end if;
                     begin
                        Mast.Scheduling_Parameters.Set_The_Priority
             (Mast.Scheduling_Parameters.Sporadic_Server_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Priority(YYVal.num));
                        if not preassigned_field_present then
                           Mast.Scheduling_Parameters.Set_Preassigned
                (Mast.Scheduling_Parameters.Fixed_Priority_Parameters'Class
                     (Sched_Params_Ref.all), True);
                        end if;
                     exception
                        when Constraint_Error =>
                           user_defined_errors.parser_error
                 ("Priority value out of range");
                     end;
      

when  161 =>
--#line  809

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Priority should be integer value");
                     end if;
                     begin
                        Mast.Scheduling_Parameters.Set_Background_Priority
             (Mast.Scheduling_Parameters.Sporadic_Server_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Priority(YYVal.num));
                     exception
                        when Constraint_Error =>
                           user_defined_errors.parser_error
                 ("Priority value out of range");
                     end;
      

when  162 =>
--#line  828

                     Mast.Scheduling_Parameters.Set_Initial_Capacity
             (Mast.Scheduling_Parameters.Sporadic_Server_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Time(YYVal.float_num));
      

when  163 =>
--#line  837

                     Mast.Scheduling_Parameters.Set_Replenishment_Period
             (Mast.Scheduling_Parameters.Sporadic_Server_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Time(YYVal.float_num));
      

when  164 =>
--#line  846

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Max pending replenishments should be integer value");
                     end if;
                     Mast.Scheduling_Parameters.Set_Max_Pending_Replenishments
             (Mast.Scheduling_Parameters.Sporadic_Server_Policy'Class
                    (Sched_Params_Ref.all),
              YYVal.num);
      

when  165 =>
--#line  859

                     Sched_Params_Ref:=new Mast.Scheduling_Parameters.EDF_Policy;
                     preassigned_field_present:=False;
      

when  174 =>
--#line  882

                     preassigned_field_present:=True;
                     Mast.Scheduling_Parameters.Set_Preassigned
               (Mast.Scheduling_Parameters.EDF_Policy'Class
                    (Sched_Params_Ref.all), YYVal.flag);
      

when  175 =>
--#line  891

                     Mast.Scheduling_Parameters.Set_Deadline
             (Mast.Scheduling_Parameters.EDF_Policy'Class
                    (Sched_Params_Ref.all),
              Mast.Time(YYVal.float_num));
                     if not preassigned_field_present then
                        Mast.Scheduling_Parameters.Set_Preassigned
                (Mast.Scheduling_Parameters.EDF_Policy'Class
                     (Sched_Params_Ref.all), True);
                     end if;
      

when  177 =>
--#line  913

                     Synch_Params_Ref:=new Mast.Synchronization_Parameters.SRP_Parameters;
                     preassigned_field_present:=False;
      

when  186 =>
--#line  936

                     preassigned_field_present:=True;
                     Mast.Synchronization_Parameters.Set_Preassigned
               (Mast.Synchronization_Parameters.SRP_Parameters'Class
                    (Synch_Params_Ref.all), YYVal.flag);
      

when  187 =>
--#line  945

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Preemption level should be integer value");
                     end if;
                     begin
                        Mast.Synchronization_Parameters.Set_Preemption_Level
             (Mast.Synchronization_Parameters.SRP_Parameters'Class
                    (Synch_Params_Ref.all),
              Mast.Preemption_Level(YYVal.num));
                        if not preassigned_field_present then
                           Mast.Synchronization_Parameters.Set_Preassigned
                (Mast.Synchronization_Parameters.SRP_Parameters'Class
                     (Synch_Params_Ref.all), True);
                        end if;
                     exception
                        when Constraint_Error =>
                           user_defined_errors.parser_error
                 ("Preemption level value out of range");
                     end;
      

when  193 =>
--#line  985

   --find the transaction
                     declare
                        The_Index : Transactions.Lists.Index;
                     begin
                        The_Index:=Transactions.Lists.Find
       (Symbol_Table.Item(YYVal.name_index),
             MAST_System.Transactions);
                        Tr_Ref:=Transactions.Lists.Item
          (The_Index,MAST_System.Transactions);
                     exception
                        when List_Exceptions.Invalid_Index =>
                           -- create dummy transaction
                           Tr_Ref:=new Transactions.Regular_Transaction;
                           user_defined_errors.parser_error
                  (To_String("Transaction "&
                   Symbol_Table.Item(YYVal.name_index)&" not found"));
                     end;
      

when  200 =>
--#line  1019

                     The_Slack_Res:=new Mast.Results.Slack_Result;
   Transactions.Set_Slack_Result(Tr_Ref.all,The_Slack_Res);
      

when  201 =>
--#line  1024

   Mast.Results.Set_Slack(The_Slack_Res.all,Float(YYVal.float_num));
      

when  202 =>
--#line  1030

                     The_Timing_Res:=new Mast.Results.Timing_Result;
      

when  214 =>
--#line  1051

                     declare
                        The_Link : Mast.Graphs.Link_Ref;
                     begin
                        --find the link
                        The_Link:=Transactions.Find_Internal_Event_Link
       (Symbol_Table.Item(YYVal.name_index),
             Tr_Ref.all);
                        Mast.Graphs.Links.Set_Link_Time_Results
              (Mast.Graphs.Links.Regular_Link(The_Link.all),
              The_Timing_Res);
                        Mast.Results.Set_Link
             (The_Timing_Res.all,The_Link);
                     exception
                           -- create dummy link
                        when Transactions.Link_Not_Found =>
                           user_defined_errors.parser_error
                  (To_String("Event "&
                   Symbol_Table.Item(YYVal.name_index)&
                   " not found in transaction "&
                   Transactions.Name(Tr_Ref)));
                     end;
      

when  215 =>
--#line  1077

   Mast.Results.Set_Worst_Local_Response_Time
          (The_Timing_Res.all, Time(YYVal.float_num));
      

when  216 =>
--#line  1084

   Mast.Results.Set_Best_Local_Response_Time
          (The_Timing_Res.all, Time(YYVal.float_num));
      

when  217 =>
--#line  1091

   Mast.Results.Set_Worst_Blocking_Time
          (The_Timing_Res.all, Time(YYVal.float_num));
      

when  218 =>
--#line  1098

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Num_Of_Suspensions should be integer value");
                     end if;
   Mast.Results.Set_Num_Of_Suspensions
          (The_Timing_Res.all, YYVal.num);
      

when  224 =>
--#line  1123

                     Mast.Results.Set_Worst_Global_Response_Time
          (The_Timing_Res.all, The_Event_Ref, Mast.Time(YYVal.float_num));
      

when  225 =>
--#line  1130

         declare
            a_name : Var_String;
         begin
                        a_name:=Symbol_Table.Item(YYVal.name_index);
                        The_Event_Ref:=Transactions.Find_Any_Event (a_name,Tr_Ref.all);
         exception
                        when Transactions.Event_Not_Found =>
                           user_defined_errors.parser_error
                    ("Event name "&To_String(a_name)&" not found");
         end;
      

when  231 =>
--#line  1159

                     Mast.Results.Set_Best_Global_Response_Time
          (The_Timing_Res.all, The_Event_Ref, Mast.Time(YYVal.float_num));
      

when  237 =>
--#line  1179

                     Mast.Results.Set_Jitter
          (The_Timing_Res.all, The_Event_Ref, Mast.Time(YYVal.float_num));
      

when  238 =>
--#line  1187

                     The_Timing_Res:=new Mast.Results.Simulation_Timing_Result;
      

when  258 =>
--#line  1216

   Mast.Results.Set_Local_Simulation_Time
          (Mast.Results.Simulation_Timing_Result
            (The_Timing_Res.all), (Time(YYVal.float_num),1));
      

when  259 =>
--#line  1224

   Mast.Results.Set_Avg_Blocking_Time
          (Mast.Results.Simulation_Timing_Result
            (The_Timing_Res.all), Time(YYVal.float_num));
      

when  260 =>
--#line  1232

   Mast.Results.Set_Max_Preemption_Time
          (Mast.Results.Simulation_Timing_Result
            (The_Timing_Res.all), Time(YYVal.float_num));
      

when  261 =>
--#line  1240

   Mast.Results.Set_Suspension_Time
          (Mast.Results.Simulation_Timing_Result
            (The_Timing_Res.all), Time(YYVal.float_num));
      

when  262 =>
--#line  1248

                     if YYVal.Is_Float then
                        user_defined_errors.parser_error
                 ("Num_Of_Queued_Activations should be integer value");
                     end if;
   Mast.Results.Set_Num_Of_Queued_Activations
          (Mast.Results.Simulation_Timing_Result
            (The_Timing_Res.all), YYVal.num);
      

when  268 =>
--#line  1274

                     Mast.Results.Set_Global_Simulation_Time
          (Mast.Results.Simulation_Timing_Result
            (The_Timing_Res.all), The_Event_Ref, 
            (Mast.Time(YYVal.float_num),1));
      

when  273 =>
--#line  1294

         A_Deadline:=Mast.Time(YYVal.float_num);
      

when  274 =>
--#line  1300

         declare
            The_Ratio : Float;
         begin
            The_Ratio:=Float(YYVal.float_num);
            Mast.Results.Set_Local_Miss_Ratio
             (Mast.Results.Simulation_Timing_Result
               (The_Timing_Res.all), A_Deadline, 
                 (Integer(The_Ratio*1.0E6),1E8));
         end;
      

when  283 =>
--#line  1334

         declare
            The_Ratio : Float;
         begin
            The_Ratio:=Float(YYVal.float_num);
            Mast.Results.Set_Global_Miss_Ratio
             (Mast.Results.Simulation_Timing_Result
               (The_Timing_Res.all), A_Deadline, The_Event_Ref,
                (Integer(The_Ratio*1.0E6),1E8));
         end;
      

when  289 =>
--#line  1364

   --find the scheduling_server
                     declare
                        The_Index : Mast.Scheduling_Servers.Lists.Index;
                     begin
                        The_Index:=Mast.Scheduling_Servers.Lists.Find
       (Symbol_Table.Item(YYVal.name_index),
             MAST_System.Scheduling_Servers);
                        SS_Ref:=Mast.Scheduling_Servers.Lists.Item
          (The_Index,MAST_System.Scheduling_Servers);             
                     exception
                        when List_Exceptions.Invalid_Index =>
                           -- create dummy scheduling_server
                           SS_Ref:=new Mast.Scheduling_Servers.Scheduling_Server;
                           user_defined_errors.parser_error
                  (To_String("Scheduling Server "&
                   Symbol_Table.Item(YYVal.name_index)&" not found"));
                     end;
      

when  295 =>
--#line  1397

                     The_SP_Res:=new Mast.Results.Sched_Params_Result;
   Mast.Scheduling_Servers.Set_Sched_Params_Result(SS_Ref.all,The_SP_Res);
      

when  296 =>
--#line  1402

   Mast.Results.Set_Sched_Params(The_SP_Res.all,Sched_Params_Ref);
      

when  297 =>
--#line  1408

                     The_SynchP_Res:=new Mast.Results.Synch_Params_Result;
   Mast.Scheduling_Servers.Set_Synch_Params_Result
           (SS_Ref.all,The_SynchP_Res);
      

when  298 =>
--#line  1414

   Mast.Results.Set_Synch_Params(The_SynchP_Res.all,Synch_Params_Ref);
      

                     when others => null;
                  end case;

-- UMASS CODES :
--   Corresponding to the codes above.
               exception
                  when others =>
                     if yy_error_report.Total_Errors > 0 then
                     yyerror_recovery.valuing := False;
                     -- We no longer want to invoke any semantic action.
                     else
                     -- this exception is not caused by syntax error,
                     -- so we reraise anyway.
                     yy_error_report.Finish_Output;
                     raise;
                     end if;
               end;
            end if;
-- END OF UMASS CODES.

            -- Pop RHS states and goto next state
            yy.tos      := yy.tos - Rule_Length(yy.rule_id) + 1;
            if yy.tos > yy.stack_size then
               Text_IO.Put_Line(" Stack size exceeded on state_stack");
-- UMASS CODES :
               yy_error_report.Put_Line(" Stack size exceeded on state_stack");
               yyerror_recovery.finale;
-- END OF UMASS CODES.
               raise yy_tokens.Syntax_Error;
            end if;
            yy.state_stack(yy.tos) := goto_state(yy.state_stack(yy.tos-1) ,
                                 Get_LHS_Rule(yy.rule_id));

-- UMASS CODES :
--   Set value stack only if valuing is True.
            if yyerror_recovery.valuing then
-- END OF UMASS CODES.
               yy.value_stack(yy.tos) := YYVal;
-- UMASS CODES :
            end if;
-- END OF UMASS CODES.

            if yy.DEBUG then
               reduce_debug(yy.rule_id,
                    goto_state(yy.state_stack(yy.tos - 1),
                               Get_LHS_Rule(yy.rule_id)));
            end if;

         end if;

-- UMASS CODES :
--   If the error flag is set to zero at current token,
--   we flag current token out.
         if yyerror_recovery.previous_error_flag > 0 and then
           yy.error_flag = 0 
         then
            yyerror_recovery.flag_token ( error => False );
         end if;

--   save the action made and error flag.
         yyerror_recovery.previous_action := yy.action;
         yyerror_recovery.previous_error_flag := yy.error_flag;
-- END OF UMASS CODES.

      end loop;

-- UMASS CODES :
      finale;
-- END OF UMASS CODES.

end YYParse;

begin 
   mast_results_lex_dfa.yy_init:=true;
   mast_results_lex_dfa.yy_start:=0;
   mast_results_lex_io.Saved_Tok_Line1:=null;
   mast_results_lex_io.Saved_Tok_Line2:=null;
   mast_results_lex_io.Line_Number_Of_Saved_Tok_Line1:=0;
   mast_results_lex_io.Line_Number_Of_Saved_Tok_Line2:=0;
   mast_results_lex_io.Tok_Begin_Line:=1;
   mast_results_lex_io.Tok_End_Line:=1;
   mast_results_lex_io.Tok_Begin_Col:=0;
   mast_results_lex_io.Tok_End_Col:=0;
   mast_results_lex_io.Token_At_End_Of_Line:=False;
   Mast_Results_Parser_Error_Report.Total_Errors:=0;
   Mast_Results_Parser_Error_Report.Total_Warnings:=0;
   YYParse;
end MAST_Results_Parser;



