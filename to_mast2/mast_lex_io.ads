-----------------------------------------------------------------------
--                              Mast                                 --
--     Modelling and Analysis Suite for Real-Time Applications       --
--                                                                   --
--                       Copyright (C) 2000-2014                     --
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
with mast_lex_dfa; use mast_lex_dfa; 
with text_io; use text_io;

package mast_lex_io is
user_input_file : file_type;
user_output_file : file_type;
NULL_IN_INPUT : exception;
AFLEX_INTERNAL_ERROR : exception;
UNEXPECTED_LAST_MATCH : exception;
PUSHBACK_OVERFLOW : exception;
AFLEX_SCANNER_JAMMED : exception;
type eob_action_type is ( EOB_ACT_RESTART_SCAN,
                          EOB_ACT_END_OF_FILE,
                          EOB_ACT_LAST_MATCH );
YY_END_OF_BUFFER_CHAR :  constant character:=  ASCII.NUL;
yy_n_chars : integer;       -- number of characters read into yy_ch_buf

-- true when we've seen an EOF for the current input file
yy_eof_has_been_seen : boolean;

-- UMASS CODES :
--   In order to support YY_Get_Token_Line, we need
--   a variable to hold current line.
type String_Ptr is access string;
Saved_Tok_Line1 : String_Ptr := Null;
Line_Number_Of_Saved_Tok_Line1 : integer := 0;
Saved_Tok_Line2 : String_Ptr := Null;
Line_Number_Of_Saved_Tok_Line2 : integer := 0;
-- Aflex will try to get next buffer before it processs the
-- last token. Since now Aflex has been changed to accept
-- one line by one line, the last token in the buffer is
-- always end_of_line ( or end_of_buffer ). So before the
-- end_of_line is processed, next line will be retrieved
-- into the buffer. So we need to maintain two lines,
-- which line will be returned in Get_Token_Line is
-- determined according to the line number. It is the same
-- reason that we can not reinitialize tok_end_col to 0 in
-- Yy_Input, but we must do it in yylex after we process the
-- end_of_line.
Tok_Begin_Line : integer := 1;
Tok_End_Line : integer := 1;
Tok_End_Col : integer := 0;
Tok_Begin_Col : integer := 0;
Token_At_End_Of_Line : Boolean := False;
-- Indicates whether or not last matched token is end_of_line.
-- END OF UMASS CODES.

procedure YY_INPUT(buf: out unbounded_character_array; result: out integer; max_size: in integer);
function yy_get_next_buffer return eob_action_type;
procedure yyunput( c : character; yy_bp: in out integer );
procedure unput(c : character);
function input return character;
procedure output(c : character);
function yywrap return boolean;
procedure Open_Input(fname : in String);
procedure Close_Input;
procedure Create_Output(fname : in String := "");
procedure Close_Output;

-- UMASS CODES :
procedure Yy_Get_Token_Line ( Yy_Line_String : out String;
                              Yy_Line_Length : out Natural );
-- Returnes the entire line in the input, on which the currently
-- matched token resides.

function Yy_Line_Number return Natural;
-- Returns the line number of the currently matched token.
-- In case a token spans lines, then the line number of the first line
-- is returned.

function Yy_Begin_Column return Natural;
function Yy_End_Column return Natural;
-- Returns the beginning and ending column positions of the
-- currently mathched token. If the token spans lines then the
-- begin column number is the column number on the first line
-- and the end columne number is the column number on the last line.

-- END OF UMASS CODES.

end mast_lex_io;
