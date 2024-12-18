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

package body mast_results_lex_io is
-- gets input and stuffs it into 'buf'.  number of characters read, or YY_NULL,
-- is returned in 'result'.

procedure YY_INPUT(buf: out unbounded_character_array; result: out integer; max_size: integer) is
      c : character;
      i : integer := 1;
      loc : integer := buf'first;
-- UMASS CODES :
--    Since buf is an out parameter which is not readable
--    and saved lines is a string pointer which space must
--    be allocated after we know the size, we maintain
--    an extra buffer to collect the input line and
--    save it into the saved line 2.
   Temp_Line : String ( 1 .. YY_BUF_SIZE + 2 );
-- END OF UMASS CODES.
begin
-- UMASS CODES :
      buf := ( others => ASCII.NUL );
-- Move the saved lines forward.
      Saved_Tok_Line1 := Saved_Tok_Line2;
      Line_Number_Of_Saved_Tok_Line1 := Line_Number_Of_Saved_Tok_Line2;
-- END OF UMASS CODES.

      if Is_Open(user_input_file) then
      while  i <= max_size  loop
         if End_Of_Line(user_input_file) then -- Ada ate our newline, put it back on the end.
               buf(loc) := ASCII.LF;
               Skip_Line(user_input_file, 1);
-- UMASS CODES :
--   We try to get one line by one line. So we return
--   here because we saw the end_of_line.
               result := i;
               Temp_Line(i) := ASCII.LF;
               Saved_Tok_Line2 := new String ( 1 .. i );
               Saved_Tok_Line2 ( 1 .. i ) := Temp_Line ( 1 .. i );
               Line_Number_Of_Saved_Tok_Line2 := Line_Number_Of_Saved_Tok_Line1 + 1;
               return;
-- END OF UMASS CODES.
         else
-- UCI CODES CHANGED:
--    The following codes are modified. Previous codes is commented out.
--    The purpose of doing this is to make it possible to set Temp_Line
--    in Ayacc-extension specific codes. Definitely, we can read the character
--    into the Temp_Line and then set the buf. But Temp_Line will only
--    be used in Ayacc-extension specific codes which makes this approach impossible.
               Get(user_input_file, c);
               buf(loc) := c;
--         get(user_input_file, buf(loc));
-- UMASS CODES :
               Temp_Line(i) := c;
-- END OF UMASS CODES.
         end if;

         loc := loc + 1;
         i := i + 1;
      end loop;
      else
      while  i <= max_size  loop
         if End_Of_Line then -- Ada ate our newline, put it back on the end.
               buf(loc) := ASCII.LF;
               Skip_Line(1);
-- UMASS CODES :
--   We try to get one line by one line. So we return
--   here because we saw the end_of_line.
               result := i;
               Temp_Line(i) := ASCII.LF;
               Saved_Tok_Line2 := new String ( 1 .. i );
               Saved_Tok_Line2 ( 1 .. i ) := Temp_Line ( 1 .. i );
               Line_Number_Of_Saved_Tok_Line2 := Line_Number_Of_Saved_Tok_Line1 + 1;
               return;
-- END OF UMASS CODES.

         else
--    The following codes are modified. Previous codes is commented out.
--    The purpose of doing this is to make it possible to set Temp_Line
--    in Ayacc-extension specific codes. Definitely, we can read the character
--    into the Temp_Line and then set the buf. But Temp_Line will only
--    be used in Ayacc-extension specific codes which makes this approach impossible.
               Get(c);
               buf(loc) := c;
--         get(buf(loc));
-- UMASS CODES :
               Temp_Line(i) := c;
-- END OF UMASS CODES.
         end if; 

         loc := loc + 1;
         i := i + 1;
      end loop;
      end if; -- for input file being standard input

      result := i - 1; 
-- UMASS CODES :
--   Since we get one line by one line, if we
--   reach here, it means that current line have
--   more that max_size characters. So it is
--   impossible to hold the whole line. We
--   report the warning message and continue.
      buf(loc - 1) := Ascii.LF;
      if Is_Open(user_input_file) then
      Skip_Line(user_input_file, 1);
      else
      Skip_Line(1);
      end if;
      Temp_Line(i-1) := ASCII.LF;
      Saved_Tok_Line2 := new String ( 1 .. i - 1);
      Saved_Tok_Line2 ( 1 .. i - 1 ) := Temp_Line ( 1 .. i - 1 );
      Line_Number_Of_Saved_Tok_Line2 := Line_Number_Of_Saved_Tok_Line1 + 1;
      Put_Line ( "Input line "
               & Integer'Image ( Line_Number_Of_Saved_Tok_Line2 )
               & "has more than "
               & Integer'Image ( max_size )
               & " characters, ... truncated." );
-- END OF UMASS CODES.
   exception
      when End_Error => result := i - 1;
         -- when we hit EOF we need to set yy_eof_has_been_seen
         yy_eof_has_been_seen := true;
-- UMASS CODES :
--   Processing incomplete line.
         if i /= 1 then
          -- Current line is not empty but do not have end_of_line.
          -- So current line is incomplete line. But we still need
          -- to save it.
            Saved_Tok_Line2 := new String ( 1 .. i - 1 );
            Saved_Tok_Line2 ( 1 .. i - 1 ) := Temp_Line ( 1 .. i - 1 );
            Line_Number_Of_Saved_Tok_Line2 := Line_Number_Of_Saved_Tok_Line1 + 1;
         end if;
-- END OF UMASS CODES.
end YY_INPUT;

-- yy_get_next_buffer - try to read in new buffer
--
-- returns a code representing an action
--     EOB_ACT_LAST_MATCH - 
--     EOB_ACT_RESTART_SCAN - restart the scanner
--     EOB_ACT_END_OF_FILE - end of file

function yy_get_next_buffer return eob_action_type is
      dest : integer := 0;
      source : integer := yytext_ptr - 1; -- copy prev. char, too
      number_to_move : integer;
      ret_val : eob_action_type;
      num_to_read : integer;
begin    
      if  yy_c_buf_p > yy_n_chars + 1  then
         raise NULL_IN_INPUT;
      end if;

      -- try to read more data

      -- first move last chars to start of buffer
      number_to_move := yy_c_buf_p - yytext_ptr;

      for i in 0..number_to_move - 1 loop
         yy_ch_buf(dest) := yy_ch_buf(source);
         dest := dest + 1;
         source := source + 1;
      end loop;
        
      if  yy_eof_has_been_seen  then
    -- don't do the read, it's not guaranteed to return an EOF,
    -- just force an EOF

         yy_n_chars := 0;
      else
         num_to_read := YY_BUF_SIZE - number_to_move - 1;

         if  num_to_read > YY_READ_BUF_SIZE  then
            num_to_read := YY_READ_BUF_SIZE;
         end if;

         -- read in more data
         YY_INPUT( yy_ch_buf(number_to_move..yy_ch_buf'last), yy_n_chars, num_to_read );
      end if;
      if  yy_n_chars = 0  then
         if  number_to_move = 1  then
            ret_val := EOB_ACT_END_OF_FILE;
         else
            ret_val := EOB_ACT_LAST_MATCH;
         end if;

         yy_eof_has_been_seen := true;
      else
         ret_val := EOB_ACT_RESTART_SCAN;
      end if;
    
      yy_n_chars := yy_n_chars + number_to_move;
      yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
      yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;

    -- yytext begins at the second character in
    -- yy_ch_buf; the first character is the one which
    -- preceded it before reading in the latest buffer;
    -- it needs to be kept around in case it's a
    -- newline, so yy_get_previous_state() will have
    -- with '^' rules active

      yytext_ptr := 1;

      return ret_val;
end yy_get_next_buffer;

procedure yyunput( c : character; yy_bp: in out integer ) is
      number_to_move : integer;
      dest : integer;
      source : integer;
      tmp_yy_cp : integer;
begin
      tmp_yy_cp := yy_c_buf_p;
      yy_ch_buf(tmp_yy_cp) := yy_hold_char; -- undo effects of setting up yytext

      if  tmp_yy_cp < 2  then
         -- need to shift things up to make room
         number_to_move := yy_n_chars + 2; -- +2 for EOB chars
         dest := YY_BUF_SIZE + 2;
         source := number_to_move;

         while  source > 0  loop
            dest := dest - 1;
            source := source - 1;
            yy_ch_buf(dest) := yy_ch_buf(source);
         end loop;

         tmp_yy_cp := tmp_yy_cp + dest - source;
         yy_bp := yy_bp + dest - source;
         yy_n_chars := YY_BUF_SIZE;

         if  tmp_yy_cp < 2  then
            raise PUSHBACK_OVERFLOW;
         end if;
      end if;

      if  tmp_yy_cp > yy_bp and then yy_ch_buf(tmp_yy_cp-1) = ASCII.LF  then
         yy_ch_buf(tmp_yy_cp-2) := ASCII.LF;
      end if;

      tmp_yy_cp := tmp_yy_cp - 1;
      yy_ch_buf(tmp_yy_cp) := c;

--  Note:  this code is the text of YY_DO_BEFORE_ACTION, only
--         here we get different yy_cp and yy_bp's
      yytext_ptr := yy_bp;
      yy_hold_char := yy_ch_buf(tmp_yy_cp);
      yy_ch_buf(tmp_yy_cp) := ASCII.NUL;
      yy_c_buf_p := tmp_yy_cp;
end yyunput;

procedure unput(c : character) is
begin
      yyunput( c, yy_bp );
end unput;

function input return character is
      c : character;
      yy_cp : constant integer := yy_c_buf_p;
begin
      yy_ch_buf(yy_cp) := yy_hold_char;

      if  yy_ch_buf(yy_c_buf_p) = YY_END_OF_BUFFER_CHAR  then
         -- need more input
         yytext_ptr := yy_c_buf_p;
         yy_c_buf_p := yy_c_buf_p + 1;

         case yy_get_next_buffer is
        -- this code, unfortunately, is somewhat redundant with
        -- that above

         when EOB_ACT_END_OF_FILE =>
            if  yywrap  then
            yy_c_buf_p := yytext_ptr;
            return ASCII.NUL;
            end if;

            yy_ch_buf(0) := ASCII.LF;
            yy_n_chars := 1;
            yy_ch_buf(yy_n_chars) := YY_END_OF_BUFFER_CHAR;
            yy_ch_buf(yy_n_chars + 1) := YY_END_OF_BUFFER_CHAR;
            yy_eof_has_been_seen := false;
            yy_c_buf_p := 1;
            yytext_ptr := yy_c_buf_p;
            yy_hold_char := yy_ch_buf(yy_c_buf_p);

            return ( input );
         when EOB_ACT_RESTART_SCAN =>
            yy_c_buf_p := yytext_ptr;

         when EOB_ACT_LAST_MATCH =>
            raise UNEXPECTED_LAST_MATCH;
         end case;
      end if;

      c := yy_ch_buf(yy_c_buf_p);
      yy_c_buf_p := yy_c_buf_p + 1;
      yy_hold_char := yy_ch_buf(yy_c_buf_p);

      return c;
end input;

procedure output(c : character) is
begin
      if Is_Open(user_output_file) then
      Ada.Text_IO.Put(user_output_file, c);
      else
      Ada.Text_IO.Put(c);
      end if;
end output;

-- default yywrap function - always treat EOF as an EOF
function yywrap return boolean is
begin
      return true;
end yywrap;

procedure Open_Input(fname : String) is
begin
      yy_init := true;
      Open(user_input_file, In_File, fname);
end Open_Input;

procedure Create_Output(fname : String := "") is
begin
      if fname /= "" then
         Create(user_output_file, Out_File, fname);
      end if;
end Create_Output;

procedure Close_Input is
begin
   if Is_Open(user_input_file) then
         Ada.Text_IO.Close(user_input_file);
   end if;
end Close_Input;

procedure Close_Output is
begin
   if Is_Open(user_output_file) then
         Ada.Text_IO.Close(user_output_file);
   end if;
end Close_Output;

-- UMASS CODES :
procedure Yy_Get_Token_Line ( Yy_Line_String : out String;
                              Yy_Line_Length : out Natural ) is
begin
  -- Currently processing line is either in saved token line1 or
  -- in saved token line2.
      if Yy_Line_Number = Line_Number_Of_Saved_Tok_Line1 then
         Yy_Line_Length := Saved_Tok_Line1.all'length;
         Yy_Line_String ( Yy_Line_String'First .. ( Yy_Line_String'First + Saved_Tok_Line1.all'length - 1 ))
      := Saved_Tok_Line1 ( 1 .. Saved_Tok_Line1.all'length );
      else
         Yy_Line_Length := Saved_Tok_Line2.all'length;
         Yy_Line_String ( Yy_Line_String'First .. ( Yy_Line_String'First + Saved_Tok_Line2.all'length - 1 ))
      := Saved_Tok_Line2 ( 1 .. Saved_Tok_Line2.all'length );
      end if;
end Yy_Get_Token_Line;

function Yy_Line_Number return Natural is
begin
   return Tok_Begin_Line;
end Yy_Line_Number;

function Yy_Begin_Column return Natural is
begin
   return Tok_Begin_Col;
end Yy_Begin_Column;

function Yy_End_Column return Natural is
begin
   return Tok_End_Col;
end Yy_End_Column;

-- END OF UMASS CODES.

end mast_results_lex_io;
