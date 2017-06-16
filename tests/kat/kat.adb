-------------------------------------------------------------------------------
-- Copyright (c) 2016, Daniel King
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * The name of the copyright holder may not be used to endorse or promote
--       Products derived from this software without specific prior written
--       permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
-- THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;
with Interfaces; use Interfaces;

package body KAT
is
   use type Interfaces.Unsigned_8;
   use type GNAT.Regpat.Match_Array;
   use type GNAT.Regpat.Match_Location;

   Rep_Regexp    : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("Repeat = (\d+)");
   Txt_Regexp    : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("Text = (\w+)");
   Len_Regexp    : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("Len = (\d+)");
   Msg_Regexp    : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("Msg = (\w+)");
   MD_Regexp     : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("MD = (\w+)");
   InLen_Regexp  : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("InLen = (\d+)");
   OutLen_Regexp : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("OutLen = (\d+)");
   In_Regexp     : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("In = (\w+)");
   Out_Regexp    : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("Out = (\w+)");
   N_Regexp      : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("N = ""(.*)""");
   S_Regexp      : GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile("S = ""(.*)""");
   
   
   ----------------------------------------------------------------------------
   -- Converts a hexadecimal string to a byte array.
   ----------------------------------------------------------------------------
   function Hex_String_To_Byte_Array(Str : in String) return Byte_Array_Access
   is
      function Char_To_Byte(C : in Character) return Keccak.Types.Byte
      is
         Byte : Keccak.Types.Byte;
      begin
         if C >= '0' and C <= '9' then
            Byte := Keccak.Types.Byte(Character'Pos(C) - Character'Pos('0'));
         
         elsif C >= 'a' and C <= 'f' then
            Byte := Keccak.Types.Byte(16#A# + (Character'Pos(C) - Character'Pos('a')));
         
         elsif C >= 'A' and C <= 'F' then
            Byte := Keccak.Types.Byte(16#A# + (Character'Pos(C) - Character'Pos('A')));
            
         else
            raise Constraint_Error;
         
         end if;
         
         return Byte;
      end Char_To_Byte;
   
      Byte_Array : Byte_Array_Access;
      I          : Natural := 0;
   begin
      if (Str'Length mod 2 /= 0) or (Str'Length = 0) then
         raise Constraint_Error;
      end if;
      
      Byte_Array := new Keccak.Types.Byte_Array(0 .. Str'Length/2 - 1);
      
      while I < Str'Length loop
         Byte_Array.all(I/2) := Interfaces.Shift_Left(Char_To_Byte(Str(Str'First + I)), 4)
                                or Char_To_Byte(Str(Str'First + I + 1));
      
         I := I + 2;
      end loop;
      
      return Byte_Array;
      
   end Hex_String_To_Byte_Array;
   
   
   
   function String_To_Byte_Array(Str : in String) return Byte_Array_Access
   is
      Byte_Array : Byte_Array_Access;
      
   begin
      Byte_Array := new Keccak.Types.Byte_Array(1 .. Str'Length);
      
      for I in Natural range 0 .. Str'Length - 1 loop
         Byte_Array(Byte_Array'First + I) := Keccak.Types.Byte(Character'Pos(Str(Str'First + I)));
      end loop;
      
      return Byte_Array;
   
   end String_To_Byte_Array;
   

   procedure Load_Test_Vectors(File_Name : in     String;
                               Tests     :    out KAT_Vectors.Vector;
                               Align_Bits : in     Boolean)
   is
      File      : Ada.Text_IO.File_Type;
      Curr_Line_Num : Natural := 1;
      
      Line_Num  : Natural;
      Len       : Natural;
      Repeat    : Natural;
      Msg       : Byte_Array_Access := null;
      MD        : Byte_Array_Access := null;
      
   begin
      Tests := KAT_Vectors.Empty_Vector;
   
      Ada.Text_IO.Open(File => File,
                       Mode => Ada.Text_IO.In_File,
                       Name => File_Name);
      
      while not Ada.Text_IO.End_Of_File(File) loop
         declare
            Line : String := Ada.Text_IO.Get_Line(File);
            
            Rep_Match : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            Txt_Match : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            Len_Match : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            Msg_Match : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            MD_Match  : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
         
         begin
            GNAT.Regpat.Match(Rep_Regexp, Line, Rep_Match);
            GNAT.Regpat.Match(Txt_Regexp, Line, Txt_Match);
            GNAT.Regpat.Match(Len_Regexp, Line, Len_Match);
            GNAT.Regpat.Match(Msg_Regexp, Line, Msg_Match);
            GNAT.Regpat.Match(MD_Regexp,  Line, MD_Match);
            
            -- Len = ??
            if Len_Match(0) /= GNAT.Regpat.No_Match then
               Line_Num := Curr_Line_Num;
               Len      := Natural'Value(Line(Len_Match(1).First .. Len_Match(1).Last));
               Repeat   := 1;
               
            -- Repeat = ??
            elsif Rep_Match(0) /= GNAT.Regpat.No_Match then
               Line_Num := Curr_Line_Num;
               Repeat   := Natural'Value(Line(Rep_Match(1).First .. Rep_Match(1).Last));
            
            -- Msg = ??
            elsif Msg_Match(0) /= GNAT.Regpat.No_Match then
               Msg := Hex_String_To_Byte_Array(Line(Msg_Match(1).First .. Msg_Match(1).Last));
            
            -- Text = ??
            elsif Txt_Match(0) /= GNAT.Regpat.No_Match then
               Msg := String_To_Byte_Array(Line(Txt_Match(1).First .. Txt_Match(1).Last));
               Len := Msg.all'Length * 8;
           
            -- MD = ??
            elsif MD_Match(0) /= GNAT.Regpat.No_Match then
               MD := Hex_String_To_Byte_Array(Line(MD_Match(1).First .. MD_Match(1).Last));
               
               declare
                  Curr_Test : KAT_Test;
               begin
                  if Align_Bits and Len mod 8 /= 0 and Msg.all'Length > 0 then
                     -- Align last byte on the least significant bit
                     Msg.all(Msg.all'Last) := Shift_Right(Msg.all(Msg.all'Last), 8 - (Len mod 8));
                  end if;
               
                  Curr_Test.Line   := Line_Num;
                  Curr_Test.Len    := Len;
                  Curr_Test.Repeat := Repeat;
                  Curr_Test.Msg    := Msg;
                  Curr_Test.MD     := MD;
                  KAT_Vectors.Append(Tests, Curr_Test);
               end;
               
            end if;
         end;
         
         Curr_Line_Num := Curr_Line_Num + 1;
      end loop;
   end Load_Test_Vectors;
   
   
   
   procedure Load_Duplex_Test_Vectors(File_Name : in     String;
                                      Tests     :    out Duplex_KAT_Vectors.Vector)
   is
      File          : Ada.Text_IO.File_Type;
      Curr_Line_Num : Natural := 1;
      
      Line_Num      : Natural;
      In_Len        : Natural;
      Out_Len       : Natural;
      In_Data       : Byte_Array_Access := null;
      Out_Data      : Byte_Array_Access := null;
      
   begin
      Tests := Duplex_KAT_Vectors.Empty_Vector;
   
      Ada.Text_IO.Open(File => File,
                       Mode => Ada.Text_IO.In_File,
                       Name => File_Name);
      
      while not Ada.Text_IO.End_Of_File(File) loop
         declare
            Line : String := Ada.Text_IO.Get_Line(File);
            
            InLen_Match  : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            In_Match     : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            OutLen_Match : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            Out_Match    : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
         
         begin
            GNAT.Regpat.Match(InLen_Regexp,  Line, InLen_Match);
            GNAT.Regpat.Match(In_Regexp,     Line, In_Match);
            GNAT.Regpat.Match(OutLen_Regexp, Line, OutLen_Match);
            GNAT.Regpat.Match(Out_Regexp,    Line, Out_Match);
            
            -- InLen = ??
            if InLen_Match(0) /= GNAT.Regpat.No_Match then
               Line_Num := Curr_Line_Num;
               In_Len   := Natural'Value(Line(InLen_Match(1).First .. InLen_Match(1).Last));
               
            -- In = ??
            elsif In_Match(0) /= GNAT.Regpat.No_Match then
               In_Data := Hex_String_To_Byte_Array(Line(In_Match(1).First .. In_Match(1).Last));
            
            -- OutLen = ??
            elsif OutLen_Match(0) /= GNAT.Regpat.No_Match then
               Out_Len := Natural'Value(Line(OutLen_Match(1).First .. OutLen_Match(1).Last));
           
            -- Out = ??
            elsif Out_Match(0) /= GNAT.Regpat.No_Match then
               Out_Data := Hex_String_To_Byte_Array(Line(Out_Match(1).First .. Out_Match(1).Last));
               
               declare
                  Curr_Test : Duplex_KAT_Test;
               begin
                  Curr_Test.Line     := Line_Num;
                  Curr_Test.In_Len   := In_Len;
                  Curr_Test.In_Data  := In_Data;
                  Curr_Test.Out_Len  := Out_Len;
                  Curr_Test.Out_Data := Out_Data;
                  Duplex_KAT_Vectors.Append(Tests, Curr_Test);
               end;
               
            end if;
         end;
         
         Curr_Line_Num := Curr_Line_Num + 1;
      end loop;
   end Load_Duplex_Test_Vectors;
   
   
   procedure Load_CSHAKE_Test_Vectors (File_Name : in     String;
                                       Tests     :    out CSHAKE_KAT_Vectors.Vector)
   is
      File          : Ada.Text_IO.File_Type;
      Curr_Line_Num : Natural := 1;
      
      Line_Num      : Natural;
      In_Len        : Natural;
      Out_Len       : Natural;
      In_Data       : Byte_Array_Access := null;
      Out_Data      : Byte_Array_Access := null;
      N_Data        : Unbounded_String;
      S_Data        : Unbounded_String;
      
   begin
      Tests := CSHAKE_KAT_Vectors.Empty_Vector;
   
      Ada.Text_IO.Open(File => File,
                       Mode => Ada.Text_IO.In_File,
                       Name => File_Name);
      
      while not Ada.Text_IO.End_Of_File(File) loop
         declare
            Line : String := Ada.Text_IO.Get_Line(File);
            
            InLen_Match  : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            In_Match     : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            OutLen_Match : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            Out_Match    : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            N_Match      : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
            S_Match      : GNAT.Regpat.Match_Array(0 .. 1) := (others => GNAT.Regpat.No_Match);
         
         begin
            GNAT.Regpat.Match(InLen_Regexp,  Line, InLen_Match);
            GNAT.Regpat.Match(In_Regexp,     Line, In_Match);
            GNAT.Regpat.Match(OutLen_Regexp, Line, OutLen_Match);
            GNAT.Regpat.Match(Out_Regexp,    Line, Out_Match);
            GNAT.Regpat.Match(N_Regexp,      Line, N_Match);
            GNAT.Regpat.Match(S_Regexp,      Line, S_Match);
               
            --  N = "??"
            if N_Match (0) /= GNAT.Regpat.No_Match then
               Line_Num := Curr_Line_Num;
               N_Data := To_Unbounded_String(Line(N_Match(1).First .. N_Match(1).Last));
               
            --  S = "??"
            elsif S_Match (0) /= GNAT.Regpat.No_Match then
               S_Data := To_Unbounded_String(Line(S_Match(1).First .. S_Match(1).Last));
            
            -- InLen = ??
            elsif InLen_Match(0) /= GNAT.Regpat.No_Match then
               In_Len   := Natural'Value(Line(InLen_Match(1).First .. InLen_Match(1).Last));
               
            -- In = ??
            elsif In_Match(0) /= GNAT.Regpat.No_Match then
               In_Data := Hex_String_To_Byte_Array(Line(In_Match(1).First .. In_Match(1).Last));
            
            -- OutLen = ??
            elsif OutLen_Match(0) /= GNAT.Regpat.No_Match then
               Out_Len := Natural'Value(Line(OutLen_Match(1).First .. OutLen_Match(1).Last));
           
            -- Out = ??
            elsif Out_Match(0) /= GNAT.Regpat.No_Match then
               Out_Data := Hex_String_To_Byte_Array(Line(Out_Match(1).First .. Out_Match(1).Last));
               
               declare
                  Curr_Test : CSHAKE_KAT_Test;
               begin
                  Curr_Test.Line     := Line_Num;
                  Curr_Test.N_Data   := N_Data;
                  Curr_Test.S_Data   := S_Data;
                  Curr_Test.In_Len   := In_Len;
                  Curr_Test.In_Data  := In_Data;
                  Curr_Test.Out_Len  := Out_Len;
                  Curr_Test.Out_Data := Out_Data;
                  CSHAKE_KAT_Vectors.Append(Tests, Curr_Test);
               end;
               
               In_Data  := null;
               Out_Data := null;
               
            end if;
         end;
         
         Curr_Line_Num := Curr_Line_Num + 1;
      end loop;
   end Load_CSHAKE_Test_Vectors;
   
   
   procedure Initialize(T : in out KAT_Test)
   is
   begin
      T.Line   := 0;
      T.Len    := 0;
      T.Repeat := 1;
      T.Msg    := null;
      T.MD     := null;
   end Initialize;
   
   
   procedure Adjust(T : in out KAT_Test)
   is
   begin
      if T.Msg /= null then
         T.Msg := new Keccak.Types.Byte_Array'(T.Msg.all);
      end if;
      
      if T.MD /= null then
         T.MD := new Keccak.Types.Byte_Array'(T.MD.all);
      end if;
   end Adjust;
   
   
   procedure Finalize(T : in out KAT_Test)
   is
      procedure Free is new Ada.Unchecked_Deallocation(Keccak.Types.Byte_Array,
                                                       Byte_Array_Access);
   begin
      if T.Msg /= null then
         Free(T.Msg);
         T.Msg := null;
      end if;
      
      if T.MD /= null then
         Free(T.MD);
         T.MD := null;
      end if;
   end Finalize;
   
   
   procedure Initialize(T : in out Duplex_KAT_Test)
   is
   begin
      T.Line     := 0;
      T.In_Len   := 0;
      T.In_Data  := null;
      T.Out_Len  := 0;
      T.Out_Data := null;
   end Initialize;
   
   
   procedure Adjust(T : in out Duplex_KAT_Test)
   is
   begin
      if T.In_Data /= null then
         T.In_Data := new Keccak.Types.Byte_Array'(T.In_Data.all);
      end if;
      
      if T.Out_Data/= null then
         T.Out_Data := new Keccak.Types.Byte_Array'(T.Out_Data.all);
      end if;
   end Adjust;
   
   
   procedure Finalize(T : in out Duplex_KAT_Test)
   is
      procedure Free is new Ada.Unchecked_Deallocation(Keccak.Types.Byte_Array,
                                                       Byte_Array_Access);
   begin
      if T.In_Data /= null then
         Free(T.In_Data);
         T.In_Data := null;
      end if;
      
      if T.Out_Data /= null then
         Free(T.Out_Data);
         T.Out_Data := null;
      end if;
   end Finalize;
   
   
   procedure Initialize(T : in out CSHAKE_KAT_Test)
   is
   begin
      T.Line     := 0;
      T.In_Len   := 0;
      T.In_Data  := null;
      T.Out_Len  := 0;
      T.Out_Data := null;
   end Initialize;
   
   
   procedure Adjust(T : in out CSHAKE_KAT_Test)
   is
   begin
      if T.In_Data /= null then
         T.In_Data := new Keccak.Types.Byte_Array'(T.In_Data.all);
      end if;
      
      if T.Out_Data/= null then
         T.Out_Data := new Keccak.Types.Byte_Array'(T.Out_Data.all);
      end if;
   end Adjust;
   
   
   procedure Finalize(T : in out CSHAKE_KAT_Test)
   is
      procedure Free is new Ada.Unchecked_Deallocation(Keccak.Types.Byte_Array,
                                                       Byte_Array_Access);
   begin
      if T.In_Data /= null then
         Free(T.In_Data);
         T.In_Data := null;
      end if;
      
      if T.Out_Data /= null then
         Free(T.Out_Data);
         T.Out_Data := null;
      end if;
   end Finalize;
   
   
   function Byte_Array_To_String(Data : in Keccak.Types.Byte_Array) return String
   is
      
      Hex_Characters : constant array(Keccak.Types.Byte range 0 .. 15) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7',
          '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      
      Str : String(1 .. Data'Length*2);
      I   : Natural := 0;
   begin
   
      while I < Data'Length loop
         Str(I*2 + 1)     := Hex_Characters(Shift_Right(Data(Data'First + I), 4));
         Str(I*2 + 2) := Hex_Characters(Data(Data'First + I) mod 16);
         
         I := I + 1;
      end loop;
      
      return Str;
   
   end Byte_Array_To_String;

end KAT;
