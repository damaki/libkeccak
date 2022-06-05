-------------------------------------------------------------------------------
--  Copyright (c) 2019, Daniel King
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--      * Redistributions of source code must retain the above copyright
--        notice, this list of conditions and the following disclaimer.
--      * Redistributions in binary form must reproduce the above copyright
--        notice, this list of conditions and the following disclaimer in the
--        documentation and/or other materials provided with the distribution.
--      * The name of the copyright holder may not be used to endorse or promote
--        Products derived from this software without specific prior written
--        permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
--  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
--  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Maps;       use Ada.Strings.Maps;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with GNAT.Regpat;

package body Test_Vectors is

   use type GNAT.Regpat.Match_Location;

   function Create_List (Value : in Value_Choice) return Value_Choice_Lists.List;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Object : in out Value_Choice) is
   begin
      case Object.VType is
         when String_Type =>
            Object.Str := Null_Unbounded_String;

         when Integer_Type =>
            Object.Int := 0;

         when Hex_Array_Type =>
            Object.Hex := null;
      end case;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding
   procedure Adjust (Object : in out Value_Choice) is
   begin
      if Object.VType = Hex_Array_Type and then Object.Hex /= null then
         Object.Hex := new Keccak.Types.Byte_Array'(Object.Hex.all);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Object : in out Value_Choice) is
      procedure Free is new Ada.Unchecked_Deallocation (Keccak.Types.Byte_Array,
                                                        Byte_Array_Access);

   begin
      if Object.VType = Hex_Array_Type and then Object.Hex /= null then
         Free (Object.Hex);
      end if;
   end Finalize;

   ------------------------------
   -- Hex_String_To_Byte_Array --
   ------------------------------

   function Hex_String_To_Byte_Array (Str : in String) return Byte_Array_Access
   is
      function Char_To_Byte (C : in Character) return Keccak.Types.Byte;

      function Char_To_Byte (C : in Character) return Keccak.Types.Byte
      is
         Byte : Keccak.Types.Byte;
      begin
         if C >= '0' and C <= '9' then
            Byte := Keccak.Types.Byte (Character'Pos (C) - Character'Pos ('0'));

         elsif C >= 'a' and C <= 'f' then
            Byte := Keccak.Types.Byte (16#A# + (Character'Pos (C) - Character'Pos ('a')));

         elsif C >= 'A' and C <= 'F' then
            Byte := Keccak.Types.Byte (16#A# + (Character'Pos (C) - Character'Pos ('A')));

         else
            raise Constraint_Error;

         end if;

         return Byte;
      end Char_To_Byte;

      Byte_Array : Byte_Array_Access;
      I          : Natural := 0;
   begin
      if (Str'Length mod 2 /= 0) then
         raise Constraint_Error;
      end if;

      Byte_Array := new Keccak.Types.Byte_Array (0 .. Str'Length / 2 - 1);

      while I < Str'Length loop
         Byte_Array.all (I / 2) := Shift_Left (Char_To_Byte (Str (Str'First + I)), 4)
                                   or Char_To_Byte (Str (Str'First + I + 1));

         I := I + 2;
      end loop;

      return Byte_Array;

   end Hex_String_To_Byte_Array;

   --------------------------
   -- String_To_Byte_Array --
   --------------------------

   function String_To_Byte_Array (Str : in String) return Byte_Array_Access
   is
      Byte_Array : Byte_Array_Access;

   begin
      Byte_Array := new Keccak.Types.Byte_Array (1 .. Str'Length);

      for I in Natural range 0 .. Str'Length - 1 loop
         Byte_Array (Byte_Array'First + I) :=
           Keccak.Types.Byte (Character'Pos (Str (Str'First + I)));
      end loop;

      return Byte_Array;

   end String_To_Byte_Array;

   --------------------------
   -- Byte_Array_To_String --
   --------------------------

   function Byte_Array_To_String (Data : in Keccak.Types.Byte_Array) return String
   is

      Hex_Characters : constant array (Keccak.Types.Byte range 0 .. 15) of Character :=
         ('0', '1', '2', '3', '4', '5', '6', '7',
          '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

      Str : String (1 .. Data'Length * 2);
      I   : Natural := 0;
   begin

      while I < Data'Length loop
         Str (I * 2 + 1)     := Hex_Characters (Shift_Right (Data (Data'First + I), 4));
         Str (I * 2 + 2) := Hex_Characters (Data (Data'First + I) mod 16);

         I := I + 1;
      end loop;

      return Str;

   end Byte_Array_To_String;

   -----------------
   -- Create_List --
   -----------------

   function Create_List (Value : in Value_Choice) return Value_Choice_Lists.List is
   begin
      return L : Value_Choice_Lists.List do
         L := Value_Choice_Lists.Empty_List;
         L.Append (Value);
      end return;
   end Create_List;

   ----------
   -- Load --
   ----------

   Pair_Regex       : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("^(\w+)\s*=\s*""?([^""]*)""?$");

   Empty_Line_Regex : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("^\s*$");

   procedure Load (File_Name    : in     String;
                   Schema       : in     Schema_Maps.Map;
                   Vectors_List :    out Lists.List) is

      File : Ada.Text_IO.File_Type;

      Test_Vector : Test_Vector_Maps.Map := Test_Vector_Maps.Empty_Map;

   begin

      Vectors_List := Lists.Empty_List;

      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => File_Name);

      while not Ada.Text_IO.End_Of_File (File) loop
         declare
            --  Read a line and trim whitespace.
            Line : constant String := Trim (Source => Ada.Text_IO.Get_Line (File),
                                            Left   => To_Set (" " & CR),
                                            Right  => To_Set (" " & CR));
         begin
            Parse_Line (Test_Vector, Vectors_List, Schema, Line);
         end;
      end loop;

      Ada.Text_IO.Close (File);

      Append_Test_Vector (Test_Vector, Vectors_List, Schema);
   end Load;

   ----------------
   -- Parse_Line --
   ----------------

   procedure Parse_Line (Test_Vector  : in out Test_Vector_Maps.Map;
                         Vectors_List : in out Lists.List;
                         Schema       : in     Schema_Maps.Map;
                         Line         : in     String)
   is
      Pair_Match       : GNAT.Regpat.Match_Array (0 .. 2) := (others => GNAT.Regpat.No_Match);
      Empty_Line_Match : GNAT.Regpat.Match_Array (0 .. 0) := (others => GNAT.Regpat.No_Match);

      Key_First : Natural;
      Key_Last  : Natural;
      Key       : Unbounded_String;

      Value_First : Natural;
      Value_Last  : Natural;
      Value       : Unbounded_String;
   begin

      GNAT.Regpat.Match (Pair_Regex,       Line, Pair_Match);
      GNAT.Regpat.Match (Empty_Line_Regex, Line, Empty_Line_Match);

      if Empty_Line_Match (0) /= GNAT.Regpat.No_Match then
         --  Found an empty line. This signals the end of the current test vector entry.

         Append_Test_Vector (Test_Vector, Vectors_List, Schema);

         Test_Vector_Maps.Clear (Test_Vector);

      elsif Pair_Match (0) /= GNAT.Regpat.No_Match then
         Key_First := Pair_Match (1).First;
         Key_Last  := Pair_Match (1).Last;
         Key := To_Unbounded_String (Line (Key_First .. Key_Last));

         if Pair_Match (2) = GNAT.Regpat.No_Match then
            Value_First := Line'First;
            Value_Last  := Line'First - 1;
         else
            Value_First := Pair_Match (2).First;
            Value_Last  := Pair_Match (2).Last;
         end if;
         Value := To_Unbounded_String (Line (Value_First .. Value_Last));

         Add_Test_Vector_Key_Value_Pair (Test_Vector, Vectors_List, Schema, Key, Value);

      end if;
   end Parse_Line;

   ------------------------------------
   -- Add_Test_Vector_Key_Value_Pair --
   ------------------------------------

   procedure Add_Test_Vector_Key_Value_Pair (Test_Vector  : in out Test_Vector_Maps.Map;
                                             Vectors_List : in out Lists.List;
                                             Schema       : in     Schema_Maps.Map;
                                             Key          : in     Unbounded_String;
                                             Value        : in     Unbounded_String)
   is
      procedure Update_List (Key  : in     Unbounded_String;
                             List : in out Value_Choice_Lists.List);

      procedure Update_List (Key    : in     Unbounded_String;
                             List   : in out Value_Choice_Lists.List) is
      begin
         case Schema.Element (Key).VType is
            when String_Type =>
               List.Append
                 (Value_Choice'(Controlled with
                  VType => String_Type,
                  Str   => Value));

            when Integer_Type =>
               List.Append
                 (Value_Choice'(Controlled with
                  VType => Integer_Type,
                  Int   => Integer'Value (To_String (Value))));

            when Hex_Array_Type =>
               List.Append
                 (Value_Choice'(Controlled with
                  VType => Hex_Array_Type,
                  Hex   => Hex_String_To_Byte_Array (To_String (Value))));

         end case;
      end Update_List;

   begin
      if Schema.Contains (Key) then

         --  Some test vector files do not have blank lines separating
         --  each test vector, so if we find a key that we have already
         --  put into the current test vector then we have found the
         --  start of the next test vector.
         if Test_Vector.Contains (Key) and not Schema.Element (Key).Is_List then
            Append_Test_Vector (Test_Vector, Vectors_List, Schema);
         end if;

         if Test_Vector.Contains (Key) then

            Test_Vector.Update_Element (Position => Test_Vector.Find (Key),
                                        Process  => Update_List'Access);

         else
            case Schema.Element (Key).VType is
               when String_Type =>
                  Test_Vector.Insert
                    (Key      => Key,
                     New_Item => Create_List (Value_Choice'(Controlled with
                       VType => String_Type,
                       Str   => Value)));

               when Integer_Type =>
                  Test_Vector.Insert
                    (Key      => Key,
                     New_Item => Create_List (Value_Choice'(Controlled with
                       VType => Integer_Type,
                       Int   => Integer'Value (To_String (Value)))));

               when Hex_Array_Type =>
                  Test_Vector.Insert
                    (Key      => Key,
                     New_Item => Create_List (Value_Choice'(Controlled with
                       VType => Hex_Array_Type,
                       Hex   => Hex_String_To_Byte_Array (To_String (Value)))));

            end case;
         end if;
      else
         raise Schema_Error with "Unknown key";
      end if;
   end Add_Test_Vector_Key_Value_Pair;

   ------------------------
   -- Append_Test_Vector --
   ------------------------

   procedure Append_Test_Vector (Test_Vector  : in out Test_Vector_Maps.Map;
                                 Vectors_List : in out Lists.List;
                                 Schema       : in     Schema_Maps.Map)
   is
   begin
      if not Test_Vector_Maps.Is_Empty (Test_Vector) then

         --  Check if there are any missing required schema values
         for C in Schema.Iterate loop
            if Schema_Maps.Element (C).Required and
              not Test_Vector.Contains (Schema_Maps.Key (C))
            then
               raise Schema_Error with "Missing required key";
            end if;
         end loop;

         Lists.Append (Vectors_List, Test_Vector);
      end if;
   end Append_Test_Vector;

end Test_Vectors;
