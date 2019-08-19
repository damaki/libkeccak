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
package body Gimli
with SPARK_Mode => On
is

   Round_Constant : constant Unsigned_32 := 16#9e37_7900#;

   ------------
   --  Swap  --
   ------------

   procedure Swap (A, B : in out Unsigned_32)
   is
      Temp : Unsigned_32;
   begin
      Temp := A;
      A := B;
      B := Temp;
   end Swap;

   --------------
   --  SP_Box  --
   --------------

   procedure SP_Box (S : in out State)
   is
      X : Unsigned_32;
      Y : Unsigned_32;
      Z : Unsigned_32;

   begin
      for Column in Column_Number loop
         X := Rotate_Left (S (Column, 0), 24);
         Y := Rotate_Left (S (Column, 1),  9);
         Z :=              S (Column, 2);

         S (Column, 2) := X xor Shift_Left (Z, 1) xor Shift_Left (Y and Z, 2);
         S (Column, 1) := Y xor X                 xor Shift_Left (X  or Z, 1);
         S (Column, 0) := Z xor Y                 xor Shift_Left (X and Y, 3);
      end loop;
   end SP_Box;

   ------------
   --  Init  --
   ------------

   procedure Init (S : out State)
   is
   begin
      S := (others => (others => 0));
   end Init;

   ---------------
   --  Permute  --
   ---------------

   procedure Permute (S : in out State)
   is
   begin
      for Round in reverse Round_Number loop
         SP_Box (S);

         --  Small swap: pattern s...s...s... etc.
         if Round mod 4 = 0 then
            Swap (S (0, 0), S (1, 0));
            Swap (S (2, 0), S (3, 0));
         end if;

         --  Big swap: pattern ..S...S...S etc.
         if Round mod 4 = 2 then
            Swap (S (0, 0), S (2, 0));
            Swap (S (1, 0), S (3, 0));
         end if;

         if Round mod 4 = 0 then
            S (0, 0) := S (0, 0) xor (Round_Constant or Unsigned_32 (Round));
         end if;
      end loop;
   end Permute;

   ---------------------------
   --  XOR_Bits_Into_State  --
   ---------------------------

   procedure XOR_Bits_Into_State (S       : in out State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
   is
      Remaining_Bits   : Natural := Bit_Len;
      Offset           : Natural := 0;

   begin
      --  Process whole words (32 bits).
      Outer_Loop :
      for Row in Row_Number loop
         pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
         pragma Loop_Invariant (Offset mod 4 = 0);
         pragma Loop_Invariant (Offset = Natural (Row) * 16);

         for Column in Column_Number loop
            pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
            pragma Loop_Invariant (Offset mod 4 = 0);
            pragma Loop_Invariant (Offset = (Natural (Row) * 16) +
                                            (Natural (Column) * 4));

            exit Outer_Loop when Remaining_Bits < 32;

            declare
               Word : Unsigned_32;
            begin
               Word := Unsigned_32 (Data (Data'First + Offset));
               Word := Word or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 1)), 8);
               Word := Word or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 2)), 16);
               Word := Word or Shift_Left (Unsigned_32 (Data (Data'First + Offset + 3)), 24);

               S (Column, Row) := S (Column, Row) xor Word;
            end;

            Offset          := Offset          + 4;
            Remaining_Bits  := Remaining_Bits  - 32;

         end loop;
      end loop Outer_Loop;

      --  Process any remaining data
      if Remaining_Bits > 0 then
         declare
            Column : constant Column_Number := Column_Number ((Bit_Len / 32) mod 4);
            Row    : constant Row_Number    := Row_Number    (Bit_Len / 128);

            Word            : Unsigned_32 := 0;
            Remaining_Bytes : constant Natural := (Remaining_Bits + 7) / 8;

         begin
            for I in Natural range 0 .. Remaining_Bytes - 1 loop
               Word := Word or Shift_Left (Unsigned_32 (Data (Data'First + Offset + I)), I * 8);
            end loop;

            S (Column, Row) := S (Column, Row) xor (Word and (2**Remaining_Bits) - 1);
         end;
      end if;
   end XOR_Bits_Into_State;

   --------------------
   --  Extract_Bits  --
   --------------------

   procedure Extract_Bytes (S    : in     State;
                            Data :    out Keccak.Types.Byte_Array)
   is
      Remaining : Natural := Data'Length;
      Offset    : Natural := 0;

      Pos       : Keccak.Types.Index_Number;

   begin
      --  Process whole words (32 bits).
      Outer_Loop :
      for Row in Row_Number loop
         pragma Loop_Invariant (Offset + Remaining = Data'Length);
         pragma Loop_Invariant (Offset mod 4 = 0);
         pragma Loop_Invariant (Offset = Natural (Row) * 16);

         for Column in Column_Number loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (Offset mod 4 = 0);
            pragma Loop_Invariant (Offset = (Natural (Row) * 16) +
                                            (Natural (Column) * 4));

            exit Outer_Loop when Remaining < 4;

            declare
               Word : constant Unsigned_32 := S (Column, Row);
            begin
               Pos := Data'First + Offset;

               Data (Pos)     := Unsigned_8 (Word and 16#FF#);
               Data (Pos + 1) := Unsigned_8 (Shift_Right (Word, 8) and 16#FF#);
               Data (Pos + 2) := Unsigned_8 (Shift_Right (Word, 16) and 16#FF#);
               Data (Pos + 3) := Unsigned_8 (Shift_Right (Word, 24) and 16#FF#);
            end;

            Offset    := Offset    + 4;
            Remaining := Remaining - 4;

         end loop;
      end loop Outer_Loop;

      pragma Assert (Remaining < 4);

      --  Process any remaining data
      if Remaining > 0 then
         declare
            Column : constant Column_Number := Column_Number ((Offset / 4) mod 4);
            Row    : constant Row_Number    := Row_Number    (Offset / 16);

            Word   : constant Unsigned_32 := S (Column, Row);

         begin
            for I in Natural range 0 .. Remaining - 1 loop
               pragma Loop_Invariant (Offset + (Remaining - I) = Data'Length);

               Data (Data'First + Offset) := Unsigned_8 (Shift_Right (Word, I * 8) and 16#FF#);

               Offset := Offset + 1;
            end loop;
         end;
      end if;
   end Extract_Bytes;

   --------------------
   --  Extract_Bits  --
   --------------------

   procedure Extract_Bits (A       : in     State;
                           Data    :    out Keccak.Types.Byte_Array;
                           Bit_Len : in     Natural)
   is
   begin
      Extract_Bytes (A, Data);

      --  Avoid exposing more bits than requested by masking away higher bits
      --  in the last byte.
      if Bit_Len > 0 and Bit_Len mod 8 /= 0 then
         Data (Data'Last) := Data (Data'Last) and (2**(Bit_Len mod 8) - 1);
      end if;
   end Extract_Bits;

end Gimli;
