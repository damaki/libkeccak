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
package body Ascon
with SPARK_Mode => On
is

   Round_Constants : constant array (Round_Number) of Unsigned_64 :=
     (0  => 16#0000_0000_0000_00f0#,
      1  => 16#0000_0000_0000_00e1#,
      2  => 16#0000_0000_0000_00d2#,
      3  => 16#0000_0000_0000_00c3#,
      4  => 16#0000_0000_0000_00b4#,
      5  => 16#0000_0000_0000_00a5#,
      6  => 16#0000_0000_0000_0096#,
      7  => 16#0000_0000_0000_0087#,
      8  => 16#0000_0000_0000_0078#,
      9  => 16#0000_0000_0000_0069#,
      10 => 16#0000_0000_0000_005a#,
      11 => 16#0000_0000_0000_004b#);

   ------------
   --  Init  --
   ------------

   procedure Init (S : out State)
   is
   begin
      S := (others => 0);
   end Init;

   ---------------
   --  Permute  --
   ---------------

   procedure Permute (S : in out State)
   is
      First_Round : constant Round_Number := Round_Number (Round_Count'Last - Num_Rounds);
   begin
      for Round in First_Round .. Round_Number'Last loop
         Add_Constant     (S, Round);
         Substitution     (S);
         Linear_Diffusion (S);
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
      --  Process whole words (64 bits).
      for X in X_Coord loop
         pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
         pragma Loop_Invariant (Offset mod 8 = 0);
         pragma Loop_Invariant (Offset = Natural (X) * 8);

         exit when Remaining_Bits < 64;

         declare
            Word : Unsigned_64;
         begin
            Word := Shift_Left (Unsigned_64 (Data (Data'First + Offset)), 56);
            Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 1)), 48);
            Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 2)), 40);
            Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 3)), 32);
            Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 4)), 24);
            Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 5)), 16);
            Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 6)), 8);
            Word := Word or Unsigned_64 (Data (Data'First + Offset + 7));

            S (X) := S (X) xor Word;
         end;

         Offset         := Offset         + 8;
         Remaining_Bits := Remaining_Bits - 64;
      end loop;

      --  Process any remaining data
      if Remaining_Bits > 0 then
         declare
            X : constant X_Coord := X_Coord (Bit_Len / 64);

            Word            : Unsigned_64 := 0;
            Remaining_Bytes : constant Natural := (Remaining_Bits + 7) / 8;

         begin
            for I in Natural range 0 .. Remaining_Bytes - 1 loop
               Word := Word or Shift_Left (Unsigned_64 (Data (Data'First + Offset + I)),
                                           56 - (I * 8));
            end loop;

            S (X) := S (X) xor (Word and (2**Remaining_Bits) - 1);
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
      for X in X_Coord loop
         pragma Loop_Invariant (Offset + Remaining = Data'Length);
         pragma Loop_Invariant (Offset mod 8 = 0);
         pragma Loop_Invariant (Offset = Natural (X) * 8);

         exit when Remaining < 8;

         declare
            Word : constant Unsigned_64 := S (X);
         begin
            Pos := Data'First + Offset;

            Data (Pos)     := Unsigned_8 (Shift_Right (Word, 56) and 16#FF#);
            Data (Pos + 1) := Unsigned_8 (Shift_Right (Word, 48) and 16#FF#);
            Data (Pos + 2) := Unsigned_8 (Shift_Right (Word, 40) and 16#FF#);
            Data (Pos + 3) := Unsigned_8 (Shift_Right (Word, 32) and 16#FF#);
            Data (Pos + 4) := Unsigned_8 (Shift_Right (Word, 24) and 16#FF#);
            Data (Pos + 5) := Unsigned_8 (Shift_Right (Word, 16) and 16#FF#);
            Data (Pos + 6) := Unsigned_8 (Shift_Right (Word,  8) and 16#FF#);
            Data (Pos + 7) := Unsigned_8 (Word and 16#FF#);
         end;

         Offset    := Offset    + 8;
         Remaining := Remaining - 8;
      end loop;

      pragma Assert (Remaining < 8);

      --  Process any remaining data
      if Remaining > 0 then
         declare
            X    : constant X_Coord := X_Coord (Offset / 8);
            Word : constant Unsigned_64 := S (X);

         begin
            for I in Natural range 0 .. Remaining - 1 loop
               pragma Loop_Invariant (Offset + (Remaining - I) = Data'Length);

               Data (Data'First + Offset) := Unsigned_8 (Shift_Right (Word, 56 - (I * 8))
                                                         and 16#FF#);

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

   --------------------
   --  Add_Constant  --
   --------------------

   procedure Add_Constant (S     : in out State;
                           Round : in     Round_Number)
   is
   begin
      S (2) := S (2) xor Round_Constants (Round);
   end Add_Constant;

   --------------------
   --  Substitution  --
   --------------------

   procedure Substitution (S : in out State)
   is
      T0, T1, T2, T3, T4 : Unsigned_64;

   begin
      --  This is the bitsliced implementation of the 5-bit S-box
      --  as described in Figure 5 of the Ascon v1.2 specification.

      --  x0 ^= x4;    x4 ^= x3;    x2 ^= x1;
      S (0) := S (0) xor S (4);
      S (4) := S (4) xor S (3);
      S (2) := S (2) xor S (1);

      --  t0  = x0;    t1  = x1;    t2  = x2;    t3  = x3;    t4  = x4;
      --  t0 =~ t0;    t1 =~ t1;    t2 =~ t2;    t3 =~ t3;    t4 =~ t4;
      T0 := not S (0);
      T1 := not S (1);
      T2 := not S (2);
      T3 := not S (3);
      T4 := not S (4);

      --  t0 &= x1;    t1 &= x2;    t2 &= x3;    t3 &= x4;    t4 &= x0;
      T0 := T0 and S (1);
      T1 := T1 and S (2);
      T2 := T2 and S (3);
      T3 := T3 and S (4);
      T4 := T4 and S (0);

      --  x0 ^= t1;    x1 ^= t2;    x2 ^= t3;    x3 ^= t4;    x4 ^= t0;
      S (0) := S (0) xor T1;
      S (1) := S (1) xor T2;
      S (2) := S (2) xor T3;
      S (3) := S (3) xor T4;
      S (4) := S (4) xor T0;

      --  x1 ^= x0;    x0 ^= x4;    x3 ^= x2;    x2 =~ x2;
      S (1) := S (1) xor S (0);
      S (0) := S (0) xor S (4);
      S (3) := S (3) xor S (2);
      S (2) := not S (2);
   end Substitution;

   ------------------------
   --  Linear_Diffusion  --
   ------------------------

   procedure Linear_Diffusion (S : in out State)
   is
   begin
      S (0) := S (0) xor Rotate_Right (S (0), 19) xor Rotate_Right (S (0), 28);
      S (1) := S (1) xor Rotate_Right (S (1), 61) xor Rotate_Right (S (1), 39);
      S (2) := S (2) xor Rotate_Right (S (2),  1) xor Rotate_Right (S (2),  6);
      S (3) := S (3) xor Rotate_Right (S (3), 10) xor Rotate_Right (S (3), 17);
      S (4) := S (4) xor Rotate_Right (S (4),  7) xor Rotate_Right (S (4), 41);
   end Linear_Diffusion;

end Ascon;
