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
with Interfaces; use Interfaces;

package body Keccak.Generic_KeccakF.Bit_Lanes
is

   ---------------------------
   --  XOR_Bits_Into_State  --
   ---------------------------

   procedure XOR_Bits_Into_State (A       : in out State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
   is
      X                : X_Coord := 0;
      Y                : Y_Coord := 0;

      Remaining_Bits   : Natural := Bit_Len;
      Offset           : Natural := 0;

      Initial_Byte_Len : constant Natural := (Bit_Len + 7) / 8 with Ghost;

   begin
      while Remaining_Bits >= 8 loop
         pragma Loop_Variant (Increases => Offset,
                              Decreases => Remaining_Bits);
         pragma Loop_Invariant (Offset + ((Remaining_Bits + 7) / 8) = Initial_Byte_Len);

         declare
            Byte : constant Keccak.Types.Byte := Data (Data'First + Offset);
            Lane : Lane_Type;

         begin
            for I in Natural range 0 .. (8 / Lane_Size_Bits) - 1 loop
               Lane := Lane_Type (Shift_Right (Byte,
                                               I * Lane_Size_Bits) and (2**Lane_Size_Bits - 1));

               A (X, Y) := A (X, Y) xor Lane;

               X := X + 1;
               if X = 0 then
                  Y := Y + 1;
               end if;
            end loop;
         end;

         Remaining_Bits := Remaining_Bits - 8;
         Offset         := Offset         + 1;
      end loop;

      if Remaining_Bits > 0 then
         declare
            Byte : Keccak.Types.Byte;
            Lane : Lane_Type;
         begin
            Byte := Data (Data'First + Offset) and (2**Remaining_Bits - 1);

            for I in Natural range 0 .. (8 / Lane_Size_Bits) - 1 loop
               Lane := Lane_Type (Shift_Right (Byte,
                                               I * Lane_Size_Bits) and (2**Lane_Size_Bits - 1));

               A (X, Y) := A (X, Y) xor Lane;

               X := X + 1;
               if X = 0 then
                  Y := Y + 1;
               end if;

               exit when X = 0 and Y = 0;
            end loop;
         end;
      end if;
   end XOR_Bits_Into_State;

   ---------------------------
   --  XOR_Bits_Into_State  --
   ---------------------------

   procedure XOR_Bits_Into_State (A       : in out Lane_Complemented_State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
   is
   begin
      XOR_Bits_Into_State
        (A       => State (A),
         Data    => Data,
         Bit_Len => Bit_Len);
   end XOR_Bits_Into_State;

   ---------------------
   --  Extract_Bytes  --
   ---------------------

   procedure Extract_Bytes (A    : in     State;
                            Data :    out Keccak.Types.Byte_Array)
   is
      X               : X_Coord := 0;
      Y               : Y_Coord := 0;

      Remaining_Bytes : Natural := Data'Length;
      Offset          : Natural := 0;

      Byte            : Keccak.Types.Byte;
   begin
      Data := (others => 0); --  workaround for flow analysis.

      --  Process entire bytes
      while Remaining_Bytes > 0 and Offset < State_Size_Bits / 8 loop
         pragma Loop_Variant (Increases => Offset,
                              Decreases => Remaining_Bytes);
         pragma Loop_Invariant (Offset + Remaining_Bytes = Data'Length);

         Byte := 0;

         for I in Natural range 0 .. (8 / Lane_Size_Bits) - 1 loop
            Byte := Byte or Shift_Left (Keccak.Types.Byte (A (X, Y)),
                                        I * Lane_Size_Bits);

            X := X + 1;
            if X = 0 then
               Y := Y + 1;
            end if;
         end loop;

         Data (Data'First + Offset) := Byte;

         Remaining_Bytes := Remaining_Bytes - 1;
         Offset          := Offset          + 1;
      end loop;

      if Remaining_Bytes > 0 then
         pragma Assert (Remaining_Bytes = 1);

         Byte := 0;

         for I in Natural range 0 .. (8 / Lane_Size_Bits) - 1 loop
            Byte := Byte or Shift_Left (Keccak.Types.Byte (A (X, Y)),
                                        I * Lane_Size_Bits);

            X := X + 1;
            if X = 0 then
               Y := Y + 1;
            end if;

            exit when X = 0 and Y = 0;
         end loop;

         Data (Data'First + Offset) := Byte;
      end if;

   end Extract_Bytes;

   ---------------------
   --  Extract_Bytes  --
   ---------------------

   procedure Extract_Bytes (A    : in     Lane_Complemented_State;
                            Data :    out Keccak.Types.Byte_Array)
   is
      Complement_Mask : constant Lane_Complemented_State :=
        (0 => (4         => Lane_Type'Last,
               others    => 0),
         1 => (0         => Lane_Type'Last,
               others    => 0),
         2 => (0 | 2 | 3 => Lane_Type'Last,
               others    => 0),
         3 => (1         => Lane_Type'Last,
               others    => 0),
         4 => (others    => 0));

      X               : X_Coord := 0;
      Y               : Y_Coord := 0;

      Remaining_Bytes : Natural := Data'Length;
      Offset          : Natural := 0;

      Byte            : Keccak.Types.Byte;
      Lane            : Lane_Type;
   begin
      Data := (others => 0); --  workaround for flow analysis.

      --  Process entire bytes
      while Remaining_Bytes > 0 and Offset < State_Size_Bits / 8 loop
         pragma Loop_Variant (Increases => Offset,
                              Decreases => Remaining_Bytes);
         pragma Loop_Invariant (Offset + Remaining_Bytes = Data'Length);

         Byte := 0;

         for I in Natural range 0 .. (8 / Lane_Size_Bits) - 1 loop
            Lane := A (X, Y) xor Complement_Mask (X, Y);
            Byte := Byte or Shift_Left (Keccak.Types.Byte (Lane), I * Lane_Size_Bits);

            X := X + 1;
            if X = 0 then
               Y := Y + 1;
            end if;
         end loop;

         Data (Data'First + Offset) := Byte;

         Remaining_Bytes := Remaining_Bytes - 1;
         Offset          := Offset          + 1;
      end loop;

      if Remaining_Bytes > 0 then
         pragma Assert (Remaining_Bytes = 1);

         Byte := 0;

         for I in Natural range 0 .. (8 / Lane_Size_Bits) - 1 loop
            Lane := A (X, Y) xor Complement_Mask (X, Y);
            Byte := Byte or Shift_Left (Keccak.Types.Byte (Lane), I * Lane_Size_Bits);

            X := X + 1;
            if X = 0 then
               Y := Y + 1;
            end if;

            exit when X = 0 and Y = 0;
         end loop;

         Data (Data'First + Offset) := Byte;
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
   --  Extract_Bits  --
   --------------------

   procedure Extract_Bits (A       : in     Lane_Complemented_State;
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

end Keccak.Generic_KeccakF.Bit_Lanes;
