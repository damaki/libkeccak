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
package body Keccak.Generic_KeccakF.Byte_Lanes
is

   procedure XOR_Bits_Into_State(A       : in out State;
                                 Data    : in     Keccak.Types.Byte_Array;
                                 Bit_Len : in     Natural)
   is
      use type Keccak.Types.Byte;

      Remaining_Bits   : Natural := Bit_Len;
      Offset           : Natural := 0;

   begin
      -- Process whole lanes (64 bits).
      Outer_Loop:
      for Y in Y_Coord loop
         pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
         pragma Loop_Invariant (Offset mod (W/8) = 0);
         pragma Loop_Invariant (Offset = Natural (Y) * (W/8) * 5);

         for X in X_Coord loop
            pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
            pragma Loop_Invariant (Offset mod (W/8) = 0);
            pragma Loop_Invariant (Offset = (Natural (Y) * (W/8) * 5) + (Natural (X) * (W/8)));

            exit Outer_Loop when Remaining_Bits < W;

            declare
               Lane : Lane_Type := 0;
            begin
               for I in Natural range 0 .. (W/8) - 1 loop
                  Lane := Lane or Shift_Left(Lane_Type(Data(Data'First + Offset + I)),
                                             I*8);
               end loop;

               A(X, Y) := A(X, Y) xor Lane;
            end;

            Offset          := Offset          + W/8;
            Remaining_Bits  := Remaining_Bits  - W;

         end loop;
      end loop Outer_Loop;

      -- Process any remaining data (smaller than 1 lane - 64 bits)
      if Remaining_Bits > 0 then
         declare
            X                : X_Coord   := X_Coord ((Bit_Len / W) mod 5);
            Y                : Y_Coord   := Y_Coord ((Bit_Len / W)  /  5);
            Word             : Lane_Type := 0;
            Remaining_Bytes  : Natural   := (Remaining_Bits + 7) / 8;

         begin
            for I in Natural range 0 .. Remaining_Bytes - 1 loop
               Word := Word or Shift_Left(Lane_Type(Data(Data'First + Offset + I)), I*8);
            end loop;

            A(X, Y) := A(X, Y) xor (Word and (2**Remaining_Bits) - 1);
         end;
      end if;
   end XOR_Bits_Into_State;



   procedure Extract_Bytes(A    : in     State;
                           Data :    out Keccak.Types.Byte_Array)
   is
      use type Keccak.Types.Byte;

      X               : X_Coord := 0;
      Y               : Y_Coord := 0;

      Remaining_Bytes : Natural := Data'Length;
      Offset          : Natural := 0;

      Lane            : Lane_Type;
   begin
      -- Case when each lane is at least 1 byte (i.e. 8, 16, 32, or 64 bits)

      -- Process whole lanes
      while Remaining_Bytes >= W/8 loop
         pragma Loop_Variant(Increases => Offset,
                             Decreases => Remaining_Bytes);
         pragma Loop_Invariant(Offset mod (W/8) = 0
                               and Offset + Remaining_Bytes = Data'Length);

         Lane := A(X, Y);

         for I in Natural range 0 .. (W/8) - 1 loop
            Data(Data'First + Offset + I)
              := Keccak.Types.Byte(Shift_Right(Lane, I*8) and 16#FF#);

            pragma Annotate (GNATprove, False_Positive,
                             """Data"" might not be initialized",
                             "Data is initialized at end of procedure");
         end loop;

         X := X + 1;
         if X = 0 then
            Y := Y + 1;
         end if;

         Remaining_Bytes := Remaining_Bytes - W/8;
         Offset          := Offset + W/8;
      end loop;

      -- Process any remaining data (smaller than 1 lane)
      if Remaining_Bytes > 0 then
         Lane := A(X, Y);

         declare
            Shift          : Natural := 0;
            Initial_Offset : Natural := Offset with Ghost;
         begin
            while Remaining_Bytes > 0 loop
               pragma Loop_Variant(Increases => Offset,
                                   Increases => Shift,
                                   Decreases => Remaining_Bytes);
               pragma Loop_Invariant(Offset + Remaining_Bytes = Data'Length
                                     and Shift mod 8 = 0
                                     and Shift = (Offset - Initial_Offset) * 8);

               Data(Data'First + Offset)
                 := Keccak.Types.Byte(Shift_Right(Lane, Shift) and 16#FF#);

               pragma Annotate (GNATprove, False_Positive,
                                """Data"" might not be initialized",
                                "Data is initialized at end of procedure");

               Shift           := Shift + 8;
               Offset          := Offset + 1;
               Remaining_Bytes := Remaining_Bytes - 1;
            end loop;
         end;
      end if;

   end Extract_Bytes;

   procedure Extract_Bits(A       : in     State;
                          Data    :    out Keccak.Types.Byte_Array;
                          Bit_Len : in     Natural)
   is
      use type Keccak.Types.Byte;

   begin
      Extract_Bytes(A, Data);

      -- Avoid exposing more bits than requested by masking away higher bits
      -- in the last byte.
      if Bit_Len > 0 and Bit_Len mod 8 /= 0 then
         Data(Data'Last) := Data(Data'Last) and (2**(Bit_Len mod 8) - 1);
      end if;
   end Extract_Bits;

end Keccak.Generic_KeccakF.Byte_Lanes;
