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
package body Keccak.Generic_KeccakF.Byte_Lanes.Twisted is

   Twist : constant array (Y_Coord, X_Coord) of X_Coord :=
     (0 => (0 => 0,
            1 => 1,
            2 => 2,
            3 => 3,
            4 => 4),
      1 => (0 => 3,
            1 => 4,
            2 => 0,
            3 => 1,
            4 => 2),
      2 => (0 => 1,
            1 => 2,
            2 => 3,
            3 => 4,
            4 => 0),
      3 => (0 => 4,
            1 => 0,
            2 => 1,
            3 => 2,
            4 => 3),
      4 => (0 => 2,
            1 => 3,
            2 => 4,
            3 => 0,
            4 => 1));
   --  Twist mapping for the X coordinate.
   --
   --  The twisted Y coordinate is equal to the un-twisted X coordinate.

   -----------------------------------
   --  XOR_Bits_Into_State_Twisted  --
   -----------------------------------

   procedure XOR_Bits_Into_State_Twisted (A       : in out State;
                                          Data    : in     Keccak.Types.Byte_Array;
                                          Bit_Len : in     Natural)
   is
      Remaining_Bits   : Natural := Bit_Len;
      Offset           : Natural := 0;

      XT : X_Coord;
      YT : Y_Coord;

   begin
      --  Process whole lanes (64 bits).
      Outer_Loop :
      for Y in Y_Coord loop
         pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
         pragma Loop_Invariant (Offset mod (Lane_Size_Bits / 8) = 0);
         pragma Loop_Invariant (Offset = Natural (Y) * (Lane_Size_Bits / 8) * 5);

         for X in X_Coord loop
            pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
            pragma Loop_Invariant (Offset mod (Lane_Size_Bits / 8) = 0);
            pragma Loop_Invariant (Offset = (Natural (Y) * (Lane_Size_Bits / 8) * 5) +
                                            (Natural (X) * (Lane_Size_Bits / 8)));

            exit Outer_Loop when Remaining_Bits < Lane_Size_Bits;

            declare
               Lane : Lane_Type := 0;
            begin
               for I in Natural range 0 .. (Lane_Size_Bits / 8) - 1 loop
                  Lane := Lane or Shift_Left (Lane_Type (Data (Data'First + Offset + I)),
                                             I * 8);
               end loop;

               XT := Twist (Y, X);
               YT := Y_Coord (X);

               A (XT, YT) := A (XT, YT) xor Lane;
            end;

            Offset          := Offset          + Lane_Size_Bits / 8;
            Remaining_Bits  := Remaining_Bits  - Lane_Size_Bits;

         end loop;
      end loop Outer_Loop;

      --  Process any remaining data (smaller than 1 lane - 64 bits)
      if Remaining_Bits > 0 then
         declare
            X : constant X_Coord := X_Coord ((Bit_Len / Lane_Size_Bits) mod 5);
            Y : constant Y_Coord := Y_Coord ((Bit_Len / Lane_Size_Bits)  /  5);

            Word            : Lane_Type := 0;
            Remaining_Bytes : constant Natural := (Remaining_Bits + 7) / 8;

         begin
            XT := Twist (Y, X);
            YT := Y_Coord (X);

            for I in Natural range 0 .. Remaining_Bytes - 1 loop
               Word := Word or Shift_Left (Lane_Type (Data (Data'First + Offset + I)), I * 8);
            end loop;

            A (XT, YT) := A (XT, YT) xor (Word and (2**Remaining_Bits) - 1);
         end;
      end if;
   end XOR_Bits_Into_State_Twisted;

   -----------------------------------
   --  XOR_Bits_Into_State_Twisted  --
   -----------------------------------

   procedure XOR_Bits_Into_State_Twisted (A       : in out Lane_Complemented_State;
                                          Data    : in     Keccak.Types.Byte_Array;
                                          Bit_Len : in     Natural)
   is
   begin
      XOR_Bits_Into_State_Twisted
        (A       => State (A),
         Data    => Data,
         Bit_Len => Bit_Len);
   end XOR_Bits_Into_State_Twisted;

   -----------------------------------
   --  XOR_Byte_Into_State_Twisted  --
   -----------------------------------

   procedure XOR_Byte_Into_State_Twisted (A       : in out State;
                                          Offset  : in     Natural;
                                          Value   : in     Keccak.Types.Byte)
   is
      Lane_Size_Bytes : constant Positive := Lane_Size_Bits / 8;

      X : constant X_Coord := X_Coord ((Offset / Lane_Size_Bytes) mod 5);
      Y : constant Y_Coord := Y_Coord (Offset / (Lane_Size_Bytes * 5));

      XT : constant X_Coord := Twist (Y, X);
      YT : constant Y_Coord := Y_Coord (X);

   begin
      A (XT, YT) := A (XT, YT) xor Shift_Left (Lane_Type (Value),
                                               (Offset mod (Lane_Size_Bits / 8)) * 8);
   end XOR_Byte_Into_State_Twisted;

   ---------------------------
   --  XOR_Byte_Into_State_Twisted  --
   ---------------------------

   procedure XOR_Byte_Into_State_Twisted (A       : in out Lane_Complemented_State;
                                          Offset  : in     Natural;
                                          Value   : in     Keccak.Types.Byte)
   is
   begin
      XOR_Byte_Into_State_Twisted (State (A), Offset, Value);
   end XOR_Byte_Into_State_Twisted;

   -----------------------------
   --  Extract_Bytes_Twisted  --
   -----------------------------

   procedure Extract_Bytes_Twisted (A    : in     State;
                                    Data :    out Keccak.Types.Byte_Array)
   is
      X               : X_Coord := 0;
      Y               : Y_Coord := 0;

      XT              : X_Coord;
      YT              : Y_Coord;

      Remaining_Bytes : Natural := Data'Length;
      Offset          : Natural := 0;

      Lane            : Lane_Type;
   begin
      --  Case when each lane is at least 1 byte (i.e. 8, 16, 32, or 64 bits)

      --  Process whole lanes
      Outer_Loop :
      for Y2 in Y_Coord loop
         pragma Loop_Invariant (Offset mod (Lane_Size_Bits / 8) = 0
                                and Offset + Remaining_Bytes = Data'Length);

         for X2 in X_Coord loop
            pragma Loop_Variant (Increases => Offset,
                                 Decreases => Remaining_Bytes);
            pragma Loop_Invariant (Offset mod (Lane_Size_Bits / 8) = 0
                                   and Offset + Remaining_Bytes = Data'Length);

            XT := Twist (Y2, X2);
            YT := Y_Coord (X2);

            if Remaining_Bytes < Lane_Size_Bits / 8 then
               X := XT;
               Y := YT;
               exit Outer_Loop;
            end if;

            Lane := A (XT, YT);

            for I in Natural range 0 .. (Lane_Size_Bits / 8) - 1 loop
               Data (Data'First + Offset + I)
               := Keccak.Types.Byte (Shift_Right (Lane, I * 8) and 16#FF#);

               pragma Annotate (GNATprove, False_Positive,
                              """Data"" might not be initialized",
                              "Data is initialized at end of procedure");
            end loop;

            Remaining_Bytes := Remaining_Bytes - Lane_Size_Bits / 8;
            Offset          := Offset + Lane_Size_Bits / 8;
         end loop;
      end loop Outer_Loop;

      --  Process any remaining data (smaller than 1 lane)
      if Remaining_Bytes > 0 then
         Lane := A (X, Y);

         declare
            Initial_Offset : constant Natural := Offset with Ghost;
            Shift          :          Natural := 0;

         begin
            while Remaining_Bytes > 0 loop
               pragma Loop_Variant (Increases => Offset,
                                    Increases => Shift,
                                    Decreases => Remaining_Bytes);
               pragma Loop_Invariant (Offset + Remaining_Bytes = Data'Length
                                      and Shift mod 8 = 0
                                      and Shift = (Offset - Initial_Offset) * 8);

               Data (Data'First + Offset)
                 := Keccak.Types.Byte (Shift_Right (Lane, Shift) and 16#FF#);

               pragma Annotate (GNATprove, False_Positive,
                                """Data"" might not be initialized",
                                "Data is initialized at end of procedure");

               Shift           := Shift + 8;
               Offset          := Offset + 1;
               Remaining_Bytes := Remaining_Bytes - 1;
            end loop;
         end;
      end if;

   end Extract_Bytes_Twisted;

   -----------------------------
   --  Extract_Bytes_Twisted  --
   -----------------------------

   procedure Extract_Bytes_Twisted (A    : in     Lane_Complemented_State;
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
      --  Some lanes need to be complemented (bitwise NOT) when reading them
      --  from the Keccak-f state. We do this by storing a mask of all 1's
      --  for those lanes that need to be complemented (and all 0's for the
      --  other lanes). We then XOR against the corresponding entry in this
      --  Complement_Mask to complement only the required lanes (as XOR'ing
      --  against all 1's has the same effect as bitwise NOT).

      X               : X_Coord := 0;
      Y               : Y_Coord := 0;

      XT              : X_Coord;
      YT              : Y_Coord;

      Remaining_Bytes : Natural := Data'Length;
      Offset          : Natural := 0;

      Lane            : Lane_Type;
   begin
      --  Case when each lane is at least 1 byte (i.e. 8, 16, 32, or 64 bits)

      --  Process whole lanes
      Outer_Loop :
      for Y2 in Y_Coord loop
         pragma Loop_Invariant (Offset mod (Lane_Size_Bits / 8) = 0
                                and Offset + Remaining_Bytes = Data'Length);

         for X2 in X_Coord loop
            pragma Loop_Variant (Increases => Offset,
                                 Decreases => Remaining_Bytes);
            pragma Loop_Invariant (Offset mod (Lane_Size_Bits / 8) = 0
                                   and Offset + Remaining_Bytes = Data'Length);

            XT := Twist (Y2, X2);
            YT := Y_Coord (X2);

            if Remaining_Bytes < Lane_Size_Bits / 8 then
               X := XT;
               Y := YT;
               exit Outer_Loop;
            end if;

            Lane := A (XT, YT) xor Complement_Mask (XT, YT);

            for I in Natural range 0 .. (Lane_Size_Bits / 8) - 1 loop
               Data (Data'First + Offset + I)
               := Keccak.Types.Byte (Shift_Right (Lane, I * 8) and 16#FF#);

               pragma Annotate (GNATprove, False_Positive,
                              """Data"" might not be initialized",
                              "Data is initialized at end of procedure");
            end loop;

            Remaining_Bytes := Remaining_Bytes - Lane_Size_Bits / 8;
            Offset          := Offset + Lane_Size_Bits / 8;
         end loop;
      end loop Outer_Loop;

      --  Process any remaining data (smaller than 1 lane)
      if Remaining_Bytes > 0 then
         Lane := A (X, Y) xor Complement_Mask (X, Y);

         declare
            Initial_Offset : constant Natural := Offset with Ghost;
            Shift          :          Natural := 0;

         begin
            while Remaining_Bytes > 0 loop
               pragma Loop_Variant (Increases => Offset,
                                    Increases => Shift,
                                    Decreases => Remaining_Bytes);
               pragma Loop_Invariant (Offset + Remaining_Bytes = Data'Length
                                      and Shift mod 8 = 0
                                      and Shift = (Offset - Initial_Offset) * 8);

               Data (Data'First + Offset)
                 := Keccak.Types.Byte (Shift_Right (Lane, Shift) and 16#FF#);

               pragma Annotate (GNATprove, False_Positive,
                                """Data"" might not be initialized",
                                "Data is initialized at end of procedure");

               Shift           := Shift + 8;
               Offset          := Offset + 1;
               Remaining_Bytes := Remaining_Bytes - 1;
            end loop;
         end;
      end if;

   end Extract_Bytes_Twisted;

   --------------------
   --  Extract_Bits  --
   --------------------

   procedure Extract_Bits_Twisted (A       : in     State;
                                   Data    :    out Keccak.Types.Byte_Array;
                                   Bit_Len : in     Natural)
   is
      use type Keccak.Types.Byte;

   begin
      Extract_Bytes_Twisted (A, Data);

      --  Avoid exposing more bits than requested by masking away higher bits
      --  in the last byte.
      if Bit_Len > 0 and Bit_Len mod 8 /= 0 then
         Data (Data'Last) := Data (Data'Last) and (2**(Bit_Len mod 8) - 1);
      end if;
   end Extract_Bits_Twisted;

   --------------------
   --  Extract_Bits  --
   --------------------

   procedure Extract_Bits_Twisted (A       : in     Lane_Complemented_State;
                                   Data    :    out Keccak.Types.Byte_Array;
                                   Bit_Len : in     Natural)
   is
      use type Keccak.Types.Byte;

   begin
      Extract_Bytes_Twisted (A, Data);

      --  Avoid exposing more bits than requested by masking away higher bits
      --  in the last byte.
      if Bit_Len > 0 and Bit_Len mod 8 /= 0 then
         Data (Data'Last) := Data (Data'Last) and (2**(Bit_Len mod 8) - 1);
      end if;
   end Extract_Bits_Twisted;

end Keccak.Generic_KeccakF.Byte_Lanes.Twisted;
