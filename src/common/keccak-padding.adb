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

package body Keccak.Padding
with SPARK_Mode => On
is

   ---------------------------
   --  Pad101_Single_Block  --
   ---------------------------

   procedure Pad101_Single_Block (Block          : in out Keccak.Types.Byte_Array;
                                  Num_Used_Bits  : in     Natural;
                                  Max_Bit_Length : in     Natural)
   is
      Last_Bit : Keccak.Types.Byte;

   begin
      --  Append first 1 bit
      Block (Block'First + (Num_Used_Bits / 8))
        := (Block (Block'First + (Num_Used_Bits / 8)) and (2**(Num_Used_Bits mod 8) - 1))
        or Shift_Left (1, Num_Used_Bits mod 8);

      --  Append zeroes
      Block (Block'First + (Num_Used_Bits / 8) + 1 .. Block'Last) := (others => 0);

      --  Append last 1 bit
      if Max_Bit_Length mod 8 = 0 then
         Last_Bit := 2#1000_0000#;
      else
         Last_Bit := Shift_Right (2#1000_0000#, 8 - (Max_Bit_Length mod 8));
      end if;

      Block (Block'Last) := Block (Block'Last) or Last_Bit;
   end Pad101_Single_Block;

   ---------------------------
   --  Pad101_Multi_Blocks  --
   ---------------------------

   procedure Pad101_Multi_Blocks (First_Block    : in out Keccak.Types.Byte_Array;
                                  Num_Used_Bits  : in     Natural;
                                  Max_Bit_Length : in     Natural;
                                  Next_Block     :    out Keccak.Types.Byte_Array;
                                  Spilled        :    out Boolean)
   is
      Num_Free_Bits : constant Natural := Max_Bit_Length - Num_Used_Bits;

      Last_Bit  : Keccak.Types.Byte;
      First_Bit : Keccak.Types.Byte;

   begin
      Next_Block := (others => 0);

      if Max_Bit_Length mod 8 = 0 then
         Last_Bit := 2#1000_0000#;
      else
         Last_Bit := Shift_Right (2#1000_0000#, 8 - (Max_Bit_Length mod 8));
      end if;

      First_Bit := Shift_Left (1, Num_Used_Bits mod 8);

      if Num_Free_Bits >= 2 then
         --  This is the case where there are at least 2 bits free in the first
         --  block. In this case, there's enough space for all the padding bits.
         --
         --  +---------------------+---------------------+
         --  |    first block      |      next block     |
         --  +---------------------+---------------------+
         --  |<-total len->|<-pad->|

         Spilled := False;

         --  Append first 1 bit
         First_Block (First_Block'First + (Num_Used_Bits / 8))
           := (First_Block (First_Block'First + (Num_Used_Bits / 8)) and (First_Bit - 1))
           or First_Bit;

         --  Append zeroes
         First_Block (First_Block'First + (Num_Used_Bits / 8) + 1 .. First_Block'Last)
           := (others => 0);

         --  Append last 1 bit
         First_Block (First_Block'Last) := First_Block (First_Block'Last) or Last_Bit;

      else
         --  This is the case where there is only 1 bit free in the first block.
         --  In this case, the padding spills into another block.
         --
         --  +----------------+----------------+
         --  |  first block   |   next block   |
         --  +----------------+----------------+
         --  |<--total len-->|<-------pad----->|

         Spilled := True;

         pragma Assert (Num_Free_Bits = 1);

         --  First 1 bit
         First_Block (First_Block'Last)
           := (First_Block (First_Block'Last) and (First_Bit - 1)) or First_Bit;

         --  Next_Block is already padded with zeroes (see above).

         --  Append last 1 bit
         Next_Block (Next_Block'Last) := Last_Bit;

      end if;

   end Pad101_Multi_Blocks;

   -----------------------------
   --  XOR_Pad101_Into_State  --
   -----------------------------

   procedure XOR_Pad101_Into_State (State     : in out State_Type;
                                    First_Bit : in     Natural;
                                    Last_Bit  : in     Natural) is
   begin
      XOR_Byte_Into_State (State  => State,
                           Offset => First_Bit / 8,
                           Value  => Shift_Left (1, First_Bit mod 8));

      XOR_Byte_Into_State (State  => State,
                           Offset => Last_Bit / 8,
                           Value  => Shift_Left (1, Last_Bit mod 8));
   end XOR_Pad101_Into_State;

   -------------------------------------
   --  Pad10_Multi_Blocks_Big_Endian  --
   -------------------------------------

   procedure Pad10_Multi_Blocks_Big_Endian (First_Block    : in out Keccak.Types.Byte_Array;
                                            Num_Used_Bits  : in     Natural;
                                            Max_Bit_Length : in     Natural;
                                            Next_Block     :    out Keccak.Types.Byte_Array;
                                            Spilled        :    out Boolean)
   is
      Num_Free_Bits : constant Natural := Max_Bit_Length - Num_Used_Bits;

      Pos : Keccak.Types.Index_Number;

      First_Bit : Keccak.Types.Byte;

   begin
      Next_Block := (others => 0);

      First_Bit := Shift_Right (16#80#, Num_Used_Bits mod 8);

      if Num_Free_Bits >= 2 then
         --  This is the case where there are at least 2 bits free in the first
         --  block. In this case, there's enough space for all the padding bits.
         --
         --  +---------------------+---------------------+
         --  |    first block      |      next block     |
         --  +---------------------+---------------------+
         --  |<-total len->|<-pad->|

         Spilled := False;

         --  Append first 1 bit
         Pos := First_Block'First + (Num_Used_Bits / 8);

         if Num_Used_Bits mod 8 = 0 then
            First_Block (Pos) := First_Bit;
         else
            --  Align last bits to the MSB.
            --  E.g. move 2#0000_0011# --> 2#1100_0000#
            First_Block (Pos) := Shift_Left (First_Block (Pos), 8 - (Num_Used_Bits mod 8));
            First_Block (Pos) := First_Block (Pos) or First_Bit;
         end if;

         --  Append zeroes
         First_Block (First_Block'First + (Num_Used_Bits / 8) + 1 .. First_Block'Last)
           := (others => 0);

      else
         --  This is the case where there is only 1 bit free in the first block.
         --  In this case, the padding spills into another block.
         --
         --  +----------------+----------------+
         --  |  first block   |   next block   |
         --  +----------------+----------------+
         --  |<--total len-->|<-------pad----->|

         Spilled := True;

         pragma Assert (Num_Free_Bits = 1);

         --  First 1 bit
         Pos := First_Block'First + (Num_Used_Bits / 8);

         if Num_Used_Bits mod 8 = 0 then
            First_Block (Pos) := First_Bit;
         else
            --  Align last bits to the MSB.
            --  E.g. move 2#0000_0011# --> 2#1100_0000#
            First_Block (Pos) := Shift_Left (First_Block (Pos), 8 - (Num_Used_Bits mod 8));
            First_Block (Pos) := First_Block (Pos) or First_Bit;
         end if;

         --  Next_Block is already padded with zeroes (see above).

      end if;
   end Pad10_Multi_Blocks_Big_Endian;

end Keccak.Padding;
