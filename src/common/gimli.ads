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
with Interfaces;   use Interfaces;
with Keccak.Types;

--  @summary
--  Implementation of the Gimli permutation.
package Gimli
with SPARK_Mode => On
is

   State_Size_Bits : constant := 384;
   --  The Gimli state size, in bits.

   type State is private;
   --  The Gimli state.

   procedure Init (S : out State)
     with Global => null,
     Depends => (S => null);
   --  Initialise the Gimli state to all zeroes.

   procedure Permute (S : in out State)
     with Global => null,
     Depends => (S =>+ null);
   --  Apply the Gimli permutation function.

   procedure XOR_Bits_Into_State (S       : in out State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Global => null,
     Depends => (S =>+ (Data, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then (Bit_Len + 7) / 8 <= Data'Length);
   --  XOR an arbitrary number of bits into the Gimli state.
   --
   --  The data size (in bits) cannot exceed the Gimli size (384 bits).

   procedure Extract_Bytes (S    : in     State;
                            Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ S),
     Pre => Data'Length <= (State_Size_Bits / 8);
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "GNATprove issues a false positive due to the use of loops to initialize Data");
   --  Copy bytes from the Gimli state.
   --
   --  The number of bytes to copy cannot exceed the Gimli state size (48 bytes)

   procedure Extract_Bits (A       : in     State;
                           Data    :    out Keccak.Types.Byte_Array;
                           Bit_Len : in     Natural)
     with Global => null,
     Depends => (Data =>+ (A, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);
   --  Copy bits from the Gimli state.
   --
   --  The number of bits to copy cannot exceed the Gimli state size (384 bits)

private

   type Row_Number    is range 0 .. 2;
   type Column_Number is range 0 .. 3;

   type Round_Number is range 1 .. 24;

   type State is array (Column_Number, Row_Number) of Unsigned_32
     with Size => 384;

   procedure SP_Box (S : in out State)
     with Inline,
     Global => null,
     Depends => (S =>+ null);
   --  SP-box operation.

   procedure Swap (A, B : in out Unsigned_32)
     with Inline,
     Global => null,
     Depends => (A => B,
                 B => A),
     Post => (A = B'Old and B = A'Old);
   --  Swap two words.

end Gimli;
