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
--  Implementation of the Ascon permutation.
package Ascon
with SPARK_Mode => On
is

   State_Size_Bits : constant := 320;
   --  The Ascon state size, in bits.

   type State is private;

   type Round_Count is range 1 .. 12;

   procedure Init (S : out State)
     with Global => null,
     Depends => (S => null);
   --  Initialise the Ascon state.

   generic
      Num_Rounds : Round_Count;
   procedure Permute (S : in out State)
     with Global => null,
     Depends => (S =>+ null);
   --  Applies the Ascon permutation to the state.
   --
   --  The number of rounds is a tunable security parameter.

   procedure XOR_Bits_Into_State (S       : in out State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Global => null,
     Depends => (S =>+ (Data, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then (Bit_Len + 7) / 8 <= Data'Length);
   --  XOR an arbitrary number of bits into the Ascon state.
   --
   --  The data size (in bits) cannot exceed the Ascon size (320 bits).

   procedure Extract_Bytes (S    : in     State;
                            Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ S),
     Pre => Data'Length <= (State_Size_Bits / 8);
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "GNATprove issues a false positive due to the use of loops to initialize Data");
   --  Copy bytes from the Ascon state.
   --
   --  The number of bytes to copy cannot exceed the Ascon state size (40 bytes)

   procedure Extract_Bits (A       : in     State;
                           Data    :    out Keccak.Types.Byte_Array;
                           Bit_Len : in     Natural)
     with Global => null,
     Depends => (Data =>+ (A, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);
   --  Copy bits from the Ascon state.
   --
   --  The number of bits to copy cannot exceed the Ascon state size (320 bits)

private

   type Round_Number is range 0 .. 11;

   type X_Coord is range 0 .. 4;

   type State is array (X_Coord) of Unsigned_64
     with Size => 320;

   procedure Add_Constant (S     : in out State;
                           Round : in     Round_Number)
     with Inline,
     Global => null,
     Depends => (S =>+ Round);

   procedure Substitution (S : in out State)
     with Inline,
     Global => null,
     Depends => (S =>+ null);

   procedure Linear_Diffusion (S : in out State)
     with Inline,
     Global => null,
     Depends => (S =>+ null);

end Ascon;
