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

--  @summary
--  Generic implementation of the Keccak-f permutations.
--
--  @description
--  This generic package implements the Keccak-f permutations for bit sizes of:
--  25, 50, 100, 200, 400, 800, and 1600 bits.
--
--  @group Keccak-f
generic
   --  The binary logarithm of the lane size.
   --
   --  This determines the Keccak-f state size. Possible values are:
   --  * Lane_Size_Log = 0 => 1-bit lanes,  Keccak-f[25]
   --  * Lane_Size_Log = 1 => 2-bit lanes,  Keccak-f[50]
   --  * Lane_Size_Log = 2 => 4-bit lanes,  Keccak-f[100]
   --  * Lane_Size_Log = 3 => 8-bit lanes,  Keccak-f[200]
   --  * Lane_Size_Log = 4 => 16-bit lanes, Keccak-f[400]
   --  * Lane_Size_Log = 5 => 32-bit lanes, Keccak-f[800]
   --  * Lane_Size_Log = 6 => 64-bit lanes, Keccak-f[1600]
   Lane_Size_Log : in Natural;

   --  Modular type for a lane of the Keccak state.
   --
   --  Lane_Type'Modulus must be equal to 2**(2**Lane_Size_Log).
   --  For example, when Lane_Size_Log=6 Lane_Type must be a 64-bit
   --  mod type (2**Lane_Size_Log = 64 when Lane_Size_Log = 6).
   type Lane_Type is mod <>;

package Keccak.Generic_KeccakF
is
   Lane_Size_Bits  : constant Positive := 2**Lane_Size_Log;
   State_Size_Bits : constant Positive := Lane_Size_Bits * 25;

   pragma Assert (Lane_Type'Modulus = 2**Lane_Size_Bits,
                  "Value for Lane_Size_Log is incompatible with the specified lane type");

   subtype Round_Count is Positive range 1 .. 24;

   type Round_Index is new Natural range 0 .. 23;

   type State is private;
   --  Keccak-f[B] state, where B is the state size in bits (e.g. 1600 bits).

   type Lane_Complemented_State is private;
   --  State type used for the lane complementing implementation.
   --
   --  A distinct type is used here to prevent confusion in using the wrong
   --  subprograms with the wrong type, as specific implementations are needed
   --  to handle the lane complemented Keccak-f state.

   procedure Init (A : out State)
     with Global => null,
     Depends => (A => null);
   --  Initialize the Keccak-f state.
   --
   --  Initially, the Keccak state is set to 0.

   procedure Init (A : out Lane_Complemented_State)
     with Global => null,
     Depends => (A => null);
   --  Initialize the Keccak-f state.
   --
   --  Initially, the Keccak state is set to 0.

private
   type X_Coord is mod 5;
   type Y_Coord is mod 5;

   type State is array (X_Coord, Y_Coord) of Lane_Type;

   type Lane_Complemented_State is new State;

end Keccak.Generic_KeccakF;
