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
with Interfaces;
with Keccak.Generic_KeccakF;
with Keccak.Generic_KeccakF.Byte_Lanes;
with Keccak.Generic_KeccakF.Lane_Complementing_Permutation;
with Keccak.Padding;

pragma Elaborate_All (Keccak.Generic_KeccakF);
pragma Elaborate_All (Keccak.Generic_KeccakF.Byte_Lanes);
pragma Elaborate_All (Keccak.Generic_KeccakF.Lane_Complementing_Permutation);

--  @summary
--  Instantiation of Keccak with a state size of 200 bits (8-bit lanes).
package Keccak.Keccak_200
with SPARK_Mode => On
is

   package KeccakF_200 is new Keccak.Generic_KeccakF
     (Lane_Size_Log => 3,
      Lane_Type     => Interfaces.Unsigned_8);

   subtype State is KeccakF_200.Lane_Complemented_State;

   package KeccakF_200_Permutation is new KeccakF_200.Lane_Complementing_Permutation
     (Rotate_Left => Interfaces.Rotate_Left);

   package KeccakF_200_Lanes is new KeccakF_200.Byte_Lanes
     (Shift_Left  => Interfaces.Shift_Left,
      Shift_Right => Interfaces.Shift_Right);

   procedure XOR_Pad101_Into_State is new Keccak.Padding.XOR_Pad101_Into_State
     (State_Size_Bits     => 200,
      State_Type          => State,
      XOR_Byte_Into_State => KeccakF_200_Lanes.XOR_Byte_Into_State);

end Keccak.Keccak_200;
