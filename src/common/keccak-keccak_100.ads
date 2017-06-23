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
with Interfaces;
with Keccak.Generic_Duplex;
with Keccak.Generic_KeccakF;
with Keccak.Generic_KeccakF.Bit_Lanes;
with Keccak.Generic_KeccakF.Optimized_Permutation;
with Keccak.Padding;
with Keccak.Generic_Sponge;
with Keccak.Types;

pragma Elaborate_All(Keccak.Generic_Duplex);
pragma Elaborate_All(Keccak.Generic_KeccakF);
pragma Elaborate_All(Keccak.Generic_KeccakF.Bit_Lanes);
pragma Elaborate_All(Keccak.Generic_KeccakF.Optimized_Permutation);
pragma Elaborate_All(Keccak.Generic_Sponge);

package Keccak.Keccak_100
with SPARK_Mode => On
is

   package KeccakF_100 is new Keccak.Generic_KeccakF
     (L           => 2,
      Lane_Type   => Keccak.Types.Unsigned_4,
      Shift_Left  => Keccak.Types.Shift_Left_4,
      Shift_Right => Keccak.Types.Shift_Right_4,
      Rotate_Left => Keccak.Types.Rotate_Left_4);

   package KeccakF_100_Permutation is new KeccakF_100.Optimized_Permutation;

   -- Keccak-f[100] permutation function with the default number of rounds.
   procedure Permute is new KeccakF_100_Permutation.Permute;

   package KeccakF_100_Lanes is new KeccakF_100.Bit_Lanes;

   package Sponge is new Keccak.Generic_Sponge
     (State_Size          => KeccakF_100.B,
      State_Type          => KeccakF_100.State,
      Init_State          => KeccakF_100.Init,
      F                   => Permute,
      XOR_Bits_Into_State => KeccakF_100_Lanes.XOR_Bits_Into_State,
      Extract_Data        => KeccakF_100_Lanes.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Multi_Blocks);

   package Duplex is new Keccak.Generic_Duplex
     (State_Size          => KeccakF_100.B,
      State_Type          => KeccakF_100.State,
      Init_State          => KeccakF_100.Init,
      F                   => Permute,
      XOR_Bits_Into_State => KeccakF_100_Lanes.XOR_Bits_Into_State,
      Extract_Bits        => KeccakF_100_Lanes.Extract_Bits,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);


end Keccak.Keccak_100;