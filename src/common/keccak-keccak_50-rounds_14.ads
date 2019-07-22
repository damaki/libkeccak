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
with Keccak.Generic_Duplex;
with Keccak.Generic_Sponge;

pragma Elaborate_All (Keccak.Generic_Duplex);
pragma Elaborate_All (Keccak.Generic_Sponge);

--  @summary
--  Instantiation of Keccak-p[50,14], with a Sponge and Duplex built on top of it.
package Keccak.Keccak_50.Rounds_14
with SPARK_Mode => On
is

   procedure Permute is new KeccakF_50_Permutation.Permute
     (First_Round => 10,
      Num_Rounds  => 14);

   package Sponge is new Keccak.Generic_Sponge
     (State_Size          => KeccakF_50.B,
      State_Type          => KeccakF_50.Lane_Complemented_State,
      Init_State          => KeccakF_50.Init,
      F                   => Permute,
      XOR_Bits_Into_State => KeccakF_50_Lanes.XOR_Bits_Into_State,
      Extract_Data        => KeccakF_50_Lanes.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Multi_Blocks);

   package Duplex is new Keccak.Generic_Duplex
     (State_Size          => KeccakF_50.B,
      State_Type          => KeccakF_50.Lane_Complemented_State,
      Init_State          => KeccakF_50.Init,
      F                   => Permute,
      XOR_Bits_Into_State => KeccakF_50_Lanes.XOR_Bits_Into_State,
      Extract_Bits        => KeccakF_50_Lanes.Extract_Bits,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);

end Keccak.Keccak_50.Rounds_14;
