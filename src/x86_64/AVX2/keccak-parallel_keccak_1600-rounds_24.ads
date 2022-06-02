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
with Keccak.Generic_Parallel_Sponge;
with Keccak.Padding;

pragma Elaborate_All (Keccak.Generic_Parallel_Sponge);

package Keccak.Parallel_Keccak_1600.Rounds_24
with SPARK_Mode => On
is

   procedure Permute_All_P2 is new KeccakF_1600_P2.Permute_All
     (First_Round => 0,
      Num_Rounds  => 24);

   procedure Permute_All_P4 is new KeccakF_1600_P4.Permute_All
     (First_Round => 0,
      Num_Rounds  => 24);

   procedure Permute_All_P8 is new KeccakF_1600_P8.Permute_All
     (Permute_All_P4);

   package Parallel_Sponge_P2 is new Keccak.Generic_Parallel_Sponge
     (State_Size                   => 1600,
      State_Type                   => KeccakF_1600_P2.Parallel_State,
      Parallelism                  => 2,
      Init                         => KeccakF_1600_P2.Init,
      Permute_All                  => Permute_All_P2,
      XOR_Bits_Into_State_Separate => KeccakF_1600_P2.XOR_Bits_Into_State_Separate,
      XOR_Bits_Into_State_All      => KeccakF_1600_P2.XOR_Bits_Into_State_All,
      Extract_Bytes                => KeccakF_1600_P2.Extract_Bytes,
      Pad                          => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits             => Keccak.Padding.Pad101_Min_Bits);

   package Parallel_Sponge_P4 is new Keccak.Generic_Parallel_Sponge
     (State_Size                   => 1600,
      State_Type                   => KeccakF_1600_P4.Parallel_State,
      Parallelism                  => 4,
      Init                         => KeccakF_1600_P4.Init,
      Permute_All                  => Permute_All_P4,
      XOR_Bits_Into_State_Separate => KeccakF_1600_P4.XOR_Bits_Into_State_Separate,
      XOR_Bits_Into_State_All      => KeccakF_1600_P4.XOR_Bits_Into_State_All,
      Extract_Bytes                => KeccakF_1600_P4.Extract_Bytes,
      Pad                          => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits             => Keccak.Padding.Pad101_Min_Bits);

   package Parallel_Sponge_P8 is new Keccak.Generic_Parallel_Sponge
     (State_Size                   => 1600,
      State_Type                   => KeccakF_1600_P8.Parallel_State,
      Parallelism                  => 8,
      Init                         => KeccakF_1600_P8.Init,
      Permute_All                  => Permute_All_P8,
      XOR_Bits_Into_State_Separate => KeccakF_1600_P8.XOR_Bits_Into_State_Separate,
      XOR_Bits_Into_State_All      => KeccakF_1600_P8.XOR_Bits_Into_State_All,
      Extract_Bytes                => KeccakF_1600_P8.Extract_Bytes,
      Pad                          => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits             => Keccak.Padding.Pad101_Min_Bits);

end Keccak.Parallel_Keccak_1600.Rounds_24;
