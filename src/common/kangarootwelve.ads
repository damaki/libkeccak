-------------------------------------------------------------------------------
-- Copyright (c) 2017, Daniel King
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
with Keccak.Keccak_1600;
with Keccak.Generic_KangarooTwelve;
with Keccak.Parallel_Keccak_1600;
with Keccak.Generic_Parallel_Sponge;
with Keccak.Generic_Parallel_XOF;
with Keccak.Generic_Sponge;
with Keccak.Generic_XOF;
with Keccak.Padding;
with Interfaces;

package KangarooTwelve
with SPARK_Mode => On
is

   K12_Capacity : constant := 256;

   procedure Permute_R12 is new Keccak.Keccak_1600.KeccakF_1600_Permutation.Permute
     (First_Round => 12,
      Num_Rounds  => 12);

   --  Serial sponge based on Keccak-f[1600,12]
   package Sponge_S1 is new Keccak.Generic_Sponge
     (State_Size          => 1600,
      State_Type          => Keccak.Keccak_1600.KeccakF_1600.State,
      Init_State          => Keccak.Keccak_1600.KeccakF_1600.Init,
      F                   => Permute_R12,
      XOR_Bits_Into_State => Keccak.Keccak_1600.KeccakF_1600_Lanes.XOR_Bits_Into_State,
      Extract_Data        => Keccak.Keccak_1600.KeccakF_1600_Lanes.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Multi_Blocks);

   --  Parallel sponge based on Keccak-f[1600,12]×2
   package Sponge_P2 is new Keccak.Generic_Parallel_Sponge
     (State_Size          => 1600,
      State_Type          => Keccak.Parallel_Keccak_1600.Parallel_State_P2,
      Parallelism         => 2,
      Init                => Keccak.Parallel_Keccak_1600.Init_P2,
      Permute_All         => Keccak.Parallel_Keccak_1600.Permute_All_P2_R12,
      XOR_Bits_Into_State => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_P2,
      Extract_Bytes       => Keccak.Parallel_Keccak_1600.Extract_Bytes_P2,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);

   --  Parallel sponge based on Keccak-f[1600,12]×4
   package Sponge_P4 is new Keccak.Generic_Parallel_Sponge
     (State_Size          => 1600,
      State_Type          => Keccak.Parallel_Keccak_1600.Parallel_State_P4,
      Parallelism         => 4,
      Init                => Keccak.Parallel_Keccak_1600.Init_P4,
      Permute_All         => Keccak.Parallel_Keccak_1600.Permute_All_P4_R12,
      XOR_Bits_Into_State => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_P4,
      Extract_Bytes       => Keccak.Parallel_Keccak_1600.Extract_Bytes_P4,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);

   --  Parallel sponge based on Keccak-f[1600,12]×8
   package Sponge_P8 is new Keccak.Generic_Parallel_Sponge
     (State_Size          => 1600,
      State_Type          => Keccak.Parallel_Keccak_1600.Parallel_State_P8,
      Parallelism         => 8,
      Init                => Keccak.Parallel_Keccak_1600.Init_P8,
      Permute_All         => Keccak.Parallel_Keccak_1600.Permute_All_P8_R12,
      XOR_Bits_Into_State => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_P8,
      Extract_Bytes       => Keccak.Parallel_Keccak_1600.Extract_Bytes_P8,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);


   --  Now we can build a XOF on each parallel sponge
   package XOF_S1 is new Keccak.Generic_XOF
     (XOF_Sponge  => Sponge_S1,
      Capacity    => K12_Capacity,
      Suffix      => 0, --  Add no suffix here, since suffix is dynamic (01 or 11)
      Suffix_Size => 0);

   package XOF_P2 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P2,
      Capacity    => K12_Capacity,
      Suffix      => 2#011#,
      Suffix_Size => 3);

   package XOF_P4 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P4,
      Capacity    => K12_Capacity,
      Suffix      => 2#011#,
      Suffix_Size => 3);

   package XOF_P8 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P8,
      Capacity    => K12_Capacity,
      Suffix      => 2#011#,
      Suffix_Size => 3);


   --  Finally, we can build our KangarooTwelve instance using the different
   --  levels of parallel XOFs
   package K12 is new Keccak.Generic_KangarooTwelve
     (XOF_Serial     => XOF_S1,
      XOF_Parallel_2 => XOF_P2,
      XOF_Parallel_4 => XOF_P4,
      XOF_Parallel_8 => XOF_P8);

end KangarooTwelve;
