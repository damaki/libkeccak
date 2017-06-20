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
with Keccak.Arch.SSE2;
with Keccak.Keccak_1600;
with Keccak.Generic_KangarooTwelve;
with Keccak.Generic_Parallel_KeccakF;
with Keccak.Generic_Parallel_Permutation_Serial_Fallback;
with Keccak.Generic_Parallel_Permutation_Parallel_Fallback;
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

   --  KangarooTwelve uses the Keccak-F[1600] permutation with 12 rounds.
   procedure Permute_KeccakF_1600_R12_S1
   is new Keccak.Keccak_1600.KeccakF_1600_Permutation.Permute
     (First_Round => 12,
      Num_Rounds  => 12);

   --  The generic KangarooTwelve implementation requires parallel implementations
   --  of 2x, 4x, and 8x parallelism.
   package KeccakF_1600_R12_P2 is
     new Keccak.Generic_Parallel_KeccakF
       (L               => 6,
        Lane_Type       => Interfaces.Unsigned_64,
        VXXI_Index      => Keccak.Arch.SSE2.V2DI_Index,
        VXXI            => Keccak.Arch.SSE2.V2DI,
        VXXI_View       => Keccak.Arch.SSE2.V2DI_View,
        Load            => Keccak.Arch.SSE2.Load,
        Store           => Keccak.Arch.SSE2.Store,
        "xor"           => Keccak.Arch.SSE2."xor",
        Rotate_Left     => Keccak.Arch.SSE2.Rotate_Left,
        And_Not         => Keccak.Arch.SSE2.And_Not,
        Shift_Left      => Interfaces.Shift_Left,
        Shift_Right     => Interfaces.Shift_Right);

   procedure Permute_KeccakF_1600_R12_P2
   is new KeccakF_1600_R12_P2.Permute_All
     (First_Round => 12,
      Num_Rounds  => 12);


   package KeccakF_1600_R12_P4 is
     new Keccak.Generic_Parallel_Permutation_Parallel_Fallback
       (Permutation_State   => KeccakF_1600_R12_P2.Parallel_State,
        Input_State_Index   => Keccak.Arch.SSE2.V2DI_Index,
        Init                => KeccakF_1600_R12_P2.Init,
        Permute             => Permute_KeccakF_1600_R12_P2,
        XOR_Bits_Into_State => KeccakF_1600_R12_P2.XOR_Bits_Into_State,
        Extract_Bytes       => KeccakF_1600_R12_P2.Extract_Bytes,
        State_Size_Bits     => 1600);

   package KeccakF_1600_R12_P8 is
     new Keccak.Generic_Parallel_Permutation_Parallel_Fallback
       (Permutation_State   => KeccakF_1600_R12_P4.Parallel_State,
        Input_State_Index   => KeccakF_1600_R12_P4.State_Index,
        Init                => KeccakF_1600_R12_P4.Init,
        Permute             => KeccakF_1600_R12_P4.Permute_All,
        XOR_Bits_Into_State => KeccakF_1600_R12_P4.XOR_Bits_Into_State,
        Extract_Bytes       => KeccakF_1600_R12_P4.Extract_Bytes,
        State_Size_Bits     => 1600);


   --  We also need parallel sponges for each level of parallelism.
   package Sponge_S1 is new Keccak.Generic_Sponge
     (State_Size          => 1600,
      State_Type          => Keccak.Keccak_1600.KeccakF_1600.State,
      Init_State          => Keccak.Keccak_1600.KeccakF_1600.Init,
      F                   => Permute_KeccakF_1600_R12_S1,
      XOR_Bits_Into_State => Keccak.Keccak_1600.KeccakF_1600_Lanes.XOR_Bits_Into_State,
      Extract_Data        => Keccak.Keccak_1600.KeccakF_1600_Lanes.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Multi_Blocks);

   package Sponge_P2 is new Keccak.Generic_Parallel_Sponge
     (State_Size          => 1600,
      State_Type          => KeccakF_1600_R12_P2.Parallel_State,
      State_Index         => Keccak.Arch.SSE2.V2DI_Index,
      Init                => KeccakF_1600_R12_P2.Init,
      Permute_All         => Permute_KeccakF_1600_R12_P2,
      XOR_Bits_Into_State => KeccakF_1600_R12_P2.XOR_Bits_Into_State,
      Extract_Bytes       => KeccakF_1600_R12_P2.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);

   package Sponge_P4 is new Keccak.Generic_Parallel_Sponge
     (State_Size          => 1600,
      State_Type          => KeccakF_1600_R12_P4.Parallel_State,
      State_Index         => KeccakF_1600_R12_P4.State_Index,
      Init                => KeccakF_1600_R12_P4.Init,
      Permute_All         => KeccakF_1600_R12_P4.Permute_All,
      XOR_Bits_Into_State => KeccakF_1600_R12_P4.XOR_Bits_Into_State,
      Extract_Bytes       => KeccakF_1600_R12_P4.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);

   package Sponge_P8 is new Keccak.Generic_Parallel_Sponge
     (State_Size          => 1600,
      State_Type          => KeccakF_1600_R12_P8.Parallel_State,
      State_Index         => KeccakF_1600_R12_P8.State_Index,
      Init                => KeccakF_1600_R12_P8.Init,
      Permute_All         => KeccakF_1600_R12_P8.Permute_All,
      XOR_Bits_Into_State => KeccakF_1600_R12_P8.XOR_Bits_Into_State,
      Extract_Bytes       => KeccakF_1600_R12_P8.Extract_Bytes,
      Pad                 => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits    => Keccak.Padding.Pad101_Min_Bits);


   --  Now we can build a XOF on each parallel sponge
   package XOF_S1 is new Keccak.Generic_XOF
     (XOF_Sponge  => Sponge_S1,
      Capacity    => K12_Capacity,
      Suffix      => 0,
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
