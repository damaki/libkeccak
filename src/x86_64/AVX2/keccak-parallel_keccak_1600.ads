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
with Keccak.Arch.SSE2;
with Keccak.Arch.AVX2;
with Keccak.Generic_Parallel_KeccakF;
with Keccak.Generic_Parallel_Permutation_Parallel_Fallback;
with Keccak.Types;
with Interfaces;

pragma Elaborate_All (Keccak.Generic_Parallel_KeccakF);
pragma Elaborate_All (Keccak.Generic_Parallel_Permutation_Parallel_Fallback);

--  @summary
--  Defines procedures for running Keccak-f permutations in parallel for
--  2x, 4x, and 8x parallelism, as well as serial permutations.
--
--  @description
--  This package must define the following types and procedures:
--  For 2x parallelism:
--    * Parallel_State_P2
--    * Init_P2
--    * XOR_Bits_Into_State_P2
--    * Extract_Bytes_P2
--
--  For 4x parallelism:
--    * Parallel_State_P4
--    * Init_P4
--    * XOR_Bits_Into_State_P4
--    * Extract_Bytes_P4
--
--  For 8x parallelism:
--    * Parallel_State_P8
--    * Init_P8
--    * XOR_Bits_Into_State_P8
--    * Extract_Bytes_P8
--
--  Any other declarations in this package are architecture-specific and should
--  not be relied upon.
package Keccak.Parallel_Keccak_1600
with SPARK_Mode => On
is

   ------------------------
   --  Keccak-f[1600]x2  --
   ------------------------

   --  128-bit SIMD (SSE) is available on this architecture.

   package KeccakF_1600_P2 is new Keccak.Generic_Parallel_KeccakF
     (Lane_Size_Log => 6,
      Lane_Type     => Interfaces.Unsigned_64,
      VXXI_Index    => Arch.SSE2.V2DI_Vectors.V2DI_Index,
      VXXI          => Arch.SSE2.V2DI_Vectors.V2DI,
      VXXI_View     => Arch.SSE2.V2DI_Vectors.V2DI_View,
      Vector_Width  => 2,
      Load          => Arch.SSE2.V2DI_Vectors.Load,
      Store         => Arch.SSE2.V2DI_Vectors.Store,
      "xor"         => Arch.SSE2.V2DI_Vectors."xor",
      Rotate_Left   => Arch.SSE2.V2DI_Vectors.Rotate_Left,
      And_Not       => Arch.SSE2.V2DI_Vectors.And_Not,
      Shift_Left    => Interfaces.Shift_Left,
      Shift_Right   => Interfaces.Shift_Right);

   subtype Parallel_State_P2 is KeccakF_1600_P2.Parallel_State;

   procedure Init_P2 (S : out KeccakF_1600_P2.Parallel_State)
                      renames KeccakF_1600_P2.Init;

   procedure XOR_Bits_Into_State_Separate_P2
     (S           : in out Parallel_State_P2;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P2.XOR_Bits_Into_State_Separate;

   procedure XOR_Bits_Into_State_All_P2
     (S           : in out Parallel_State_P2;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P2.XOR_Bits_Into_State_All;

   procedure Extract_Bytes_P2
     (S           : in     Parallel_State_P2;
      Data        :    out Types.Byte_Array;
      Data_Offset : in     Natural;
      Byte_Len    : in     Natural)
      renames KeccakF_1600_P2.Extract_Bytes;

   ------------------------
   --  Keccak-f[1600]x4  --
   ------------------------

   --  256-bit SIMD (AVX2) is available on this architecture.

   package KeccakF_1600_P4 is new Keccak.Generic_Parallel_KeccakF
     (Lane_Size_Log => 6,
      Lane_Type     => Interfaces.Unsigned_64,
      VXXI_Index    => Arch.AVX2.V4DI_Vectors.V4DI_Index,
      VXXI          => Arch.AVX2.V4DI_Vectors.V4DI,
      VXXI_View     => Arch.AVX2.V4DI_Vectors.V4DI_View,
      Vector_Width  => 4,
      Load          => Arch.AVX2.V4DI_Vectors.Load,
      Store         => Arch.AVX2.V4DI_Vectors.Store,
      "xor"         => Arch.AVX2.V4DI_Vectors."xor",
      Rotate_Left   => Arch.AVX2.V4DI_Vectors.Rotate_Left,
      And_Not       => Arch.AVX2.V4DI_Vectors.And_Not,
      Shift_Left    => Interfaces.Shift_Left,
      Shift_Right   => Interfaces.Shift_Right);

   subtype Parallel_State_P4 is KeccakF_1600_P4.Parallel_State;

   procedure Init_P4 (S : out KeccakF_1600_P4.Parallel_State)
                      renames KeccakF_1600_P4.Init;

   procedure XOR_Bits_Into_State_Separate_P4
     (S           : in out Parallel_State_P4;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P4.XOR_Bits_Into_State_Separate;

   procedure XOR_Bits_Into_State_All_P4
     (S           : in out Parallel_State_P4;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P4.XOR_Bits_Into_State_All;

   procedure Extract_Bytes_P4
     (S           : in     Parallel_State_P4;
      Data        :    out Types.Byte_Array;
      Data_Offset : in     Natural;
      Byte_Len    : in     Natural)
      renames KeccakF_1600_P4.Extract_Bytes;

   ------------------------
   --  Keccak-f[1600]x8  --
   ------------------------

   --  No 512-bit SIMD available on this architecture. Fallback to 4x2

   package KeccakF_1600_P8
   is new Keccak.Generic_Parallel_Permutation_Parallel_Fallback
     (Permutation_State            => Parallel_State_P4,
      Base_Parallelism             => 4,
      Parallel_Factor              => 2,
      Init                         => Init_P4,
      XOR_Bits_Into_State_Separate => XOR_Bits_Into_State_Separate_P4,
      XOR_Bits_Into_State_All      => XOR_Bits_Into_State_All_P4,
      Extract_Bytes                => Extract_Bytes_P4,
      State_Size_Bits              => 1600);

   subtype Parallel_State_P8 is KeccakF_1600_P8.Parallel_State;

   procedure Init_P8 (S : out Parallel_State_P8)
                      renames KeccakF_1600_P8.Init;

   procedure XOR_Bits_Into_State_Separate_P8
     (S           : in out Parallel_State_P8;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P8.XOR_Bits_Into_State_Separate;

   procedure XOR_Bits_Into_State_All_P8
     (S           : in out Parallel_State_P8;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P8.XOR_Bits_Into_State_All;

   procedure Extract_Bytes_P8
     (S           : in     Parallel_State_P8;
      Data        :    out Types.Byte_Array;
      Data_Offset : in     Natural;
      Byte_Len    : in     Natural)
      renames KeccakF_1600_P8.Extract_Bytes;

end Keccak.Parallel_Keccak_1600;
