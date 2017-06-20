with Keccak.Arch.SSE2;
with Keccak.Generic_Parallel_KeccakF;
with Keccak.Generic_Parallel_Permutation_Parallel_Fallback;
with Keccak.Types;
with Interfaces;

pragma Elaborate_All (Keccak.Generic_Parallel_KeccakF);
pragma Elaborate_All (Keccak.Generic_Parallel_Permutation_Parallel_Fallback);

package Keccak.Parallel_Keccak_1600
with SPARK_Mode => On
is

   ----------------------------------------------------------------------------
   -- Keccak-f[1600]x2

   --  128-bit SIMD (SSE) is available on this architecture.

   package KeccakF_1600_P2 is new Keccak.Generic_Parallel_KeccakF
     (L            => 6,
      Lane_Type    => Interfaces.Unsigned_64,
      VXXI_Index   => Arch.SSE2.V2DI_Vectors.V2DI_Index,
      VXXI         => Arch.SSE2.V2DI_Vectors.V2DI,
      VXXI_View    => Arch.SSE2.V2DI_Vectors.V2DI_View,
      Vector_Width => 2,
      Load         => Arch.SSE2.V2DI_Vectors.Load,
      Store        => Arch.SSE2.V2DI_Vectors.Store,
      "xor"        => Arch.SSE2.V2DI_Vectors."xor",
      Rotate_Left  => Arch.SSE2.V2DI_Vectors.Rotate_Left,
      And_Not      => Arch.SSE2.V2DI_Vectors.And_Not,
      Shift_Left   => Interfaces.Shift_Left,
      Shift_Right  => Interfaces.Shift_Right);

   subtype Parallel_State_P2 is KeccakF_1600_P2.Parallel_State;

   procedure Init_P2 (S : out KeccakF_1600_P2.Parallel_State)
                      renames KeccakF_1600_P2.Init;

   procedure Permute_All_P2_R12 is new KeccakF_1600_P2.Permute_All
     (First_Round => 12,
      Num_Rounds  => 12);

   procedure Permute_All_P2_R24 is new KeccakF_1600_P2.Permute_All
     (First_Round => 0,
      Num_Rounds  => 24);

   procedure XOR_Bits_Into_State_P2
     (S           : in out Parallel_State_P2;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P2.XOR_Bits_Into_State;

   procedure Extract_Bytes_P2
     (S           : in     Parallel_State_P2;
      Data        : in out Types.Byte_Array;
      Data_Offset : in     Natural;
      Byte_Len    : in     Natural)
      renames KeccakF_1600_P2.Extract_Bytes;

   ----------------------------------------------------------------------------
   --  Keccak-f[1600]x4

   --  No 256-bit SIMD available on this architecture. Fallback to 2x2

   package KeccakF_1600_P4
   is new Keccak.Generic_Parallel_Permutation_Parallel_Fallback
     (Permutation_State   => Parallel_State_P2,
      Base_Parallelism    => 2,
      Parallel_Factor     => 2,
      Init                => Init_P2,
      XOR_Bits_Into_State => XOR_Bits_Into_State_P2,
      Extract_Bytes       => Extract_Bytes_P2,
      State_Size_Bits     => 1600);

   subtype Parallel_State_P4 is KeccakF_1600_P4.Parallel_State;

   procedure Init_P4 (S : out Parallel_State_P4)
                      renames KeccakF_1600_P4.Init;

   procedure Permute_All_P4_R12 is new KeccakF_1600_P4.Permute_All
     (Permute_All_P2_R12);

   procedure Permute_All_P4_R24 is new KeccakF_1600_P4.Permute_All
     (Permute_All_P2_R24);

   procedure XOR_Bits_Into_State_P4
     (S           : in out Parallel_State_P4;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P4.XOR_Bits_Into_State;

   procedure Extract_Bytes_P4
     (S           : in     Parallel_State_P4;
      Data        : in out Types.Byte_Array;
      Data_Offset : in     Natural;
      Byte_Len    : in     Natural)
      renames KeccakF_1600_P4.Extract_Bytes;

   ----------------------------------------------------------------------------
   --  Keccak-f[1600]x8

   --  No 512-bit SIMD available on this architecture. Fallback to 2x4

   package KeccakF_1600_P8
   is new Keccak.Generic_Parallel_Permutation_Parallel_Fallback
     (Permutation_State   => Parallel_State_P2,
      Base_Parallelism    => 2,
      Parallel_Factor     => 4,
      Init                => Init_P2,
      XOR_Bits_Into_State => XOR_Bits_Into_State_P2,
      Extract_Bytes       => Extract_Bytes_P2,
      State_Size_Bits     => 1600);

   subtype Parallel_State_P8 is KeccakF_1600_P8.Parallel_State;

   procedure Init_P8 (S : out Parallel_State_P8)
                      renames KeccakF_1600_P8.Init;

   procedure Permute_All_P8_R12 is new KeccakF_1600_P8.Permute_All
     (Permute_All_P2_R12);

   procedure Permute_All_P8_R24 is new KeccakF_1600_P8.Permute_All
     (Permute_All_P2_R24);

   procedure XOR_Bits_Into_State_P8
     (S           : in out Parallel_State_P8;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
      renames KeccakF_1600_P8.XOR_Bits_Into_State;

   procedure Extract_Bytes_P8
     (S           : in     Parallel_State_P8;
      Data        : in out Types.Byte_Array;
      Data_Offset : in     Natural;
      Byte_Len    : in     Natural)
      renames KeccakF_1600_P8.Extract_Bytes;


end Keccak.Parallel_Keccak_1600;
