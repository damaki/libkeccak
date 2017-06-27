with CSHAKE;
with SHAKE;
with Keccak.Parallel_Keccak_1600;
with Keccak.Generic_Parallel_CSHAKE;
with Keccak.Generic_Parallel_Hash;
with Keccak.Generic_Parallel_Sponge;
with Keccak.Generic_Parallel_XOF;
with Keccak.Padding;

package Parallel_Hash
with SPARK_Mode => On
is

   ----------------------------------------------------------------------------
   --  Parallel sponge instantiations based on Keccak-p[1600,24]

   package Sponge_P2 is new Keccak.Generic_Parallel_Sponge
     (State_Size                   => 1600,
      State_Type                   => Keccak.Parallel_Keccak_1600.Parallel_State_P2,
      Parallelism                  => 2,
      Init                         => Keccak.Parallel_Keccak_1600.Init_P2,
      Permute_All                  => Keccak.Parallel_Keccak_1600.Permute_All_P2_R24,
      XOR_Bits_Into_State_Separate => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_Separate_P2,
      XOR_Bits_Into_State_All      => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_All_P2,
      Extract_Bytes                => Keccak.Parallel_Keccak_1600.Extract_Bytes_P2,
      Pad                          => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits             => Keccak.Padding.Pad101_Min_Bits);

   package Sponge_P4 is new Keccak.Generic_Parallel_Sponge
     (State_Size                   => 1600,
      State_Type                   => Keccak.Parallel_Keccak_1600.Parallel_State_P4,
      Parallelism                  => 4,
      Init                         => Keccak.Parallel_Keccak_1600.Init_P4,
      Permute_All                  => Keccak.Parallel_Keccak_1600.Permute_All_P4_R24,
      XOR_Bits_Into_State_Separate => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_Separate_P4,
      XOR_Bits_Into_State_All      => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_All_P4,
      Extract_Bytes                => Keccak.Parallel_Keccak_1600.Extract_Bytes_P4,
      Pad                          => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits             => Keccak.Padding.Pad101_Min_Bits);

   package Sponge_P8 is new Keccak.Generic_Parallel_Sponge
     (State_Size                   => 1600,
      State_Type                   => Keccak.Parallel_Keccak_1600.Parallel_State_P8,
      Parallelism                  => 8,
      Init                         => Keccak.Parallel_Keccak_1600.Init_P8,
      Permute_All                  => Keccak.Parallel_Keccak_1600.Permute_All_P8_R24,
      XOR_Bits_Into_State_Separate => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_Separate_P8,
      XOR_Bits_Into_State_All      => Keccak.Parallel_Keccak_1600.XOR_Bits_Into_State_All_P8,
      Extract_Bytes                => Keccak.Parallel_Keccak_1600.Extract_Bytes_P8,
      Pad                          => Keccak.Padding.Pad101_Single_Block,
      Min_Padding_Bits             => Keccak.Padding.Pad101_Min_Bits);

   ----------------------------------------------------------------------------
   --  Parallel XOF instantiations with 256 bits of capacity
   --
   --  Note that for ParallelHash the function name and customization strings
   --  are empty, in which case CSHAKE128 is equivalent to SHAKE128.

   package XOF128_P2 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P2,
      Capacity    => 256,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF128_P4 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P4,
      Capacity    => 256,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF128_P8 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P8,
      Capacity    => 256,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   ----------------------------------------------------------------------------
   --  Parallel XOF instantiations with 512 bits of capacity
   --
   --  Note that for ParallelHash the function name and customization strings
   --  are empty, in which case CSHAKE256 is equivalent to SHAKE256.

   package XOF256_P2 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P2,
      Capacity    => 512,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF256_P4 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P4,
      Capacity    => 512,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF256_P8 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Sponge_P8,
      Capacity    => 512,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   ----------------------------------------------------------------------------
   --  ParallelHash instantiations

   package ParallelHash128 is new Keccak.Generic_Parallel_Hash
     (CV_Size_Bytes    => 256 / 8,
      CSHAKE_Serial    => CSHAKE.CSHAKE128,
      SHAKE_Serial     => SHAKE.SHAKE128,
      SHAKE_Parallel_2 => XOF128_P2,
      SHAKE_Parallel_4 => XOF128_P4,
      SHAKE_Parallel_8 => XOF128_P8);

   package ParallelHash256 is new Keccak.Generic_Parallel_Hash
     (CV_Size_Bytes    => 512 / 8,
      CSHAKE_Serial    => CSHAKE.CSHAKE256,
      SHAKE_Serial     => SHAKE.SHAKE256,
      SHAKE_Parallel_2 => XOF256_P2,
      SHAKE_Parallel_4 => XOF256_P4,
      SHAKE_Parallel_8 => XOF256_P8);

end Parallel_Hash;
