with CSHAKE;
with SHAKE;
with Keccak.Parallel_Keccak_1600.Rounds_24;
with Keccak.Generic_Parallel_CSHAKE;
with Keccak.Generic_Parallel_Hash;
with Keccak.Generic_Parallel_Sponge;
with Keccak.Generic_Parallel_XOF;
with Keccak.Padding;

package Parallel_Hash
with SPARK_Mode => On
is

   ----------------------------------------------------------------------------
   --  Parallel XOF instantiations with 256 bits of capacity
   --
   --  Note that for ParallelHash the function name and customization strings
   --  are empty, in which case CSHAKE128 is equivalent to SHAKE128.

   package XOF128_P2 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P2,
      Capacity    => 256,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF128_P4 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P4,
      Capacity    => 256,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF128_P8 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P8,
      Capacity    => 256,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   ----------------------------------------------------------------------------
   --  Parallel XOF instantiations with 512 bits of capacity
   --
   --  Note that for ParallelHash the function name and customization strings
   --  are empty, in which case CSHAKE256 is equivalent to SHAKE256.

   package XOF256_P2 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P2,
      Capacity    => 512,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF256_P4 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P4,
      Capacity    => 512,
      Suffix      => 2#11_11#,
      Suffix_Size => 4);

   package XOF256_P8 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P8,
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
