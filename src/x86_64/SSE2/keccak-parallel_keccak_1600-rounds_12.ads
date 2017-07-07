with Keccak.Keccak_1600.Rounds_12;
with Keccak.Generic_Parallel_Sponge;
with Keccak.Padding;

pragma Elaborate_All (Keccak.Generic_Parallel_Sponge);

package Keccak.Parallel_Keccak_1600.Rounds_12
with SPARK_Mode => On
is

   procedure Permute_All_P2 is new KeccakF_1600_P2.Permute_All
     (First_Round => 12,
      Num_Rounds  => 12);
   
   procedure Permute_All_P4 is new KeccakF_1600_P4.Permute_All
     (Permute_All_P2);
   
   procedure Permute_All_P8 is new KeccakF_1600_P8.Permute_All
     (Permute_All_P2);
   
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

end Keccak.Parallel_Keccak_1600.Rounds_12;
