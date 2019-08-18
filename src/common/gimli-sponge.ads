pragma SPARK_Mode (On);

with Gimli;
with Keccak.Generic_Sponge;
with Keccak.Padding;

package Gimli.Sponge is new Keccak.Generic_Sponge
  (State_Size_Bits     => Gimli.State_Size_Bits,
   State_Type          => Gimli.State,
   Init_State          => Gimli.Init,
   Permute             => Gimli.Permute,
   XOR_Bits_Into_State => Gimli.XOR_Bits_Into_State,
   Extract_Data        => Gimli.Extract_Bytes,
   Pad                 => Keccak.Padding.Pad101_Multi_Blocks);
