pragma SPARK_Mode (On);

with Gimli.Sponge;
with Keccak.Generic_Hash;

package Gimli.Hash is new Keccak.Generic_Hash
  (Hash_Sponge => Gimli.Sponge,
   Digest_Size => 256 / 8,
   Capacity    => 256,
   Suffix      => 0,
   Suffix_Size => 0);