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
with Keccak.Keccak_1600.Rounds_12;
with Keccak.Generic_KangarooTwelve;
with Keccak.Parallel_Keccak_1600.Rounds_12;
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

   --  Now we can build a XOF on each parallel sponge
   package XOF_S1 is new Keccak.Generic_XOF
     (XOF_Sponge  => Keccak.Keccak_1600.Rounds_12.Sponge,
      Capacity    => K12_Capacity,
      Suffix      => 0, --  Add no suffix here, since suffix is dynamic (01 or 11)
      Suffix_Size => 0);

   package XOF_P2 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_12.Parallel_Sponge_P2,
      Capacity    => K12_Capacity,
      Suffix      => 2#011#,
      Suffix_Size => 3);

   package XOF_P4 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_12.Parallel_Sponge_P4,
      Capacity    => K12_Capacity,
      Suffix      => 2#011#,
      Suffix_Size => 3);

   package XOF_P8 is new Keccak.Generic_Parallel_XOF
     (Sponge      => Keccak.Parallel_Keccak_1600.Rounds_12.Parallel_Sponge_P8,
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
