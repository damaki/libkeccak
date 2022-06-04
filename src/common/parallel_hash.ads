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
with CSHAKE;
with SHAKE;
with Keccak.Parallel_Keccak_1600.Rounds_24;
with Keccak.Generic_Parallel_Hash;
with Keccak.Generic_Parallel_XOF;

--  @summary
--  Instantiates ParallelHash algorithms as defined in NIST SP 800-185.
--
--  @group ParallelHash
package Parallel_Hash
with SPARK_Mode => On
is

   --  @summary
   --  Contains implementation details for constructing ParallelHash128 and 256.
   --
   --  @private
   package Implementation is

      -------------------------------------------------------------
      --  Parallel XOF instantiations with 256 bits of capacity  --
      -------------------------------------------------------------
      --
      --  Note that for ParallelHash the function name and customization strings
      --  should be empty, in which case CSHAKE128 is equivalent to SHAKE128.

      package XOF128_P2 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P2,
         Capacity    => 256,
         Suffix      => 2#11_11#,
         Suffix_Size => 4);

      package XOF128_P4 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P4,
         Capacity    => 256,
         Suffix      => 2#11_11#,
         Suffix_Size => 4);

      package XOF128_P8 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P8,
         Capacity    => 256,
         Suffix      => 2#11_11#,
         Suffix_Size => 4);

      -------------------------------------------------------------
      --  Parallel XOF instantiations with 512 bits of capacity  --
      -------------------------------------------------------------
      --
      --  Note that for ParallelHash the function name and customization strings
      --  should be empty, in which case CSHAKE256 is equivalent to SHAKE256.

      package XOF256_P2 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P2,
         Capacity    => 512,
         Suffix      => 2#11_11#,
         Suffix_Size => 4);

      package XOF256_P4 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P4,
         Capacity    => 512,
         Suffix      => 2#11_11#,
         Suffix_Size => 4);

      package XOF256_P8 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P8,
         Capacity    => 512,
         Suffix      => 2#11_11#,
         Suffix_Size => 4);

   end Implementation;

   -----------------------------------
   --  ParallelHash instantiations  --
   -----------------------------------

   package ParallelHash128 is new Keccak.Generic_Parallel_Hash
     (CV_Size_Bytes    => 256 / 8,
      CSHAKE_Serial    => CSHAKE.CSHAKE128,
      SHAKE_Serial     => SHAKE.SHAKE128,
      SHAKE_Parallel_2 => Implementation.XOF128_P2,
      SHAKE_Parallel_4 => Implementation.XOF128_P4,
      SHAKE_Parallel_8 => Implementation.XOF128_P8);

   package ParallelHash256 is new Keccak.Generic_Parallel_Hash
     (CV_Size_Bytes    => 512 / 8,
      CSHAKE_Serial    => CSHAKE.CSHAKE256,
      SHAKE_Serial     => SHAKE.SHAKE256,
      SHAKE_Parallel_2 => Implementation.XOF256_P2,
      SHAKE_Parallel_4 => Implementation.XOF256_P4,
      SHAKE_Parallel_8 => Implementation.XOF256_P8);

end Parallel_Hash;
