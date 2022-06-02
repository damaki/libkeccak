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
with Keccak.Keccak_1600.Rounds_14;
with Keccak.Generic_KangarooTwelve;
with Keccak.Parallel_Keccak_1600.Rounds_14;
with Keccak.Generic_Parallel_XOF;
with Keccak.Generic_XOF;

--  @summary
--  Defines the MarsupilamiFourteen instance.
--
--  @description
--  MarsupilamiFourteen is a variant of KangarooTwelve which provides 256-bit
--  security (compared to 128-bit for KangarooTwelve). MarsupilamiFourteen is
--  identical to KangarooTwelve except for the following differences:
--
--  * MarsupilamiFourteen uses a 512-bit capacity (256-bit for KangarooTwelve)
--  * MarsupilamiFourteen uses a 14-round Keccak permutation (14 rounds for KangarooTwelve)
--  * MarsupilamiFourteen uses a 512-bit chaining value (256-bit for KangarooTwelve)
--
--  @group MarsupilamiFourteen
package MarsupilamiFourteen
with SPARK_Mode => On
is

   --  @summary
   --  Implementation-defined packages for creating a M14 instance.
   --
   --  @private
   package Implementation is

      M14_Capacity : constant := 512;
      --  Capacity in bits (security parameter) for MarsupilamiFourteen.

      --  Now we can build a XOF on each parallel sponge
      package XOF_S1 is new Keccak.Generic_XOF
      (XOF_Sponge  => Keccak.Keccak_1600.Rounds_14.Sponge,
         Capacity    => M14_Capacity,
         Suffix      => 0, --  Add no suffix here, since suffix is dynamic (01 or 11)
         Suffix_Size => 0);

      package XOF_P2 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_14.Parallel_Sponge_P2,
         Capacity    => M14_Capacity,
         Suffix      => 2#011#,
         Suffix_Size => 3);

      package XOF_P4 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_14.Parallel_Sponge_P4,
         Capacity    => M14_Capacity,
         Suffix      => 2#011#,
         Suffix_Size => 3);

      package XOF_P8 is new Keccak.Generic_Parallel_XOF
      (XOF_Sponge      => Keccak.Parallel_Keccak_1600.Rounds_14.Parallel_Sponge_P8,
         Capacity    => M14_Capacity,
         Suffix      => 2#011#,
         Suffix_Size => 3);

   end Implementation;

   package M14 is new Keccak.Generic_KangarooTwelve
     (CV_Size_Bytes  => 512 / 8,
      XOF_Serial     => Implementation.XOF_S1,
      XOF_Parallel_2 => Implementation.XOF_P2,
      XOF_Parallel_4 => Implementation.XOF_P4,
      XOF_Parallel_8 => Implementation.XOF_P8);

end MarsupilamiFourteen;
