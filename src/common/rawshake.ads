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

with Keccak.Keccak_1600.Rounds_24;
with Keccak.Generic_XOF;

pragma Elaborate_All (Keccak.Generic_XOF);

--  @summary
--  Instantations of RawSHAKE128 and RawSHAKE256.
--
--  @group SHAKE
package RawSHAKE
with SPARK_Mode => On
is

   --  RawSHAKE has 2 suffix bits appended to each message: 2#11#.
   --
   --  See Section 6.3 of NIST FIPS-202.

   package RawSHAKE128 is new Keccak.Generic_XOF
     (XOF_Sponge  => Keccak.Keccak_1600.Rounds_24.Sponge,
      Capacity    => 256,
      Suffix      => 2#11#,
      Suffix_Size => 2);

   package RawSHAKE256 is new Keccak.Generic_XOF
     (XOF_Sponge  => Keccak.Keccak_1600.Rounds_24.Sponge,
      Capacity    => 512,
      Suffix      => 2#11#,
      Suffix_Size => 2);

end RawSHAKE;
