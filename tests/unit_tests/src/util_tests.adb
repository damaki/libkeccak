-------------------------------------------------------------------------------
--  Copyright (c) 2016, Daniel King
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

with AUnit.Assertions; use AUnit.Assertions;
with Keccak.Types;     use Keccak.Types;
with Keccak.Util;      use Keccak.Util;

package body Util_Tests
is

   overriding
   procedure Set_Up (T : in out Test)
   is
   begin
      null;
   end Set_Up;

   procedure Test_Left_Encode_Bit_Length_Equivalence (T : in out Test)
   is
   begin
      for N in 0 .. Natural'Last / 8 loop
         Assert (Left_Encode_NIST_Bit_Length (N) = Left_Encode_NIST (N * 8),
                 "Failed for N = " & Natural'Image (N));
      end loop;
   end Test_Left_Encode_Bit_Length_Equivalence;

   procedure Test_Right_Encode_Bit_Length_Equivalence (T : in out Test)
   is
   begin
      for N in 0 .. Natural'Last / 8 loop
         Assert (Right_Encode_NIST_Bit_Length (N) = Right_Encode_NIST (N * 8),
                 "Failed for N = " & Natural'Image (N));
      end loop;
   end Test_Right_Encode_Bit_Length_Equivalence;

end Util_Tests;
