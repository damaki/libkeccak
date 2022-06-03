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

with AUnit.Test_Fixtures;
with Keccak.Generic_KeccakF;
with Keccak.Types;

generic
   with package KeccakF is new Keccak.Generic_KeccakF (<>);
   with procedure XOR_Bits_Into_State (A       : in out KeccakF.State;
                                       Data    : in     Keccak.Types.Byte_Array;
                                       Bit_Len : in     Natural);
   with procedure Extract_Bytes (A    : in     KeccakF.State;
                                 Data :    out Keccak.Types.Byte_Array);
   with procedure Extract_Bits (A       : in     KeccakF.State;
                                Data    :    out Keccak.Types.Byte_Array;
                                Bit_Len : in     Natural);
   with function Rotate_Left (Value  : in KeccakF.Lane_Type;
                              Amount : in Natural) return KeccakF.Lane_Type;
package KeccakF_Tests
is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with record
      State : KeccakF.State;
   end record;

   overriding
   procedure Set_Up (T : in out Test);

   procedure Test_Initial_State (T : in out Test);
   procedure Test_Extract_Bytes (T : in out Test);
   procedure Test_XOR_No_Data (T : in out Test);
   procedure Test_XOR_Entire_State (T : in out Test);
   procedure Test_XOR_Bit_Length (T : in out Test);
   procedure Test_Extract_Bits_Same_As_Extract_Bytes (T : in out Test);
   procedure Test_Permute_Implementations (T : in out Test);

end KeccakF_Tests;
