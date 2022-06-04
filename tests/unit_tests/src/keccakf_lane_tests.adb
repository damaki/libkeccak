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

package body KeccakF_Lane_Tests is

   --  Test that XORing bytes into a (zeroed) state, then extracting them
   --  yields the original data.
   --
   --  This ensures that XOR_Bits_Into_State and Extract_Bits both use the
   --  same mapping to the internal state.
   procedure Test_XOR_Extract (T : in out Test) is
      State_Size_Bytes : constant Positive := State_Size_Bits / 8;

      S : State;

      Data_In  : Byte_Array (1 .. State_Size_Bytes);
      Data_Out : Byte_Array (1 .. State_Size_Bytes);

   begin
      for I in Data_In'Range loop
         Data_In (I) := Keccak.Types.Byte (I mod 256);
      end loop;

      for N in 1 .. State_Size_Bytes loop

         Init_State (S);

         XOR_Bits_Into_State (A       => S,
                              Data    => Data_In,
                              Bit_Len => N * 8);

         Data_Out := (others => 16#AA#);

         Extract_Bits (A       => S,
                       Data    => Data_Out (1 .. N),
                       Bit_Len => N * 8);

         Assert (Data_In (1 .. N) = Data_Out (1 .. N),
                 "Failed for N = " & Integer'Image (N));

      end loop;
   end Test_XOR_Extract;

end KeccakF_Lane_Tests;
