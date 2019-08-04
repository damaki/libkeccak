-------------------------------------------------------------------------------
-- Copyright (c) 2019, Daniel King
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

with AUnit.Assertions; use AUnit.Assertions;

package body Ketje_Tests
is

   --  Test that performing an encrypt then a decrypt yields the original plaintext.
   --
   --  The test is repeated for varying lengths of AAD preceding the encryption/decryption.
   procedure Test_Encrypt_Decrypt (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      AAD : constant Byte_Array (1 .. 50) := (others => 16#FF#);
      PT1 : Byte_Array (1 .. 1024);
      CT1 : Byte_Array (1 .. 1024) := (others => 0);
      PT2 : Byte_Array (1 .. 1024) := (others => 0);

   begin
      for I in 0 .. PT1'Length - 1 loop
         PT1 (PT1'First + I) := Byte (I mod 256);
      end loop;

      for N in 0 .. AAD'Length loop
         MonkeyWrap.Init (Ctx   => Ctx,
                        Key   => Key,
                        Nonce => Nonce);

         MonkeyWrap.Update_Auth_Data (Ctx  => Ctx,
                                      Data => AAD (1 .. N));

         MonkeyWrap.Update_Encrypt (Ctx        => Ctx,
                                    Plaintext  => PT1,
                                    Ciphertext => CT1);

         MonkeyWrap.Init (Ctx   => Ctx,
                        Key   => Key,
                        Nonce => Nonce);

         MonkeyWrap.Update_Auth_Data (Ctx  => Ctx,
                                      Data => AAD (1 .. N));

         MonkeyWrap.Update_Decrypt (Ctx        => Ctx,
                                    Ciphertext => CT1,
                                    Plaintext  => PT2);

         Assert (PT1 = PT2, "Decryption resulted in different plaintext with AAD'Length=" & Integer'Image (N));
      end loop;

   end Test_Encrypt_Decrypt;

   procedure Test_Streaming_AAD (T : in out Test) is
   begin
      null;
   end Test_Streaming_AAD;

   procedure Test_Streaming_Ciphertext (T : in out Test) is
   begin
      null;
   end Test_Streaming_Ciphertext;

   procedure Test_Streaming_Tag (T : in out Test) is
   begin
      null;
   end Test_Streaming_Tag;

end Ketje_Tests;
