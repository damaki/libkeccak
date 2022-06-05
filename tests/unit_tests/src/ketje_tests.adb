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

with AUnit.Assertions; use AUnit.Assertions;
with Interfaces;       use Interfaces;
with Keccak.Types;     use Keccak.Types;

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

         Assert
           (PT1 = PT2,
            "Decryption resulted in different plaintext with AAD'Length =" & Integer'Image (N));
      end loop;

   end Test_Encrypt_Decrypt;

   --  Test that processing the same AAD with varying calls to
   --  Update_Auth_Data produces the same tag.
   --
   --  This ensures that data can be streamed in any way and still
   --  produce the same result.
   procedure Test_Streaming_AAD (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      AAD : Byte_Array (1 .. 256);

      Tag1 : Byte_Array (1 .. 256);
      Tag2 : Byte_Array (1 .. 256);

      Offset    : Natural;
      Remaining : Natural;

   begin
      for I in 0 .. AAD'Length - 1 loop
         AAD (AAD'First + I) := Byte (I mod 256);
      end loop;

      --  Generate reference tag
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Update_Auth_Data (Ctx, AAD);
      MonkeyWrap.Extract_Tag (Ctx, Tag1);

      for Chunk_Size in 1 .. AAD'Length loop

         MonkeyWrap.Init (Ctx   => Ctx,
                          Key   => Key,
                          Nonce => Nonce);

         Offset    := 0;
         Remaining := AAD'Length;

         --  Process AAD in chunks
         while Remaining > 0 loop
            pragma Loop_Invariant (Offset + Remaining = AAD'Length);

            if Remaining >= Chunk_Size then
               MonkeyWrap.Update_Auth_Data
                  (Ctx  => Ctx,
                   Data => AAD (AAD'First + Offset .. AAD'First + Offset + Chunk_Size - 1));

               Offset    := Offset    + Chunk_Size;
               Remaining := Remaining - Chunk_Size;
            else
               MonkeyWrap.Update_Auth_Data
                  (Ctx  => Ctx,
                   Data => AAD (AAD'First + Offset .. AAD'Last));

               Offset    := Offset + Remaining;
               Remaining := 0;
            end if;
         end loop;

         --  Generate tag
         MonkeyWrap.Extract_Tag (Ctx, Tag2);

         Assert (Tag1 = Tag2, "Wrong tag for chunk size =" & Integer'Image (Chunk_Size));
      end loop;
   end Test_Streaming_AAD;

   --  Test that processing the same plaintext with varying calls to
   --  Update_Encrypt produces the same ciphertext.
   --
   --  This ensures that data can be streamed in any way and still
   --  produce the same result.
   procedure Test_Streaming_Encryption (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      PT : Byte_Array (1 .. 256);

      CT1 : Byte_Array (1 .. 256);
      CT2 : Byte_Array (1 .. 256);

      Tag1 : Byte_Array (1 .. 256);
      Tag2 : Byte_Array (1 .. 256);

      Offset    : Natural;
      Remaining : Natural;

   begin
      for I in 0 .. PT'Length - 1 loop
         PT (PT'First + I) := Byte (I mod 256);
      end loop;

      --  Generate reference ciphertext & tag
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Update_Encrypt (Ctx, PT, CT1);
      MonkeyWrap.Extract_Tag (Ctx, Tag1);

      for Chunk_Size in 1 .. PT'Length loop

         MonkeyWrap.Init (Ctx, Key, Nonce);

         Offset    := 0;
         Remaining := PT'Length;

         CT2 := (others => 0);

         --  Process plaintext in chunks
         while Remaining > 0 loop
            pragma Loop_Invariant (Offset + Remaining = PT'Length);

            if Remaining >= Chunk_Size then
               MonkeyWrap.Update_Encrypt
                  (Ctx        => Ctx,
                   Plaintext  => PT  (PT'First  + Offset .. PT'First  + Offset + Chunk_Size - 1),
                   Ciphertext => CT2 (CT2'First + Offset .. CT2'First + Offset + Chunk_Size - 1));

               Offset    := Offset    + Chunk_Size;
               Remaining := Remaining - Chunk_Size;
            else
               MonkeyWrap.Update_Encrypt
                  (Ctx  => Ctx,
                   Plaintext  => PT  (PT'First  + Offset .. PT'Last),
                   Ciphertext => CT2 (CT2'First + Offset .. CT2'Last));

               Offset    := Offset + Remaining;
               Remaining := 0;
            end if;
         end loop;

         --  Generate tag
         MonkeyWrap.Extract_Tag (Ctx, Tag2);

         Assert (CT1  = CT2,  "Wrong ciphertext for chunk size =" & Integer'Image (Chunk_Size));
         Assert (Tag1 = Tag2, "Wrong tag for chunk size =" & Integer'Image (Chunk_Size));
      end loop;
   end Test_Streaming_Encryption;

   --  Test that processing the same ciphertext with varying calls to
   --  Update_Decrypt produces the same ciphertext.
   --
   --  This ensures that data can be streamed in any way and still
   --  produce the same result.
   procedure Test_Streaming_Decryption (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      CT : Byte_Array (1 .. 256);

      PT1 : Byte_Array (1 .. 256);
      PT2 : Byte_Array (1 .. 256);

      Tag1 : Byte_Array (1 .. 256);
      Tag2 : Byte_Array (1 .. 256);

      Offset    : Natural;
      Remaining : Natural;

   begin
      for I in 0 .. CT'Length - 1 loop
         CT (CT'First + I) := Byte (I mod 256);
      end loop;

      --  Generate reference plaintext & tag
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Update_Decrypt (Ctx, CT, PT1);
      MonkeyWrap.Extract_Tag (Ctx, Tag1);

      for Chunk_Size in 1 .. CT'Length loop

         MonkeyWrap.Init (Ctx, Key, Nonce);

         Offset    := 0;
         Remaining := CT'Length;

         PT2 := (others => 0);

         --  Process plaintext in chunks
         while Remaining > 0 loop
            pragma Loop_Invariant (Offset + Remaining = CT'Length);

            if Remaining >= Chunk_Size then
               MonkeyWrap.Update_Decrypt
                  (Ctx        => Ctx,
                   Ciphertext => CT  (CT'First  + Offset .. CT'First  + Offset + Chunk_Size - 1),
                   Plaintext  => PT2 (PT2'First + Offset .. PT2'First + Offset + Chunk_Size - 1));

               Offset    := Offset    + Chunk_Size;
               Remaining := Remaining - Chunk_Size;
            else
               MonkeyWrap.Update_Decrypt
                  (Ctx        => Ctx,
                   Ciphertext => CT  (CT'First  + Offset .. CT'Last),
                   Plaintext  => PT2 (PT2'First + Offset .. PT2'Last));

               Offset    := Offset + Remaining;
               Remaining := 0;
            end if;
         end loop;

         --  Generate tag
         MonkeyWrap.Extract_Tag (Ctx, Tag2);

         Assert (PT1  = PT2,  "Wrong plaintext for chunk size =" & Integer'Image (Chunk_Size));
         Assert (Tag1 = Tag2, "Wrong tag for chunk size =" & Integer'Image (Chunk_Size));
      end loop;
   end Test_Streaming_Decryption;

   --  Test that extracting the same tag with varying calls to
   --  Extract_Tag produces the same ciphertext.
   --
   --  This ensures that data can be streamed in any way and still
   --  produce the same result.
   procedure Test_Streaming_Tag (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      Tag1 : Byte_Array (1 .. 256);
      Tag2 : Byte_Array (1 .. 256);

      Offset    : Natural;
      Remaining : Natural;

   begin
      --  Generate reference tag
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Extract_Tag (Ctx, Tag1);

      for Chunk_Size in 1 .. Tag1'Length loop

         MonkeyWrap.Init (Ctx   => Ctx,
                          Key   => Key,
                          Nonce => Nonce);

         Offset    := 0;
         Remaining := Tag1'Length;

         Tag2 := (others => 0);

         --  Process tag in chunks
         while Remaining > 0 loop
            pragma Loop_Invariant (Offset + Remaining = Tag1'Length);

            if Remaining >= Chunk_Size then
               MonkeyWrap.Extract_Tag
                  (Ctx => Ctx,
                   Tag => Tag2 (Tag2'First + Offset .. Tag2'First + Offset + Chunk_Size - 1));

               Offset    := Offset    + Chunk_Size;
               Remaining := Remaining - Chunk_Size;
            else
               MonkeyWrap.Extract_Tag
                  (Ctx => Ctx,
                   Tag => Tag2 (Tag2'First + Offset .. Tag2'Last));

               Offset    := Offset + Remaining;
               Remaining := 0;
            end if;
         end loop;

         Assert (Tag1 = Tag2, "Wrong tag for chunk size =" & Integer'Image (Chunk_Size));
      end loop;
   end Test_Streaming_Tag;

   --  Test that Verify_Tag produces the correct results for valid and
   --  invalid tags.
   procedure Test_Verify_Tag (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      Tag : Byte_Array (1 .. 256);

      Valid : Boolean;

   begin
      --  Generate reference tag
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Extract_Tag (Ctx, Tag);

      --  Check that the valid tag is detected as valid
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Verify_Tag (Ctx, Tag, Valid);
      Assert (Valid, "Verify_Tag false positive");

      --  Check that the tag is invalid for a completely wrong tag
      for B of Tag loop
         B := B xor 16#FF#;
      end loop;
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Verify_Tag (Ctx, Tag, Valid);
      Assert (not Valid, "Verify_Tag false negative");

      --  Put the tag back to normal
      for B of Tag loop
         B := B xor 16#FF#;
      end loop;

      --  Test that a corrupted bit in the middle of a streamed tag
      --  is detected, and that the Valid flag sticks to "invalid".
      Tag (180) := Tag (180) xor 16#08#;
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Verify_Tag (Ctx, Tag (1 .. 63), Valid);
      Assert (Valid, "Verify_Tag false positive in chunk 1");
      MonkeyWrap.Verify_Tag (Ctx, Tag (64 .. 127), Valid);
      Assert (Valid, "Verify_Tag false positive in chunk 2");
      MonkeyWrap.Verify_Tag (Ctx, Tag (128 .. 191), Valid); --  byte 180 is invalid
      Assert (not Valid, "Verify_Tag false negative in chunk 3");

      --  Valid should stick to false, even though the last chunk
      --  of data is still valid.
      MonkeyWrap.Verify_Tag (Ctx, Tag (192 .. 256), Valid);
      Assert (not Valid, "Verify_Tag false negative in chunk 4");

   end Test_Verify_Tag;

   --  Test that verifying the same tag with varying calls to
   --  Verify_Tag produces the same result.
   --
   --  This ensures that data can be streamed in any way and still
   --  produce the same result.
   procedure Test_Streaming_Verify_Tag (T : in out Test) is
      Ctx : MonkeyWrap.Context;

      Key   : constant Byte_Array (1 .. 8) := (others => 16#AA#);
      Nonce : constant Byte_Array (1 .. 8) := (others => 16#55#);

      Tag : Byte_Array (1 .. 256);

      Valid : Boolean;

      Offset    : Natural;
      Remaining : Natural;

   begin
      --  Generate reference tag
      MonkeyWrap.Init (Ctx, Key, Nonce);
      MonkeyWrap.Extract_Tag (Ctx, Tag);

      for Chunk_Size in 1 .. Tag'Length loop

         MonkeyWrap.Init (Ctx   => Ctx,
                          Key   => Key,
                          Nonce => Nonce);

         Offset    := 0;
         Remaining := Tag'Length;

         --  Process tag in chunks
         while Remaining > 0 loop
            pragma Loop_Invariant (Offset + Remaining = Tag'Length);

            if Remaining >= Chunk_Size then
               MonkeyWrap.Verify_Tag
                  (Ctx   => Ctx,
                   Tag   => Tag (Tag'First + Offset .. Tag'First + Offset + Chunk_Size - 1),
                   Valid => Valid);

               Offset    := Offset    + Chunk_Size;
               Remaining := Remaining - Chunk_Size;
            else
               MonkeyWrap.Verify_Tag
                  (Ctx   => Ctx,
                   Tag   => Tag (Tag'First + Offset .. Tag'Last),
                   Valid => Valid);

               Offset    := Offset + Remaining;
               Remaining := 0;
            end if;

            Assert
              (Valid,
               "Invalid tag at offset =" &
                 Integer'Image (Offset) &
                 ", chunk size =" &
                 Integer'Image (Chunk_Size));
         end loop;
      end loop;
   end Test_Streaming_Verify_Tag;

end Ketje_Tests;
