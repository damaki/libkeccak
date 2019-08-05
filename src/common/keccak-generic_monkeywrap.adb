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

with Interfaces;  use Interfaces;
with Keccak.Util; use Keccak.Util;

package body Keccak.Generic_MonkeyWrap is

   Frame_Bits_00 : constant Keccak.Types.Byte := 2#00#;
   Frame_Bits_01 : constant Keccak.Types.Byte := 2#10#;
   Frame_Bits_11 : constant Keccak.Types.Byte := 2#11#;
   Frame_Bits_10 : constant Keccak.Types.Byte := 2#01#;
   Frame_Bits_0  : constant Keccak.Types.Byte := 2#0#;

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx   :    out Context;
                   Key   : in     Keccak.Types.Byte_Array;
                   Nonce : in     Keccak.Types.Byte_Array) is

      Block : Keccak.Types.Byte_Array (0 .. 1 + Key'Length + Nonce'Length);
      --  Combines the packed Key & Nonce.

      Rate : MonkeyDuplex.Rate_Bits_Number;

   begin
      --  keypack(Key, |Key| + 16)
      Block (0)               := Key'Length + 2;
      Block (1 .. Key'Length) := Key;
      Block (Key'Length + 1)  := 16#01#;

      Block (Key'Length + 2 .. Block'Last) := Nonce;

      Rate := (Block_Size_Bytes * 8) + MonkeyDuplex.Min_Padding_Bits + 2;

      Ctx := (Inner_Ctx       => MonkeyDuplex.Start (Rate     => Rate,
                                                     Data     => Block,
                                                     Bit_Len  => Block'Length * 8),
              Current_State   => Auth_Data,
              In_Data         => (others => 0),
              In_Data_Length  => 0,
              Keystream       => (others => 0),
              Tag_Accumulator => 0);

      pragma Annotate (GNATprove, False_Positive,
                       """Block"" might not be initialized",
                       "Block is initialised in parts, which confuses GNATprove");
   end Init;

   ------------------------
   --  Update_Auth_Data  --
   ------------------------

   procedure Update_Auth_Data (Ctx  : in out Context;
                               Data : in     Keccak.Types.Byte_Array) is

      Offset    : Natural := 0;
      Remaining : Natural := Data'Length;

      Pos : Natural;

      Remaining_In_Chunk : Natural;

      Initial_In_Data_Length : constant Block_Byte_Count := Ctx.In_Data_Length with Ghost;

   begin
      --  Concatenate the data with the previous leftover bytes.
      if Ctx.In_Data_Length > 0 then
         Remaining_In_Chunk := Block_Size_Bytes - Ctx.In_Data_Length;

         if Remaining >= Remaining_In_Chunk then
            Ctx.In_Data (Ctx.In_Data_Length .. Ctx.In_Data_Length + (Remaining_In_Chunk - 1)) :=
              Data (Data'First .. Data'First + (Remaining_In_Chunk - 1));

            Offset             := Offset             + Remaining_In_Chunk;
            Remaining          := Remaining          - Remaining_In_Chunk;
            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining_In_Chunk;

            pragma Assert (Ctx.In_Data_Length = Block_Size_Bytes);

            if Remaining > 0 then
               MonkeyDuplex.Step_Mute (Ctx                 => Ctx.Inner_Ctx,
                                       In_Data             => Ctx.In_Data,
                                       In_Data_Bit_Length  => Block_Size_Bytes * 8,
                                       Suffix              => Frame_Bits_00,
                                       Suffix_Bit_Length   => 2);

               Ctx.In_Data_Length := 0;
            end if;

         else
            Ctx.In_Data (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining - 1) := Data;
            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining;

            Offset    := Remaining;
            Remaining := 0;
         end if;
      end if;

      pragma Assert (if Remaining = 0 then Ctx.In_Data_Length >= Initial_In_Data_Length);

      --  Process complete blocks, but don't process last block if it is
      --  a complete block.
      while Remaining > Block_Size_Bytes loop
         pragma Loop_Variant (Increases => Offset,
                              Decreases => Remaining);
         pragma Loop_Invariant (Ctx.In_Data_Length = 0
                                and Offset + Remaining = Data'Length);

         Pos := Data'First + Offset;

         MonkeyDuplex.Step_Mute
           (Ctx                => Ctx.Inner_Ctx,
            In_Data            => Data (Pos .. Pos + Block_Size_Bytes - 1),
            In_Data_Bit_Length => Block_Size_Bytes * 8,
            Suffix             => Frame_Bits_00,
            Suffix_Bit_Length  => 2);

         Offset    := Offset    + Block_Size_Bytes;
         Remaining := Remaining - Block_Size_Bytes;
      end loop;

      --  Save the last (partial or full) block.
      --  Even if this is a full block, we don't process it yet because we
      --  don't yet know if this is the last block of authenticated data or not
      --  which determines the suffix value to use.
      --  (there may be additional calls to Update_Auth_Data).
      if Remaining > 0 then
         pragma Assert (Remaining <= Block_Size_Bytes);

         Ctx.In_Data_Length := Remaining;
         Ctx.In_Data (0 .. Remaining - 1) := Data (Data'First + Offset .. Data'Last);
      end if;

   end Update_Auth_Data;

   ----------------------
   --  Update_Encrypt  --
   ----------------------

   procedure Update_Encrypt (Ctx        : in out Context;
                             Plaintext  : in     Keccak.Types.Byte_Array;
                             Ciphertext :    out Keccak.Types.Byte_Array) is

      Offset    : Natural := 0;
      Remaining : Natural := Plaintext'Length;

      Remaining_Keystream : Natural;

      PT_Pos : Keccak.Types.Index_Number;
      CT_Pos : Keccak.Types.Index_Number;

   begin

      --  Process last block of AAD, if needed.
      if Ctx.Current_State = Auth_Data then
         --  Process final block of auth data to generate first Keystream bits.

         MonkeyDuplex.Step (Ctx                 => Ctx.Inner_Ctx,
                            In_Data             => Ctx.In_Data,
                            In_Data_Bit_Length  => Ctx.In_Data_Length * 8,
                            Suffix              => Frame_Bits_01,
                            Suffix_Bit_Length   => 2,
                            Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                            Out_Data_Bit_Length => Block_Size_Bytes * 8);

         Ctx.In_Data_Length := 0;
         Ctx.Current_State  := Encrypting;
      end if;

      pragma Assert (Offset + Remaining = Plaintext'Length);

      --  Concatenate plaintext with leftover plaintext from previous call to Encrypt.
      if Ctx.In_Data_Length > 0 and Remaining > 0 then
         Remaining_Keystream := Block_Size_Bytes - Ctx.In_Data_Length;

         if Remaining_Keystream <= Plaintext'Length then

            --  Use all remaining keystream bytes to produce the ciphertext.
            for I in 0 .. Remaining_Keystream - 1 loop
               Ciphertext (Ciphertext'First + I) :=
                 Plaintext (Plaintext'First + I)
                 xor Ctx.Keystream (Ctx.In_Data_Length + I);
            end loop;

            --  Concatenate the plaintext with the previous leftover plaintext

            PT_Pos := Plaintext'First + Offset;

            Ctx.In_Data (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining_Keystream - 1) :=
              Plaintext (PT_Pos .. PT_Pos + Remaining_Keystream - 1);

            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining_Keystream;
            Offset             := Offset             + Remaining_Keystream;
            Remaining          := Remaining          - Remaining_Keystream;

         else

            for I in 0 .. Remaining - 1 loop
               Ciphertext (Ciphertext'First + I) :=
                 Plaintext (Plaintext'First + I)
                 xor Ctx.Keystream (Ctx.In_Data_Length + I);
            end loop;

            PT_Pos := Plaintext'First + Offset;

            Ctx.In_Data (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining - 1) :=
              Plaintext (PT_Pos .. PT_Pos + Remaining - 1);

            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining;
            Offset             := Offset + Remaining;
            Remaining          := 0;

         end if;
      end if;

      if Remaining > 0 then
         pragma Assert (Ctx.In_Data_Length in 0 | Block_Size_Bytes);

         if Ctx.In_Data_Length = Block_Size_Bytes then
            MonkeyDuplex.Step
              (Ctx                 => Ctx.Inner_Ctx,
               In_Data             => Ctx.In_Data (0 .. Block_Size_Bytes - 1),
               In_Data_Bit_Length  => Block_Size_Bytes * 8,
               Suffix              => Frame_Bits_11,
               Suffix_Bit_Length   => 2,
               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Ctx.In_Data_Length := 0;
         end if;

         --  Process full blocks.
         while Remaining > Block_Size_Bytes loop
            pragma Loop_Variant (Increases => Offset,
                                 Decreases => Remaining);
            pragma Loop_Invariant (Offset + Remaining = Plaintext'Length
                                   and Ctx.In_Data_Length = 0);

            PT_Pos := Plaintext'First  + Offset;
            CT_Pos := Ciphertext'First + Offset;

            for I in 0 .. Block_Size_Bytes - 1 loop
               Ciphertext (CT_Pos + I) := Plaintext (PT_Pos + I) xor Ctx.Keystream (I);
            end loop;

            MonkeyDuplex.Step
              (Ctx                 => Ctx.Inner_Ctx,
               In_Data             => Plaintext (PT_Pos .. PT_Pos + Block_Size_Bytes - 1),
               In_Data_Bit_Length  => Block_Size_Bytes * 8,
               Suffix              => Frame_Bits_11,
               Suffix_Bit_Length   => 2,
               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Offset    := Offset    + Block_Size_Bytes;
            Remaining := Remaining - Block_Size_Bytes;
         end loop;

         --  Process last block.
         if Remaining > 0 then

            PT_Pos := Plaintext'First  + Offset;
            CT_Pos := Ciphertext'First + Offset;

            for I in 0 .. Remaining - 1 loop
               Ciphertext (CT_Pos + I) := Plaintext (PT_Pos + I) xor Ctx.Keystream (I);
            end loop;

            --  Store last partial/full block of plaintext.
            --
            --  Even if we have a full block, we can't process it with MonkeyDuplex
            --  yet because we don't know if this is the last plaintext block.

            Ctx.In_Data (0 .. Remaining - 1) := Plaintext (PT_Pos .. Plaintext'Last);
            Ctx.In_Data_Length := Remaining;

         end if;
      end if;

   end Update_Encrypt;

   ----------------------
   --  Update_Decrypt  --
   ----------------------

   procedure Update_Decrypt (Ctx        : in out Context;
                             Ciphertext : in     Keccak.Types.Byte_Array;
                             Plaintext  :    out Keccak.Types.Byte_Array) is

      Offset    : Natural := 0;
      Remaining : Natural := Plaintext'Length;

      Remaining_Keystream : Natural;

      PT_Pos : Keccak.Types.Index_Number;
      CT_Pos : Keccak.Types.Index_Number;

   begin

      --  Process last block of AAD, if needed.
      if Ctx.Current_State = Auth_Data then
         --  Process final block of auth data to generate first Keystream bits.

         MonkeyDuplex.Step (Ctx                 => Ctx.Inner_Ctx,
                            In_Data             => Ctx.In_Data,
                            In_Data_Bit_Length  => Ctx.In_Data_Length * 8,
                            Suffix              => Frame_Bits_01,
                            Suffix_Bit_Length   => 2,
                            Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                            Out_Data_Bit_Length => Block_Size_Bytes * 8);

         Ctx.In_Data_Length := 0;
         Ctx.Current_State  := Decrypting;
      end if;

      pragma Assert (Offset + Remaining = Plaintext'Length);

      --  Concatenate plaintext with leftover plaintext from previous call to Encrypt.
      if Ctx.In_Data_Length > 0 and Remaining > 0 then
         Remaining_Keystream := Block_Size_Bytes - Ctx.In_Data_Length;

         if Remaining_Keystream <= Plaintext'Length then

            --  Use all remaining keystream bytes to produce the ciphertext.
            for I in 0 .. Remaining_Keystream - 1 loop
               Plaintext (Plaintext'First + I) :=
                 Ciphertext (Ciphertext'First + I)
                 xor Ctx.Keystream (Ctx.In_Data_Length + I);
            end loop;

            --  Concatenate the plaintext with the previous leftover plaintext

            PT_Pos := Plaintext'First + Offset;

            Ctx.In_Data (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining_Keystream - 1) :=
              Plaintext (PT_Pos .. PT_Pos + Remaining_Keystream - 1);

            pragma Annotate (GNATprove, False_Positive,
                             """Plaintext"" might not be initialized",
                             "The plaintext slice used is initialized in the preceding loop");

            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining_Keystream;
            Offset             := Offset             + Remaining_Keystream;
            Remaining          := Remaining          - Remaining_Keystream;

         else

            for I in 0 .. Remaining - 1 loop
               Plaintext (Plaintext'First + I) :=
                 Ciphertext (Ciphertext'First + I)
                 xor Ctx.Keystream (Ctx.In_Data_Length + I);
            end loop;

            PT_Pos := Plaintext'First + Offset;

            Ctx.In_Data (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining - 1) :=
              Plaintext (PT_Pos .. PT_Pos + Remaining - 1);

            pragma Annotate (GNATprove, False_Positive,
                             """Plaintext"" might not be initialized",
                             "The plaintext slice used is initialized in the preceding loop");

            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining;
            Offset             := Offset + Remaining;
            Remaining          := 0;

         end if;
      end if;

      if Remaining > 0 then
         pragma Assert (Ctx.In_Data_Length in 0 | Block_Size_Bytes);

         if Ctx.In_Data_Length = Block_Size_Bytes then
            MonkeyDuplex.Step
              (Ctx                 => Ctx.Inner_Ctx,
               In_Data             => Ctx.In_Data (0 .. Block_Size_Bytes - 1),
               In_Data_Bit_Length  => Block_Size_Bytes * 8,
               Suffix              => Frame_Bits_11,
               Suffix_Bit_Length   => 2,
               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Ctx.In_Data_Length := 0;
         end if;

         --  Process full blocks.
         while Remaining > Block_Size_Bytes loop
            pragma Loop_Variant (Increases => Offset,
                                 Decreases => Remaining);
            pragma Loop_Invariant (Offset + Remaining = Plaintext'Length
                                   and Ctx.In_Data_Length = 0);

            PT_Pos := Plaintext'First  + Offset;
            CT_Pos := Ciphertext'First + Offset;

            for I in 0 .. Block_Size_Bytes - 1 loop
               Plaintext (PT_Pos + I) := Ciphertext (CT_Pos + I) xor Ctx.Keystream (I);
            end loop;

            MonkeyDuplex.Step
              (Ctx                 => Ctx.Inner_Ctx,
               In_Data             => Plaintext (PT_Pos .. PT_Pos + Block_Size_Bytes),
               In_Data_Bit_Length  => Block_Size_Bytes * 8,
               Suffix              => Frame_Bits_11,
               Suffix_Bit_Length   => 2,
               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            pragma Annotate (GNATprove, False_Positive,
                             """Plaintext"" might not be initialized",
                             "The plaintext slice used is initialized in the preceding loop");

            Offset    := Offset    + Block_Size_Bytes;
            Remaining := Remaining - Block_Size_Bytes;
         end loop;

         --  Process last block.
         if Remaining > 0 then

            PT_Pos := Plaintext'First  + Offset;
            CT_Pos := Ciphertext'First + Offset;

            for I in 0 .. Remaining - 1 loop
               Plaintext (PT_Pos + I) := Ciphertext (CT_Pos + I) xor Ctx.Keystream (I);
            end loop;

            --  Store last partial/full block of plaintext.
            --
            --  Even if we have a full block, we can't process it with MonkeyDuplex
            --  yet because we don't know if this is the last plaintext block.

            Ctx.In_Data (0 .. Remaining - 1) := Plaintext (PT_Pos .. Plaintext'Last);

            pragma Annotate (GNATprove, False_Positive,
                             """Plaintext"" might not be initialized",
                             "The plaintext slice used is initialized in the preceding loop");

            Ctx.In_Data_Length := Remaining;

         end if;
      end if;

   end Update_Decrypt;

   -------------------
   --  Extract_Tag  --
   -------------------

   procedure Extract_Tag (Ctx : in out Context;
                          Tag :    out Keccak.Types.Byte_Array) is

      Offset    : Natural := 0;
      Remaining : Natural := Tag'Length;

      Pos : Keccak.Types.Index_Number;

      Remaining_Output : Natural;

   begin
      if Ctx.Current_State in Auth_Data | Encrypting | Decrypting then

         if Ctx.Current_State = Auth_Data then
            --  Finish auth data stage
            MonkeyDuplex.Step_Mute
              (Ctx                 => Ctx.Inner_Ctx,
               In_Data             => Ctx.In_Data,
               In_Data_Bit_Length  => Ctx.In_Data_Length * 8,
               Suffix              => Frame_Bits_01,
               Suffix_Bit_Length   => 2);

            Ctx.In_Data_Length := 0;
         end if;

         --  Finish encryption/decryption stage.
         MonkeyDuplex.Stride (Ctx                 => Ctx.Inner_Ctx,
                              In_Data             => Ctx.In_Data,
                              In_Data_Bit_Length  => Ctx.In_Data_Length * 8,
                              Suffix              => Frame_Bits_10,
                              Suffix_Bit_Length   => 2,
                              Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                              Out_Data_Bit_Length => Block_Size_Bytes * 8);

         Ctx.In_Data_Length := 0;
         Ctx.Current_State  := Extracting_Tag;
      end if;

      Remaining_Output := Block_Size_Bytes - Ctx.In_Data_Length;

      if Remaining > 0 then
         --  First, take from the previous leftovers.
         if Remaining < Remaining_Output then
            Tag := Ctx.Keystream (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining - 1);
            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining;

            Offset    := Remaining;
            Remaining := 0;
         else

            Pos := Tag'First + Offset;
            Tag (Pos .. Pos + Remaining_Output - 1) :=
              Ctx.Keystream (Ctx.In_Data_Length .. Ctx.In_Data_Length + Remaining_Output - 1);

            Ctx.In_Data_Length := Block_Size_Bytes;

            Offset    := Offset    + Remaining_Output;
            Remaining := Remaining - Remaining_Output;
         end if;

         --  Process full blocks
         while Remaining >= Block_Size_Bytes loop
            pragma Loop_Variant (Increases => Offset,
                                 Decreases => Remaining);
            pragma Loop_Invariant (Offset + Remaining = Tag'Length);

            Pos := Tag'First + Offset;

            MonkeyDuplex.Step (Ctx                 => Ctx.Inner_Ctx,
                               In_Data             => Ctx.In_Data,
                               In_Data_Bit_Length  => 0,
                               Suffix              => Frame_Bits_0,
                               Suffix_Bit_Length   => 1,
                               Out_Data            => Tag (Pos .. Pos + Block_Size_Bytes - 1),
                               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Offset    := Offset    + Block_Size_Bytes;
            Remaining := Remaining - Block_Size_Bytes;
         end loop;

         --  Generate last partial block
         if Remaining > 0 then
            MonkeyDuplex.Step (Ctx                 => Ctx.Inner_Ctx,
                               In_Data             => Ctx.In_Data,
                               In_Data_Bit_Length  => 0,
                               Suffix              => Frame_Bits_0,
                               Suffix_Bit_Length   => 1,
                               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Tag (Tag'First + Offset .. Tag'Last) := Ctx.Keystream (0 .. Remaining - 1);

            Ctx.In_Data_Length := Remaining;
         end if;
      end if;
   end Extract_Tag;

   ------------------
   --  Verify_Tag  --
   ------------------

   procedure Verify_Tag (Ctx   : in out Context;
                         Tag   : in     Keccak.Types.Byte_Array;
                         Valid :    out Boolean) is

      Offset    : Natural := 0;
      Remaining : Natural := Tag'Length;

      Tag_Pos : Keccak.Types.Index_Number;
      KS_Pos  : Keccak.Types.Index_Number;

      Remaining_Output : Natural;
   begin
      if Ctx.Current_State in Auth_Data | Encrypting | Decrypting then

         if Ctx.Current_State = Auth_Data then
            --  Finish auth data stage
            MonkeyDuplex.Step_Mute
              (Ctx                 => Ctx.Inner_Ctx,
               In_Data             => Ctx.In_Data,
               In_Data_Bit_Length  => Ctx.In_Data_Length * 8,
               Suffix              => Frame_Bits_01,
               Suffix_Bit_Length   => 2);

            Ctx.In_Data_Length := 0;
         end if;

         --  Finish encryption/decryption stage.
         MonkeyDuplex.Stride (Ctx                 => Ctx.Inner_Ctx,
                              In_Data             => Ctx.In_Data,
                              In_Data_Bit_Length  => Ctx.In_Data_Length * 8,
                              Suffix              => Frame_Bits_10,
                              Suffix_Bit_Length   => 2,
                              Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                              Out_Data_Bit_Length => Block_Size_Bytes * 8);

         Ctx.In_Data_Length  := 0;
         Ctx.Current_State   := Verifying_Tag;
         Ctx.Tag_Accumulator := 0;
      end if;

      Remaining_Output := Block_Size_Bytes - Ctx.In_Data_Length;

      if Remaining > 0 then
         --  First, take from the previous leftovers.

         Tag_Pos := Tag'First + Offset;
         KS_Pos  := Ctx.In_Data_Length;

         if Remaining < Remaining_Output then
            Compare (A1          => Tag           (Tag_Pos .. Tag_Pos + Remaining - 1),
                     A2          => Ctx.Keystream (KS_Pos  .. KS_Pos  + Remaining - 1),
                     Accumulator => Ctx.Tag_Accumulator);

            Ctx.In_Data_Length := Ctx.In_Data_Length + Remaining;

            Offset    := Remaining;
            Remaining := 0;
         else
            Compare (A1          => Tag           (Tag_Pos .. Tag_Pos + Remaining_Output - 1),
                     A2          => Ctx.Keystream (KS_Pos  .. KS_Pos  + Remaining_Output - 1),
                     Accumulator => Ctx.Tag_Accumulator);

            Ctx.In_Data_Length := Block_Size_Bytes;

            Offset    := Offset    + Remaining_Output;
            Remaining := Remaining - Remaining_Output;
         end if;

         --  Process full blocks
         while Remaining >= Block_Size_Bytes loop
            pragma Loop_Variant (Increases => Offset,
                                 Decreases => Remaining);
            pragma Loop_Invariant (Offset + Remaining = Tag'Length);

            MonkeyDuplex.Step (Ctx                 => Ctx.Inner_Ctx,
                               In_Data             => Ctx.In_Data,
                               In_Data_Bit_Length  => 0,
                               Suffix              => Frame_Bits_0,
                               Suffix_Bit_Length   => 1,
                               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Tag_Pos := Tag'First + Offset;

            Compare (A1          => Tag (Tag_Pos .. Tag_Pos + Block_Size_Bytes - 1),
                     A2          => Ctx.Keystream (0  .. Block_Size_Bytes - 1),
                     Accumulator => Ctx.Tag_Accumulator);

            Offset    := Offset    + Block_Size_Bytes;
            Remaining := Remaining - Block_Size_Bytes;
         end loop;

         --  Generate last partial block
         if Remaining > 0 then
            MonkeyDuplex.Step (Ctx                 => Ctx.Inner_Ctx,
                               In_Data             => Ctx.In_Data,
                               In_Data_Bit_Length  => 0,
                               Suffix              => Frame_Bits_0,
                               Suffix_Bit_Length   => 1,
                               Out_Data            => Ctx.Keystream (0 .. Block_Size_Bytes - 1),
                               Out_Data_Bit_Length => Block_Size_Bytes * 8);

            Tag_Pos := Tag'First + Offset;

            Compare (A1          => Tag (Tag_Pos .. Tag_Pos + Remaining - 1),
                     A2          => Ctx.Keystream (0  .. Remaining - 1),
                     Accumulator => Ctx.Tag_Accumulator);

            Ctx.In_Data_Length := Remaining;
         end if;
      end if;

      Valid := Ctx.Tag_Accumulator = 0;
   end Verify_Tag;

   -------------------
   --  New_Session  --
   -------------------

   procedure New_Session (Ctx : in out Context) is
   begin
      Ctx.Current_State   := Auth_Data;
      Ctx.In_Data_Length  := 0;
      Ctx.Tag_Accumulator := 0;
   end New_Session;

end Keccak.Generic_MonkeyWrap;
