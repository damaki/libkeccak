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

with Keccak.Generic_MonkeyDuplex;
with Keccak.Types;

--  @summary
--  Generic implementation of the MonkeyWrap construction.
--
--  @description
--  MonkeyWrap is an authenticated encryption scheme built on MonkeyDuplex.
--  Ketje variants are instantiations of this package.
--
--  This API is used as follows:
--
--  1 Call Init, providing a secret encryption key and a unique
--    "number used once" (nonce).
--
--  2 Optionally call Update_Auth_Data zero or more times to provide
--    arbitrary-length additional data that is authenticated, but
--    is not encrypted.
--
--  3 Optionally call Update_Encrypt or Update_Decrypt (but not both)
--    zero or more times to encrypt/decrypt arbitrary-length data.
--
--  4 Optionally call Extract_Tag or Verify_Tag (but not both)
--    zero or more times to get or check an arbitrary-length
--    authentication tag respectively.
--
--  Ketje supports the concept of sessions, where sequences of messages
--  can be authenticated rather than a single message. The first session is
--  initialised by loading the key and nonce. At the end of each message,
--  the New_Session procedure can be called to begin a new session, which
--  puts the Context back to step 2 above.
generic
   Block_Size_Bytes : Positive;

   with package MonkeyDuplex is new Keccak.Generic_MonkeyDuplex (<>);
package Keccak.Generic_MonkeyWrap is

   Max_Rate_Bits : constant Positive :=
     MonkeyDuplex.State_Size_Bits - MonkeyDuplex.Min_Padding_Bits;
   --  Maximum possible rate of the underlying MonkeyDuplex.
   --
   --  E.g. for Keccak-p[1600] this is 1600 - 2 = 1598

   Max_Block_Size_Bits : constant Positive := Max_Rate_Bits - 2;
   --  MonkeyWrap specification requires: 0 < p <= b - 4
   --
   --  I.e. the block size does not exceed state size - 4 bits
   --  (2 bits padding, 2 bits domain separation).
   --
   --  E.g. for Keccak-p[1600] this is 1600 - 4 = 1596

   Max_Key_Size_Bits : constant Positive := MonkeyDuplex.State_Size_Bits - 18;
   --  The size of the key must fit in the underlying permutation's block size,
   --  minus 18 bits.
   --
   --  See Section 5.1 of the Ketje v2 specification.

   pragma Assert (Block_Size_Bytes <= Max_Block_Size_Bits / 8);

   type State is (Auth_Data,
                  Encrypting,
                  Extracting_Tag,
                  Decrypting,
                  Verifying_Tag);

   type Context is private;

   procedure Init (Ctx   :    out Context;
                   Key   : in     Keccak.Types.Byte_Array;
                   Nonce : in     Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Ctx => (Key, Nonce)),
     Pre => (Key'Length <= Max_Key_Size_Bits / 8
             and then Nonce'Length <= (Max_Key_Size_Bits - (Key'Length * 8)) / 8),
     Post => State_Of (Ctx) = Auth_Data;
   --  Initialise the MonkeyWrap context.
   --
   --  The combined Key + Nonce size, in bits, cannot exceed Max_Key_Size_Bits.
   --  For example, for Keccak-p[1600] the maximum Key + Nonce size is 1574 bits
   --  (196 bytes).

   procedure Update_Auth_Data (Ctx  : in out Context;
                               Data : in     Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Data),
     Pre => State_Of (Ctx) = Auth_Data,
     Post => State_Of (Ctx) = Auth_Data;
   --  Process additional authenticated data (AAD).
   --
   --  This procedure can be called multiple times to process a large amount
   --  of AAD (streaming input).

   procedure Update_Encrypt (Ctx        : in out Context;
                             Plaintext  : in     Keccak.Types.Byte_Array;
                             Ciphertext :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Ctx        =>+ Plaintext,
                 Ciphertext =>+ (Ctx, Plaintext)),
     Pre => (Plaintext'Length = Ciphertext'Length
             and (State_Of (Ctx) in Auth_Data | Encrypting)),
     Post => State_Of (Ctx) = Encrypting;
   pragma Annotate (GNATprove, False_Positive,
                    """Ciphertext"" might not be initialized",
                    "Ciphertext is fully initialised by end of procedure");
   --  Encrypt some plaintext and produce ciphertext.
   --
   --  This procedure can be called multiple times to process a large amount
   --  of streaming ciphertext (streaming encryption).

   procedure Update_Decrypt (Ctx        : in out Context;
                             Ciphertext : in     Keccak.Types.Byte_Array;
                             Plaintext  :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Ctx       =>+ (Plaintext, Ciphertext),
                 Plaintext =>+ (Ctx, Ciphertext)),
     Pre => (Plaintext'Length = Ciphertext'Length
             and (State_Of (Ctx) in Auth_Data | Decrypting)),
     Post => State_Of (Ctx) = Decrypting;
   pragma Annotate (GNATprove, False_Positive,
                    """Plaintext"" might not be initialized",
                    "Plaintext is fully initialised by end of procedure");

   procedure Extract_Tag (Ctx : in out Context;
                          Tag :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Tag,
                 Tag =>+ Ctx),
     Pre => (State_Of (Ctx) in Auth_Data | Encrypting | Decrypting | Extracting_Tag),
     Post => State_Of (Ctx) = Extracting_Tag;
   pragma Annotate (GNATprove, False_Positive,
                    """Tag"" might not be initialized",
                    "Tag is fully initialised by end of procedure");
   --  Produce the authentication tag.
   --
   --  This procedure can be called multiple times to produce a tag of
   --  arbitrary length (streaming output).

   procedure Verify_Tag (Ctx   : in out Context;
                         Tag   : in     Keccak.Types.Byte_Array;
                         Valid :    out Boolean)
     with Global => null,
     Depends => (Ctx   =>+ Tag,
                 Valid => (Ctx, Tag)),
     Pre => (State_Of (Ctx) in Auth_Data | Encrypting | Decrypting | Verifying_Tag),
     Post => State_Of (Ctx) = Verifying_Tag;
   --  Verify whether a tag is valid.
   --
   --  This procedure can be called multiple times to verify artbirarily long tags.
   --  At each call, Valid will be set to the result of the encryption so far.

   procedure New_Session (Ctx : in out Context)
     with Global => null,
     Depends => (Ctx =>+ null),
     Post => State_Of (Ctx) = Auth_Data;
   --  Begin a new session.
   --
   --  This can be called at any time to start a new session, but is normally
   --  called after generating or verify the tag. The ciphertexts and tags
   --  generated in all future sessions depends on all previous sessions before
   --  it.

   function State_Of (Ctx : in Context) return State
     with Global => null;
   --  Get the current context state.
   --
   --  The state determines which API functions can be used at a point in time.
   --  For example, when the Context is encrypting or decrypting data it is not
   --  possible to provide further additional authenticated data.

private

   Additional_Bits : constant Positive := MonkeyDuplex.Min_Padding_Bits + 2;

   subtype Block_Byte_Count is Natural range 0 .. Block_Size_Bytes;

   type Context is record
      Inner_Ctx     : MonkeyDuplex.Context;
      --  The inner MonkeyDuplex instance.

      Current_State : State;
      --  The current context state.

      In_Data         : Keccak.Types.Byte_Array (Block_Byte_Count);
      --  During AAD, this stores the last partial/full block of AAD.
      --
      --  During encryption/decryption, this stores the previous plaintext block.

      In_Data_Length  : Block_Byte_Count;
      --  During AAD, this is the size of the pending data in 'In_Data'.
      --
      --  During Encryption/Decryption, this is the number of pending plaintext
      --  bytes that are pending to be processed into MonkeyDuplex.
      --  The number of remaining Keystream bytes is Block_Size - In_Data_Length.
      --
      --  During tag phases, this is the number of tag bytes that have been
      --  consumed in the Keystream buffer.

      Keystream : Keccak.Types.Byte_Array (Block_Byte_Count);
      --  A block of keystream bits.
      --
      --  During encryption/decryption phases this stores the keystream data.
      --
      --  During the tag extraction/verification phases this stores remainder
      --  tag bytes.

      Tag_Accumulator : Keccak.Types.Byte;
      --  An accumulator used during constant-time tag verification.
      --
      --  This starts of as zero, and during comparison the XOR of each
      --  byte is ORed into this value, thereby accumulating differences.
      --  If, at the end of verification this value is zero then the
      --  expected tag and actual tag are identical. Otherwise, non-zero
      --  values indicate at least 1 bit is different between the two
      --  tags.

   end record
     with Predicate =>
       (In_Data_Length <= Block_Size_Bytes
        and Block_Size_Bytes = (MonkeyDuplex.Rate_Of (Inner_Ctx) - Additional_Bits) / 8
        and (MonkeyDuplex.Rate_Of (Inner_Ctx) - Additional_Bits) mod 8 = 0);

   function State_Of (Ctx : in Context) return State is
     (Ctx.Current_State);

end Keccak.Generic_MonkeyWrap;
