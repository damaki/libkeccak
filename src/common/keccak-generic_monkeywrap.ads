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
     Pre => (Key'Length <= (Max_Rate_Bits / 8) - 3
             and then Nonce'Length <= Max_Rate_Bits / 8
             and then Key'Length + 3 + Nonce'Length <= Max_Rate_Bits / 8),
     Post => State_Of (Ctx) = Auth_Data;
   --  Initialise the MonkeyWrap context.
   --
   --  The combined Key + Nonce length cannot exceed the maximum rate of the
   --  underlying MonkeyDuplex instance. For example, for Keccak-p[1600] the
   --  maximum Key + Nonce size is 1574 bits.

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

   function State_Of (Ctx : in Context) return State
     with Global => null;

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
      --  During encryption, this stores the last plaintext block.

      In_Data_Length  : Block_Byte_Count;
      --  During AAD, this is the size of the pending data in 'In_Data'.
      --
      --  During Encryption/Decryption, this is the number of pending plaintext
      --  bytes that are pending to be processed into MonkeyDuplex.
      --  The number of remaining Keystream bytes is Block_Size - In_Data_Length.

      Keystream : Keccak.Types.Byte_Array (Block_Byte_Count);
      --  A block of keystream bits.
      --
      --  This is only valid during the encryption/decryption phases.

      Tag_Accumulator : Keccak.Types.Byte;

   end record
     with Predicate =>
       (In_Data_Length <= Block_Size_Bytes
        and Block_Size_Bytes = (MonkeyDuplex.Rate_Of (Inner_Ctx) - Additional_Bits) / 8
        and (MonkeyDuplex.Rate_Of (Inner_Ctx) - Additional_Bits) mod 8 = 0);

   function State_Of (Ctx : in Context) return State is
     (Ctx.Current_State);

end Keccak.Generic_MonkeyWrap;
