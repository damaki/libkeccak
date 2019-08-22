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

with Keccak.Generic_Sponge;
with Keccak.Types;

--  @summary
--  A generic implementation of a hash algorithm based on the Sponge
--  cryptographic primitive.
--
--  @description
--  This package is the basis for each of the four SHA-3 hashing algorithms
--  described in Section 6.1 of NIST FIPS-202 (August 2015).
--
--  This API is used as follows:
--
--  1 Initialise a context by calling Init.
--
--  2 Call Update one or more times to input an arbitrary number of bytes into
--    the hash algorithm.
--
--  3 Call Final to generate the final hash digest.
--
--  @group Hash
generic
   with package Hash_Sponge is new Keccak.Generic_Sponge (<>);

   Digest_Size : Positive;
   --  Output digest size in bits. E.g. for SHA3-256 Digest_Size=256

   Capacity : Positive := Digest_Size * 2;
   --  The capacity of the hash. By default the capacity is twice the digest size.
   --
   --  The Rate is calculated as Hash_Sponge.State_Size - Capacity, and the value
   --  for the capacity must be chosen so that the rate is a multiple of 8.

   Suffix      : Keccak.Types.Byte;
   --  Up to 8 bits that are appended to the message before padding bits.
   --
   --  The least significant bit is the first bit that is appended.

   Suffix_Size : Natural;
   --  The number of bits in the Suffix to append. This must be a value
   --  in the range 0 .. 8.

   Permutation_Initial_Value : Keccak.Types.Byte_Array := Keccak.Types.Null_Byte_Array;
   --  An optional inital value for the permutation state.
   --
   --  If non-empty, this data block will be written to the permutation state
   --  and then the permutation function will be applied. Otherwise, the
   --  permutation state will be zero-initialised.
   --
   --  The length of this parameter cannot exceed the permutation state size.

package Keccak.Generic_Hash
is
   --  Import common types from Keccak.Types to avoid users of the
   --  package to be dependent on Keccak.Types.

   subtype Byte is Keccak.Types.Byte;

   subtype Index_Number is Keccak.Types.Index_Number;
   subtype Byte_Array   is Keccak.Types.Byte_Array;

   subtype Digest_Index is Keccak.Types.Index_Number
   range 0 .. (Keccak.Types.Index_Number (Digest_Size) / 8) - 1;

   subtype Digest_Type is Keccak.Types.Byte_Array (Digest_Index);

   subtype Rate_Bits_Number is Hash_Sponge.Rate_Bits_Number;

   type States is (Updating, Ready_To_Finish, Finished);
   --  The possible states for the context.
   --
   --  @value Updating When in this state the context can be fed
   --  with input data by calling the Update procedure.
   --
   --  @value Ready_To_Finish When in this state the Update procedure can
   --  no longer be called (i.e. no more data can be input to the context),
   --  and the context is ready to compute the final hash value.
   --
   --  @value Finished When in this state the final hash value has been computed
   --  and the context can not be used further. However, the context could be
   --  re-initialized and used again for a new hash computation.

   type Context is private;

   -----------------------
   --  Hash procedures  --
   -----------------------

   procedure Init (Ctx : out Context)
     with Global => null,
     Depends => (Ctx => null),
     Post => State_Of (Ctx) = Updating;
   --  Initializes the hash context.
   --
   --  The context is initially in the Updating state.

   procedure Update (Ctx        : in out Context;
                     Message    : in     Byte_Array;
                     Bit_Length : in     Natural)
     with Global => null,
     Depends => (Ctx =>+ (Message, Bit_Length)),
     Pre => (State_Of (Ctx) = Updating
             and then (Message'Length < Natural'Last / 8)
             and then Bit_Length <= Message'Length * 8),
     Post => (Rate_Of (Ctx) = Rate_Of (Ctx'Old)),
     Contract_Cases => (Bit_Length mod 8 = 0 => State_Of (Ctx) = Updating,
                        others               => State_Of (Ctx) = Ready_To_Finish);
   --  Add bit-oriented messages to the hash computation
   --
   --  This procedure can be called multiple times to process
   --  large amounts of data in chunks. However, for all calls before the last
   --  call the Bit_Length parameter must be a multiple of 8. Once Update has
   --  been called where Bit_Length is not a multiple of 8, then the context
   --  moves to the Ready_To_Finish state where it can no longer accept calls
   --  to Update.
   --
   --  @param Ctx The hash context to update.
   --  @param Message The bytes to hash.
   --  @param Bit_Length The number of bits to hash from the Message array.
   --  Any additional bits in the Message array past this length are ignored.

   procedure Update (Ctx     : in out Context;
                     Message : in     Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Message),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Updating;
   --  Add byte-oriented messages to the hash computation.
   --
   --  This procedure can be called multiple times to process large amounts
   --  of data in chunks.
   --
   --  @param Ctx The hash context to update.
   --  @param Message The bytes to hash.

   procedure Final (Ctx    : in out Context;
                    Digest :    out Digest_Type)
     with Global => null,
     Depends => ((Digest, Ctx) => Ctx),
     Pre => State_Of (Ctx) in Updating | Ready_To_Finish,
     Post => (State_Of (Ctx) = Finished
              and Rate_Of (Ctx) = Rate_Of (Ctx'Old));
   --  Finish the hash context and get the digest (hash).
   --
   --  Note that after Final is called the context cannot be used
   --  further (the context is finished). However, the context can
   --  be re-initialized and used again for a new SHA3 computation
   --  by calling Init.
   --
   --  @param Ctx The hash context.
   --  @param Digest The computed digest (hash) is output in this parameter.

   function State_Of (Ctx : in Context) return States
     with Global => null;
   --  Get the current state of the context.
   --
   --  @return The context's current state.

   function Rate_Of (Ctx : in Context) return Rate_Bits_Number
     with Global => null;
   --  Get the current rate (in bits) of the context.
   --
   --  @return The rate of the context.

private
   use type Hash_Sponge.States;

   type Context is record
      Sponge_Ctx      : Hash_Sponge.Context;
      Update_Complete : Boolean;
   end record;

   function In_Queue_Bit_Length (Ctx : in Context) return Natural
   is (Hash_Sponge.In_Queue_Bit_Length (Ctx.Sponge_Ctx));

   function Rate_Of (Ctx : in Context) return Rate_Bits_Number
   is (Hash_Sponge.Rate_Of (Ctx.Sponge_Ctx));

   function Can_Absorb (Ctx : in Context) return Boolean
   is (In_Queue_Bit_Length (Ctx) mod 8 = 0
       and In_Queue_Bit_Length (Ctx) < Rate_Of (Ctx));

   function State_Of (Ctx : in Context) return States
   is (if Hash_Sponge.State_Of (Ctx.Sponge_Ctx) = Hash_Sponge.Squeezing then Finished
       elsif Can_Absorb (Ctx) and not Ctx.Update_Complete then Updating
       else Ready_To_Finish);

end Keccak.Generic_Hash;
