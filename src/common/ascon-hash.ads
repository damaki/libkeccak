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

with Ascon.Sponge;
with Keccak.Types; use Keccak.Types;

--  @summary
--  Implementation of the Ascon-Hash algorithm.
--
--  @description
--  This implementation is very similar to Keccak.Generic_Hash, but with the following
--  modifications specific to the the Ascon-Hash specification:
--    1. An IV is used to initialise the sponge state based on the rate,
--       the number of rounds (a), and the maximum output length in bits.
--    2. No suffix bits are appended.
--    3. The capacity is fixed to 256 bits.
package Ascon.Hash
with SPARK_Mode => On
is

   subtype Digest_Type is Keccak.Types.Byte_Array (1 .. 32);

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
     Post => State_Of (Ctx) = Finished;
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

private
   use type Ascon.Sponge.States;

   Rate_Bits : constant := 64;

   type Context is record
      Sponge_Ctx      : Ascon.Sponge.Context;
      Update_Complete : Boolean;
   end record
     with Predicate => Ascon.Sponge.Rate_Of (Sponge_Ctx) = Rate_Bits;

   function In_Queue_Bit_Length (Ctx : in Context) return Natural
   is (Ascon.Sponge.In_Queue_Bit_Length (Ctx.Sponge_Ctx));

   function Can_Absorb (Ctx : in Context) return Boolean
   is (In_Queue_Bit_Length (Ctx) mod 8 = 0
       and In_Queue_Bit_Length (Ctx) < Rate_Bits);

   function State_Of (Ctx : in Context) return States
   is (if Ascon.Sponge.State_Of (Ctx.Sponge_Ctx) = Ascon.Sponge.Squeezing then Finished
       elsif Can_Absorb (Ctx) and not Ctx.Update_Complete then Updating
       else Ready_To_Finish);

end Ascon.Hash;
