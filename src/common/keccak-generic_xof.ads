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

-------------------------------------------------------------------------------
--  This package provides a generic implementation for the SHAKE XOF.
--  It is the basis for each of the two SHAKE XOF algorithms
--  described in Section 6.2 of NIST FIPS-202 (August 2015).
--
--  The package provides three main procedures:
--     * Init to initialize the XOF state
--     * Update to append data to the XOF algorithm
--     * Extract to get an arbitrary amount of output bytes from the XOF.
-------------------------------------------------------------------------------

with Keccak.Generic_Sponge;
with Keccak.Types;

--  @summary
--  Generic eXtendable Output Function (XOF).
--
--  @group XOF
generic
   with package XOF_Sponge is new Keccak.Generic_Sponge (<>);

   Capacity      : Positive;
   --  Sponge capacity in bits.
   --
   --  This must be a multiple of 8, and must be smaller than the state size.

   Suffix        : Keccak.Types.Byte;

   Suffix_Size   : Natural;

   Permutation_Initial_Value : Keccak.Types.Byte_Array := Keccak.Types.Null_Byte_Array;
   --  An optional inital value for the permutation state.
   --
   --  If non-empty, this data block will be written to the permutation state
   --  and then the permutation function will be applied. Otherwise, the
   --  permutation state will be zero-initialised.
   --
   --  The length of this parameter cannot exceed the permutation state size.

package Keccak.Generic_XOF
is
   --  Import common types from Keccak.Types to avoid users of the
   --  package to be dependent on Keccak.Types.

   subtype Byte is Keccak.Types.Byte;

   subtype Index_Number is Keccak.Types.Index_Number;
   subtype Byte_Array   is Keccak.Types.Byte_Array;

   subtype Rate_Bits_Number is XOF_Sponge.Rate_Bits_Number;

   type States is (Updating, Ready_To_Extract, Extracting);

   type Context is private;

   ----------------------
   --  XOF procedures  --
   ----------------------

   procedure Init (Ctx : out Context)
     with Global => null,
     Depends => (Ctx => null),
     Post => State_Of (Ctx) = Updating;
   --  Initializes the XOF.
   --
   --  Initially, the XOF is in the Updating state; data can be input into the
   --  XOF by calling the Update procedure.
   --
   --  @param Ctx The context to initialize.

   procedure Update (Ctx        : in out Context;
                     Message    : in     Byte_Array;
                     Bit_Length : in     Natural)
     with Global => null,
     Depends => (Ctx =>+ (Message, Bit_Length)),
     Pre => (State_Of (Ctx) = Updating
             and then (Message'Length < Natural'Last / 8)
             and then Bit_Length <= Message'Length * 8),
     Contract_Cases => (Bit_Length mod 8 = 0 => State_Of (Ctx) = Updating,
                        others               => State_Of (Ctx) = Ready_To_Extract);
   --  Input bit-oriented data into the XOF.
   --
   --  This function can be called multiple times to input large amounts of
   --  data.
   --
   --  The XOF must be in the 'Updating' state when this procedure is called.
   --  Note that if Update is called where Bit_Length is not a multiple of 8
   --  bits then the XOF moves to the 'Ready_To_Extract' state and no additional
   --  bytes can be input into the XOF. Otherwise, if Bit_Length is a multiple of
   --  8 bits then Update can be called again to input additional data.
   --
   --  @param Ctx The XOF object into which the data is input.
   --
   --  @param Message Contains the data to input into the XOF object.
   --
   --  @param Bit_Length The number of bits from the 'Message' array to input
   --    into the XOF. Any additional bits in 'Message' after this number of
   --    bits are ignored. All calls to Update before the last call must have
   --    Bit_Length as a multiple of 8 bits. The last call to Update can have
   --    Bit_Length with any value.
   procedure Update (Ctx     : in out Context;
                     Message : in     Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Message),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Updating;
   --  Input byte-oriented data into the XOF.
   --
   --  This procedure can be called multiple times to process large amounts
   --  of data in chunks.
   --
   --  @param Ctx The hash context to update.
   --  @param Message The bytes to input into the XOF.

   procedure Extract (Ctx    : in out Context;
                      Digest :    out Byte_Array)
     with Global => null,
     Depends => ((Digest, Ctx) => (Ctx, Digest)),
     Post => State_Of (Ctx) = Extracting;
   --  Extract bytes from the XOF.
   --
   --  Each call to Extract can read an arbitrary number of bytes from the XOF.
   --  Additionally, Extract can be called any number of times.
   --
   --  @param Ctx The XOF context object
   --
   --  @param Digest The bytes from the XOF are output into this array. The
   --    length of the array determines the number of bytes that are extracted.

   function State_Of (Ctx : in Context) return States
     with Global => null;
   --  @return The current state of the XOF context.

   function Rate return Positive
     with Global => null,
     Post => Rate'Result mod 8 = 0;
   --  @return The rate of the XOF (in bits).

private
   use type XOF_Sponge.States;

   type Context is record
      Sponge_Ctx      : XOF_Sponge.Context;
      Update_Complete : Boolean;
   end record;

   function Rate return Positive
   is (XOF_Sponge.Block_Size_Bits - Capacity);

   function Can_Absorb (Ctx : in Context) return Boolean
   is (XOF_Sponge.In_Queue_Bit_Length (Ctx.Sponge_Ctx) mod 8 = 0
       and (XOF_Sponge.In_Queue_Bit_Length (Ctx.Sponge_Ctx) <
              XOF_Sponge.Rate_Of (Ctx.Sponge_Ctx)));

   function State_Of (Ctx : in Context) return States
   is (if XOF_Sponge.State_Of (Ctx.Sponge_Ctx) = XOF_Sponge.Squeezing then Extracting
       elsif Ctx.Update_Complete or (not Can_Absorb (Ctx)) then Ready_To_Extract
       else Updating);

end Keccak.Generic_XOF;
