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
with Keccak.Generic_CSHAKE;
with Keccak.Types;          use Keccak.Types;

--  @summary
--  Generic implementation of the TupleHash algorithm.
--
--  @description
--  TupleHash is a SHA-3-derived hash function with variable-length output
--  that is designed to simply hash a tuple of input strings, any or all of
--  which may be empty strings, in an unambiguous way.
--
--  This API is used as follows:
--
--  1 Call Init to initialise a new TupleHash context.
--
--  2 Call Update_Tuple_Item for each item in the tuple.
--
--  3 Call either Finish or Extract to produce the desired type of output
--    (TupleHash or TupleHashXOF):
--
--  * Finish is used to produce a single output of arbitrary length (TupleHash).
--    The requested output length affects the output. For example, requesting
--    a 10-byte output will produce an unrelated hash to requesting a 20-byte
--    output.
--
--  * Extract can be called one or more times to produce an arbitrary number
--    of output bytes (TupleHashXOF). In this case, the total output length is
--    unknown in advance so the output does not change based on the overall length.
--    For example, a 10-byte output is the truncated version of a 20-byte output.
--
--  @group TupleHash
generic
   with package CSHAKE is new Generic_CSHAKE (<>);
package Keccak.Generic_Tuple_Hash
is

   type Context is private;

   type States is (Updating, Extracting, Finished);
   --  @value Updating When in this state additional data can be input into the
   --    TupleHash context.
   --
   --  @value Extracting When in this state, the TupleHash context can generate
   --    output bytes by calling the Extract procedure.
   --
   --  @value Finished When in this state the context is finished and no more data
   --    can be input or output.

   procedure Init (Ctx           :    out Context;
                   Customization : in     String := "")
     with Global => null,
     Depends => (Ctx => Customization),
     Post => State_Of (Ctx) = Updating;
   --  Initialise the TupleHash context.
   --
   --  @param Ctx The TupleHash context to initialise.
   --
   --  @param Customization An optional customisation string to provide domain
   --    separation between different instances of TupleHash.

   procedure Update_Tuple_Item (Ctx  : in out Context;
                                Item : in     Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Item),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Updating;
   --  Process the next tuple item.
   --
   --  The entire tuple item must be passed into this procedure.
   --
   --  This may be called multiple times to process an arbitrary number of items.

   procedure Finish (Ctx     : in out Context;
                     Digest  :    out Byte_Array)
     with Global => null,
     Depends => ((Ctx, Digest) => (Ctx, Digest)),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Finished;
   --  Produce a TupleHash digest (TupleHash variant)
   --
   --  After calling this procedure the context can no longer be used. However,
   --  it can be re-initialized to perform a new TupleHash computation.
   --
   --  The number of output bytes requested is determined from the length of
   --  the Digest array (i.e. Digest'Length) and has an effect on the value of the
   --  output digest. For example, two different ParallelHash computations with identical
   --  inputs (same key and input data) but with different digest lengths will
   --  produce independent digest values.
   --
   --  Note that this procedure can only be called once for each ParallelHash
   --  computation. This requires that the required digest length is known before
   --  calling this procedure, and a Byte_Array with the correct length is
   --  given to this procedure. For applications where the number of required
   --  output bytes is not known until after bytes are output, see the Extract
   --  procedure.

   procedure Extract (Ctx    : in out Context;
                      Digest :    out Byte_Array)
     with Global => null,
     Depends => ((Ctx, Digest) => (Ctx, Digest)),
     Pre => State_Of (Ctx) in Updating | Extracting,
     Post => State_Of (Ctx) = Extracting;
   --  Produce a TupleHash digest (TupleHashXOF variant)
   --
   --  After calling this procudure no more data can be input into the ParllelHash
   --  computation.
   --
   --  This function can be called multiple times to produce an arbitrary
   --  number of output bytes.

   function State_Of (Ctx : in Context) return States
     with Global => null;

private
   use type CSHAKE.States;

   type Context is record
      Ctx      : CSHAKE.Context;
      Finished : Boolean;
   end record;

   function State_Of (Ctx : in Context) return States
   is (if Ctx.Finished then Finished
       elsif CSHAKE.State_Of (Ctx.Ctx) = CSHAKE.Updating then Updating
       else Extracting);

end Keccak.Generic_Tuple_Hash;
