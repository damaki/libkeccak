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
with Keccak.Types;

pragma Elaborate_All (Keccak.Generic_CSHAKE);

--  @summary
--  Generic implementation of KMAC as described in NIST SP 800-185.
--
--  @description
--  This package implements the KMAC algorithm on top of any instantiation
--  of the Generic_CSHAKE package. For example, KMAC128 is implemented on
--  top of cSHAKE128.
--
--  The KECCAK Message Authentication Code (KMAC) algorithm is a PRF and
--  keyed hash function based on KECCAK. It provides variable-length output,
--  and unlike SHAKE and cSHAKE, altering the requested output length generates
--  a new, unrelated output.
--
--  This API is used as follows:
--
--  1 Call Init to initialise a new context. The private key and an optional
--    customisation string (for domain separation) are provided here.
--
--  2 Call Update one or more times to input an arbitrary amount of data into
--    the KMAC context.
--
--  3 Call either Finish or Extract to produce the desired type of output
--    (KMAC or KMACXOF):
--
--  * Finish is used to produce a single output of arbitrary length (KMAC).
--    The requested output length affects the output. For example, requesting
--    a 10-byte output will produce an unrelated hash to requesting a 20-byte
--    output.
--
--  * Extract can be called one or more times to produce an arbitrary number
--    of output bytes (KMACXOF). In this case, the total output length is
--    unknown in advance so the output does not change based on the overall length.
--    For example, a 10-byte output is the truncated version of a 20-byte output.
--
--  @group KMAC
generic
   with package KMAC_CSHAKE is new Keccak.Generic_CSHAKE (<>);
package Keccak.Generic_KMAC
is

   type Context is private;

   type States is (Updating, Extracting, Finished);
   --  @value Updating When in this state additional data can be input into the
   --    KMAC context.
   --
   --  @value Extracting When in this state, the KMAC context can generate
   --    output bytes by calling the Extract procedure.
   --
   --  @value Finished When in this state the context is finished and no more data
   --    can be input or output.

   procedure Init (Ctx           :    out Context;
                   Key           : in     Types.Byte_Array;
                   Customization : in     String)
     with Global => null,
     Depends => (Ctx => (Key, Customization)),
     Post => State_Of (Ctx) = Updating;
   --  Initialize the KMAC context.
   --
   --  In cases where many KMAC computations are performed with the same
   --  key and customization string it is possible to
   --  initialize a context once with the desired parameters, then copy the
   --  context as many times as necessary for the different computations.
   --
   --  @param Ctx The contex to initialize.
   --
   --  @param Key The variable-length key to use for the KMAC context.
   --     Note that it is permitted for the length of the key to be 0.
   --
   --  @param Customization An optional customization string to provide domain
   --     separation for different usages of KMAC.

   procedure Update (Ctx     : in out Context;
                     Message : in     Types.Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Message),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Updating;
   --  Process data with KMAC.
   --
   --  Note that this function can be called multiple times to process an
   --  arbitrary amount of data.
   --
   --  @param Ctx The KMAC context.
   --
   --  @param Message The byte array containing the bytes to process.

   procedure Finish (Ctx : in out Context;
                     MAC :    out Types.Byte_Array)
     with Global => null,
     Depends => ((Ctx, MAC) => (Ctx, MAC)),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Finished;
   --  Finish the KMAC computation and generate the MAC.
   --
   --  After calling this procedure the context can no longer be used. However,
   --  it can be re-initialized to perform a new KMAC computation.
   --
   --  The number of output bytes requested is determined from the length of
   --  the MAC array (i.e. MAC'Length) and has an effect on the value of the
   --  output MAC. For example, two different KMAC computations with identical
   --  inputs (same key and input data) but with different MAC lengths will
   --  produce independent MAC values.
   --
   --  Note that this procedure can only be called once for each KMAC
   --  computation. This requires that the required MAC length is known before
   --  calling this procedure, and a Byte_Array with the correct length is
   --  given to this procedure. For applications where the number of required
   --  output bytes is not known until after bytes are output, see the Extract
   --  procedure.
   --
   --  @param Ctx The KMAC context.
   --
   --  @param MAC The computed MAC is written to this array. The length of
   --     this array can be arbitrary.

   procedure Extract (Ctx : in out Context;
                      MAC :    out Types.Byte_Array)
     with Global => null,
     Depends => ((Ctx, MAC) => (Ctx, MAC)),
     Pre => State_Of (Ctx) in Updating | Extracting,
     Post => State_Of (Ctx) = Extracting;
   --  Finish the KMAC computation generate XOF output bytes.
   --
   --  After calling this procudure no more data can be input into the KMAC
   --  computation.
   --
   --  This function can be called multiple times to produce an arbitrary
   --  number of output bytes.

   function State_Of (Ctx : in Context) return States
     with Global => null;
   --  Get the current state of the context.
   --
   --  The context can only be used whilst it is in the "Updating" state.
   --  Otherwise, once the context is finished then it can no longer be used,
   --  and it must be reset in order to be re-used for a new KMAC computation.

   function Rate return Positive
     with Global => null,
     Post => Rate'Result mod 8 = 0;
   --  Get the rate parameter (in bits) of the KMAC instance.
   --
   --  The rate is defined as the underlying state size minus the capacity
   --  parameter. E.g. for KMAC128 the state size is 1600 bits (the state
   --  is based on Keccak[1600]), and the Capacity is 256 bits, which results
   --  in a rate of 1600 - 256 = 1344 bits (168 bytes).

private
   use type KMAC_CSHAKE.States;

   type Context is record
      CSHAKE_Ctx : KMAC_CSHAKE.Context;
      Finished   : Boolean;
   end record;

   function State_Of (Ctx : in Context) return States
   is (if Ctx.Finished then Finished
       elsif KMAC_CSHAKE.State_Of (Ctx.CSHAKE_Ctx) = KMAC_CSHAKE.Updating then
          Updating
       else Extracting);

   function Rate return Positive
   is (KMAC_CSHAKE.Rate);

end Keccak.Generic_KMAC;
