-------------------------------------------------------------------------------
-- Copyright (c) 2016, Daniel King
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
with Keccak.Generic_CSHAKE;
with Keccak.Types;

pragma Elaborate_All(Keccak.Generic_CSHAKE);

generic
   with package KMAC_CSHAKE is new Keccak.Generic_CSHAKE(<>);
package Keccak.Generic_KMAC
is

   type Context is private;

   type States is (Updating, Finished);

   procedure Init(Ctx           :    out Context;
                  Key           : in     Types.Byte_Array;
                  Customization : in     String;
                  Output_Length : in     Positive)
     with Depends => (Ctx => (Key, Customization, Output_Length)),
     Post => (Output_Length_Of(Ctx) = Output_Length
              and State_Of(Ctx) = Updating);
   --  Initialize the KMAC context.
   --
   --  @param Ctx The contex to initialize.
   --
   --  @param Key The variable-length key to use for the KMAC context.
   --     Note that it is permitted for the length of the key to be 0.
   --
   --  @param Customization An optional customization string to provide domain
   --     separation for different usages of KMAC.
   --
   --  @param Output_Length The length (in bytes) of the output digest produced
   --     by the KMAC instance. Note that any positive value is permitted for
   --     the length.


   procedure Update(Ctx     : in out Context;
                    Message : in     Types.Byte_Array)
     with Depends => (Ctx => + Message),
     Pre => State_Of(Ctx) = Updating,
     Post => (State_Of(Ctx) = Updating
              and Output_Length_Of(Ctx) = Output_Length_Of(Ctx'Old));
   --  Process data with KMAC.
   --
   --  Note that this function can be called multiple times to process an
   --  arbitrary amount of data.
   --
   --  @param Ctx The KMAC context.
   --
   --  @param Message The byte array containing the bytes to process.


   procedure Finish(Ctx : in out Context;
                    MAC :    out Types.Byte_Array)
     with Depends => ((Ctx, MAC) => (Ctx, MAC)),
     Pre => (State_Of(Ctx) = Updating
             and MAC'Length = Output_Length_Of(Ctx)),
     Post => State_Of(Ctx) = Finished;
   --  Finish the KMAC computation and generate the MAC.
   --
   --  Note that the length of the MAC array must exactly match the output
   --  length that was configured when the context was initialized.
   --
   --  After calling this procedure the context can no longer be used. However,
   --  it can be re-initialized to perform a new KMAC computation.
   --
   --  @param Ctx The KMAC context.
   --
   --  @param MAC The computed MAC is written to this array. The length of
   --     this array must exactly match the output length that was configured
   --     when the KMAC context was initialized.


   function State_Of(Ctx : in Context) return States;
   --  Get the current state of the context.
   --
   --  The context can only be used whilst it is in the "Updating" state.
   --  Otherwise, once the context is finished then it can no longer be used,
   --  and it must be reset in order to be re-used for a new KMAC computation.



   function Rate return Positive
     with Post => Rate'Result mod 8 = 0;
   --  Get the rate parameter (in bits) of the KMAC instance.
   --
   --  The rate is defined as the underlying state size minus the capacity
   --  parameter. E.g. for KMAC128 the state size is 1600 bits (the state
   --  is based on Keccak[1600]), and the Capacity is 256 bits, which results
   --  in a rate of 1600 - 256 = 1344 bits (168 bytes).

   function Output_Length_Of(Ctx : in Context) return Positive;
   --  Get the output length (in bytes) of a KMAC context.
   --
   --  The output length is configured when the context is initialized.

private
   use type KMAC_CSHAKE.States;

   type Context is record
      CSHAKE_Ctx    : KMAC_CSHAKE.Context;
      Output_Length : Positive;
   end record;

   function State_Of(Ctx : in Context) return States
   is (if KMAC_CSHAKE.State_Of(Ctx.CSHAKE_Ctx) = KMAC_CSHAKE.Updating
       then Updating
       else Finished);

   function Rate return Positive
   is (KMAC_CSHAKE.Rate);

   function Output_Length_Of(Ctx : in Context) return Positive
   is (Ctx.Output_Length);

end Keccak.Generic_KMAC;
