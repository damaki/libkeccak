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
with Keccak.Types; use Keccak.Types;
with Keccak.Generic_XOF;

pragma Elaborate_All(Keccak.Generic_XOF);

generic
   with package XOF is new Keccak.Generic_XOF(<>);
   --  The extendable output function on which the CSHAKE instance is
   --  constructed.
package Keccak.Generic_CSHAKE
is

   type Context is private;

   type States is (Updating, Ready_To_Extract, Extracting);

   procedure Init(Ctx           :    out Context;
                  Customization : in     String := "";
                  Function_Name : in     String := "")
     with Global => null,
     Depends => (Ctx => (Customization, Function_Name)),
     Pre => Customization /= "" or Function_Name /= "",
     Post => State_Of(Ctx) = Updating;
   --  Initialize the CSHAKE context.
   --
   --  Note that the Customization and Function_Name strings are optional, but
   --  at least one must be a non-empty string. Otherwise, if a usage requires
   --  both strings to be empty then SHAKE must be used instead, or
   --  Keccak.XOF if a customized version of SHAKE is required.
   --
   --  In cases where many CSHAKE computations are performed with the same
   --  customization and function name strings it is possible to initialize
   --  a context once with the desired parameters, then copy the context as
   --  many times as necessary for the different computations.
   --
   --  @param Ctx The context to initialize.
   --
   --  @param Customization An optional customization string to provide domain
   --     separation from different usages of CSHAKE.
   --
   --  @param Function_Name An optional name for the function for which CSHAKE
   --     is being used. This is intended to be used only for NIST-defined
   --     constructions based on CSHAKE. For other non-NIST usages this string
   --     should be the empty string.



   procedure Update(Ctx     : in out Context;
                    Message : in     Byte_Array)
     with Global => null,
     Inline,
     Depends => (Ctx => + Message),
     Pre => State_Of(Ctx) = Updating,
     Post => State_Of(Ctx) = Updating;
   --  Process data with CSHAKE.
   --
   --  This function can be called multiple times to process a large amount of
   --  data.
   --
   --  @param Ctx The CSHAKE context.
   --
   --  @param Message The data to process with CSHAKE.


   procedure Update(Ctx        : in out Context;
                    Message    : in     Byte_Array;
                    Bit_Length : in     Natural)
     with Global => null,
     Depends => (Ctx => + (Message, Bit_Length)),
     Pre => (State_Of(Ctx) = Updating
             and then (Message'Length < Natural'Last / 8)
             and then Bit_Length <= Message'Length * 8),
     Contract_Cases => (Bit_Length mod 8 = 0 => State_Of(Ctx) = Updating,
                        others               => State_Of(Ctx) = Ready_To_Extract);


   procedure Extract(Ctx    : in out Context;
                     Digest :    out Byte_Array)
     with Global => null,
     Depends => ((Ctx, Digest) => (Ctx, Digest)),
     Post => State_Of(Ctx) = Extracting;
   --  Produce output bytes.
   --
   --  Note that after Extract has been called it is no longer possible to
   --  process data using Update.
   --
   --  This function can be called multiple times to produce an arbitrary
   --  amount of output bytes.
   --
   --  @param Ctx The CSHAKE context.
   --
   --  @param Digest The output bytes are written to this byte array. The
   --     length of this array determines the number of bytes to produce.



   function State_Of(Ctx : in Context) return States
     with Global => null;
   --  Get the current state of a context.



   function Rate return Positive
     with Global => null,
       Post => Rate'Result mod 8 = 0;
   --  Get the rate parameter (in bits) of the CSHAKE instance.
   --
   --  The rate is defined as the underlying state size minus the capacity
   --  parameter. E.g. for CSHAKE128 the state size is 1600 bits (the state
   --  is based on Keccak[1600]), and the Capacity is 256 bits, which results
   --  in a rate of 1600 - 256 = 1344 bits (168 bytes).


private
   use type XOF.States;

   type Context is record
      XOF_Ctx       : XOF.Context;
   end record;

   function State_Of(Ctx : in Context) return States
   is (case XOF.State_Of (Ctx.XOF_Ctx) is
          when XOF.Updating         => Updating,
          when XOF.Ready_To_Extract => Ready_To_Extract,
          when XOF.Extracting       => Extracting);

   function Rate return Positive
   is (XOF.Rate);

end Keccak.Generic_CSHAKE;
