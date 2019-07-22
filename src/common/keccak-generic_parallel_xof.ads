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
with Keccak.Generic_Parallel_Sponge;
with Keccak.Types;

--  @summary
--  Implements a generic parallel eXtendable Output Function (XOF).
--
--  @description
--  The XOF is built on top of a Generic_Parallel_Sponge instance, and
--  is able to calculate N separate XOFs in parallel where N is the
--  number of parallel instances.
--
--  @group XOF
generic
   with package Sponge is new Keccak.Generic_Parallel_Sponge (<>);

   --  Sponge capacity.
   --
   --  This must be a multiple of 8, and must be smaller than the state size.
   Capacity      : Positive;

   Suffix        : Keccak.Types.Byte;

   Suffix_Size   : Natural;

package Keccak.Generic_Parallel_XOF
is

   Num_Parallel_Instances : constant Positive := Sponge.Num_Parallel_Instances;

   type Context is private;

   type States is (Updating, Extracting, Finished);

   subtype Rate_Bits_Number is Sponge.Rate_Bits_Number;

   procedure Init (Ctx : out Context)
     with Global => null,
     Post => State_Of (Ctx) = Updating;

   procedure Update_Separate (Ctx  : in out Context;
                              Data : in     Types.Byte_Array)
     with Global => null,
     Pre => (Data'Length mod Num_Parallel_Instances = 0
             and Data'Length / Num_Parallel_Instances <= Natural'Last / 8
             and State_Of (Ctx) = Updating),
     Contract_Cases =>
       ((Data'Length / Num_Parallel_Instances) mod (Rate / 8) = 0
        => State_Of (Ctx) = Updating,

        others
        => State_Of (Ctx) = Extracting);

   procedure Update_All (Ctx        : in out Context;
                         Data       : in     Types.Byte_Array)
     with Global => null,
     Pre => State_Of (Ctx) = Updating,
     Contract_Cases =>
       (Data'Length mod (Rate / 8) = 0 => State_Of (Ctx) = Updating,
        others                         => State_Of (Ctx) = Extracting);

   procedure Extract_Separate (Ctx  : in out Context;
                               Data :    out Types.Byte_Array)
     with Global => null,
     Pre => (Data'Length mod Num_Parallel_Instances = 0
             and State_Of (Ctx) in Updating | Extracting),
     Contract_Cases =>
       ((Data'Length / Num_Parallel_Instances) mod (Rate / 8) = 0
        => State_Of (Ctx) = Extracting,

        others
        => State_Of (Ctx) = Finished);

   function State_Of (Ctx : in Context) return States
     with Global => null;

   function Rate return Rate_Bits_Number
     with Global => null;

private
   use type Sponge.States;

   type Context is record
      Sponge_Ctx : Sponge.Context (Capacity);
   end record
     with Predicate => Sponge.Rate_Of (Context.Sponge_Ctx) = Rate;

   function State_Of (Ctx : in Context) return States
   is (case Sponge.State_Of (Ctx.Sponge_Ctx) is
          when Sponge.Absorbing => Updating,
          when Sponge.Squeezing => Extracting,
          when Sponge.Finished  => Finished);

   function Rate return Rate_Bits_Number
   is (Sponge.Block_Size_Bits - Capacity);

end Keccak.Generic_Parallel_XOF;
