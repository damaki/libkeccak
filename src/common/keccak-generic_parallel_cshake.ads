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
with Keccak.Generic_Parallel_XOF;
with Keccak.Types;

pragma Elaborate_All (Keccak.Generic_Parallel_XOF);

--  @summary
--  Parallel implementation of the cSHAKE algorithm.
--
--  @description
--  This package supports N separate instances of cSHAKE which are processed
--  in parallel, where N = Num_Parallel_Instances. It is built on top of any
--  instantiation of the Generic_Parallel_CSHAKE package.
--
--  This is the basis of parallel algorithms, e.g. ParallelHash.
--
--  This API is used as follows:
--
--  1 Initialise a new context by calling Init. Customization strings can be
--    optionally given to Init to provide domain separation between different
--    uses of cSHAKE.
--
--  2 Call Update_Separate one or more times to input an arbitrary amount of
--    data into each cSHAKE instance.
--
--  3 Call Extract_Separate one or more times to produce an arbitrary number
--    of output bytes from each cSHAKE instance.
--
--  @group cSHAKE
generic
   with package XOF is new Keccak.Generic_Parallel_XOF (<>);
package Keccak.Generic_Parallel_CSHAKE
is

   Num_Parallel_Instances : constant Positive := XOF.Num_Parallel_Instances;

   type Context is private;

   type States is (Updating, Extracting, Finished);

   subtype Rate_Bits_Number is XOF.Rate_Bits_Number;

   procedure Init (Ctx           :    out Context;
                   Customization : in     String;
                   Function_Name : in     String)
     with Global => null,
     Pre => Customization /= "" or Function_Name /= "",
     Post => State_Of (Ctx) = Updating;
   --  Initialize the parallel cSHAKE context.
   --
   --  All parallel instances are initialised with the same Customization
   --  and Function_Name strings.
   --
   --  Note that the Customization and Function_Name strings are optional, but
   --  at least one must be a non-empty string.
   --
   --  In cases where many cSHAKE computations are performed with the same
   --  customization and function name strings it is possible to initialize
   --  a context once with the desired parameters, then copy the context as
   --  many times as necessary for the different computations. The following
   --  example creates two contexts initialised to the same value:
   --
   --     declare
   --        Ctx1 : Context;
   --        Ctx2 : Context;
   --     begin
   --        Init (Ctx1, "Example", "");
   --
   --        Ctx2 := Ctx1;
   --     end;
   --
   --  @param Ctx The context to initialize.
   --
   --  @param Customization An optional customization string to provide domain
   --     separation from different usages of cSHAKE.
   --
   --  @param Function_Name An optional name for the function for which cSHAKE
   --     is being used. This is intended to be used only for NIST-defined
   --     constructions based on cSHAKE. For other non-NIST usages this string
   --     should be the empty string.

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
   --  Input data into each cSHAKE instance.
   --
   --  The Data buffer is split into N equal-sized chunks, where N is the
   --  number of parallel instances. For example, with 4-level parallelism, the
   --  Data will be split into four chunks of equal length:
   --
   --     +-----------+-----------+-----------+-----------+
   --     |     0     |     1     |     2     |     3     | Data (split into N chunks)
   --     +-----------+-----------+-----------+-----------+
   --           |           |           |           | Absorb
   --           V           V           V           V
   --     +-----------+-----------+-----------+-----------+
   --     |     0     |     1     |     2     |     3     | Context (N cSHAKE instances)
   --     +-----------+-----------+-----------+-----------+
   --
   --  Chunk 0 will be absorbed into the first cSHAKE instance; chunk 1 will
   --  be absorbed into the second cSHAKE instance, and so on...
   --
   --  This procedure can be called multiple times to absorb an arbitrary
   --  amount of data, provided that the length of each chunk is a multiple
   --  of the rate. If the chunk length is not a multiple of the rate then
   --  the data will be absorbed, but the Context will advance to the
   --  Squeezing state and no more data can be absorbed.

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
   --  Get output bytes from each cSHAKE instance.
   --
   --     +-----------+-----------+-----------+-----------+
   --     |     0     |     1     |     2     |     3     | Context (N cSHAKE instances)
   --     +-----------+-----------+-----------+-----------+
   --           |           |           |           | Extract
   --           V           V           V           V
   --     +-----------+-----------+-----------+-----------+
   --     |     0     |     1     |     2     |     3     | Data (split into N chunks)
   --     +-----------+-----------+-----------+-----------+
   --
   --  This function may be called multiple times to generate a large amount of
   --  output data, as long as the length of the Data buffer is a multiple of
   --  N * Rate, where N is Num_Parallel_Instances and Rate is the Rate parameter
   --  in bytes. If the Data buffer length does not meet this criterea, then
   --  the context enters the Finished state and no more output bytes can be
   --  produced.

   function State_Of (Ctx : in Context) return States
     with Global => null;
   --  Get the current state of the context.

   function Rate return Rate_Bits_Number
     with Global => null;
   --  Get the rate of the context, in bits.

private

   type Context is record
      XOF_Ctx : XOF.Context;
   end record;

   function State_Of (Ctx : in Context) return States
   is (case XOF.State_Of (Ctx.XOF_Ctx) is
          when XOF.Updating   => Updating,
          when XOF.Extracting => Extracting,
          when XOF.Finished   => Finished);

   function Rate return Rate_Bits_Number
   is (XOF.Rate);

end Keccak.Generic_Parallel_CSHAKE;
