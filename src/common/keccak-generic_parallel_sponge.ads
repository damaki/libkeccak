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
with Keccak.Types;

generic
   State_Size : Positive;
   --  Size in bits of the underlying permutation state.
   --  E.g. for Keccak-f[1600] this should be set to 1600.

   type State_Type is private;
   --  Type of the parallel permutation state.

   Parallelism : Positive;
   --  Number of parallel instances provided by State_Type.

   with procedure Init (S : out State_Type);
   --  Initializes the parallel permutation states.

   with procedure Permute_All (S : in out State_Type);
   --  Apply the permutation function to each state in parallel.

   with procedure XOR_Bits_Into_State_Separate
     (S           : in out State_Type;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural);
   --  XOR bits into a specific instance of the permutation state.

   with procedure XOR_Bits_Into_State_All
     (S           : in out State_Type;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural);

   with procedure Extract_Bytes (S           : in     State_Type;
                                 Data        : in out Keccak.Types.Byte_Array;
                                 Data_Offset : in     Natural;
                                 Byte_Len    : in     Natural);
   --  Extracts a bytes of output from the state

   with procedure Pad (Block          : in out Keccak.Types.Byte_Array;
                       Num_Used_Bits  : in     Natural;
                       Max_Bit_Length : in     Natural);
   --  Apply the padding rule to a block of data.

   Min_Padding_Bits : Natural;
   --  Minimum number of padding bits appended to the message.
   --
   --  E.g. for pad10*1 there are a minimum of 2 padding bits (two '1' bits).

package Keccak.Generic_Parallel_Sponge
is
   Num_Parallel_Instances : constant Positive := Parallelism;

   Block_Size_Bits        : constant Positive := State_Size;

   subtype Rate_Bits_Number is Positive range 1 .. State_Size - 1
     with Dynamic_Predicate => Rate_Bits_Number mod 8 = 0;
   --  Number representing the Rate (in bits).
   --
   --  The Rate must be a positive integer, and less than the size of the
   --  state (i.e. there must be at least 1 bit of "capacity"). Furthermore,
   --  this implementation restricts the Rate to a multiple of 8 bits.

   type Context (Capacity : Positive) is private;

   type States is (Absorbing, Squeezing, Finished);

   procedure Init (Ctx : out Context)
     with Global => null,
     Pre => (Ctx.Capacity < State_Size
             and then (State_Size - Ctx.Capacity) mod 8 = 0),
     Post => State_Of (Ctx) = Absorbing;

   procedure Absorb_Bytes_Separate (Ctx        : in out Context;
                                    Data       : in     Types.Byte_Array)
     with Global => null,
     Pre => (Data'Length mod Num_Parallel_Instances = 0
             and Data'Length / Num_Parallel_Instances <= Natural'Last / 8
             and State_Of (Ctx) = Absorbing),
     Contract_Cases =>
       ((Data'Length / Num_Parallel_Instances) mod (Rate_Of (Ctx) / 8) = 0
        => State_Of (Ctx) = Absorbing,

        others
        => State_Of (Ctx) = Squeezing);
   --  Absorb bytes into all parallel instances.
   --
   --  The @Data@ buffer is split into N equal sized chunks, where N is the
   --  number of parallel instances. For example, with Keccak-f[1600]�4, the
   --  @Data@ will be split into four chunks of equal length:
   --
   --  +-----------+-----------+-----------+-----------+
   --  |     0     |     1     |     2     |     3     |
   --  +-----------+-----------+-----------+-----------+
   --
   --  Chunk 0 will be absorbed into the first parallel instance; chunk 1 will
   --  be absorbed into the second parallel instance, and so on...
   --
   --  This procedure can be called multiple times to absorb an arbitrary
   --  amount of data, provided that the length of each chunk is a multiple
   --  of the rate. If the chunk length is not a multiple of the rate then
   --  the data will be absorbed, but the @Context@ will advance to the
   --  Squeezing state and no more data can be absorbed.

   procedure Absorb_Bytes_All (Ctx        : in out Context;
                               Data       : in     Types.Byte_Array)
     with Global => null,
     Pre => State_Of (Ctx) = Absorbing,
     Contract_Cases =>
       (Data'Length mod (Rate_Of (Ctx) / 8) = 0 => State_Of (Ctx) = Absorbing,
        others                                  => State_Of (Ctx) = Squeezing);

   procedure Absorb_Bytes_Separate_With_Suffix
     (Ctx        : in out Context;
      Data       : in     Types.Byte_Array;
      Suffix     : in     Types.Byte;
      Suffix_Len : in     Natural)
     with Global => null,
     Pre => (Data'Length mod Num_Parallel_Instances = 0
             and Data'Length / Num_Parallel_Instances <= Natural'Last / 8
             and Suffix_Len in 0 .. 8 - Min_Padding_Bits
             and State_Of (Ctx) = Absorbing),
     Post => State_Of (Ctx) = Squeezing;

   procedure Absorb_Bytes_All_With_Suffix
     (Ctx        : in out Context;
      Data       : in     Types.Byte_Array;
      Suffix     : in     Types.Byte;
      Suffix_Len : in     Natural)
     with Global => null,
     Pre => (State_Of (Ctx) = Absorbing
             and Suffix_Len in 0 .. 8 - Min_Padding_Bits),
     Post => State_Of (Ctx) = Squeezing;

   procedure Squeeze_Bytes_Separate (Ctx        : in out Context;
                                     Data       :    out Types.Byte_Array)
     with Global => null,
     Pre => (Data'Length mod Num_Parallel_Instances = 0
             and State_Of (Ctx) in Absorbing | Squeezing),
     Post => Rate_Of (Ctx) = Rate_Of (Ctx'Old),
     Contract_Cases =>
       ((Data'Length / Num_Parallel_Instances) mod (Rate_Of (Ctx) / 8) = 0
        => State_Of (Ctx) = Squeezing,

        others
        => State_Of (Ctx) = Finished);

   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "GNATprove issues a false positive due to the use of loops to initialize Data");

   function State_Of (Ctx : in Context) return States;

   function Rate_Of (Ctx : in Context) return Rate_Bits_Number;

private

   --  The rate number here represents bytes, not bits.
   --  This makes it easier to handle in proof, since bytes are
   --  always a multiple of 8 bits.
   subtype Rate_Bytes_Number is Positive range 1 .. ((State_Size + 7) / 8) - 1;

   type Context (Capacity : Positive) is record
      Permutation_State : State_Type;
      Rate              : Rate_Bytes_Number;
      State             : States;
   end record
     with Predicate =>
       (Context.Rate = (State_Size - Context.Capacity) / 8
        and then (State_Size - Context.Capacity) mod 8 = 0
        and then Context.Rate * 8 = State_Size - Context.Capacity);

   function State_Of (Ctx : in Context) return States
   is (Ctx.State);

   function Rate_Of (Ctx : in Context) return Rate_Bits_Number
   is (Ctx.Rate * 8);

end Keccak.Generic_Parallel_Sponge;
