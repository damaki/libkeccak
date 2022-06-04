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

--  @summary
--  Simulates a higher-order level of parallelism from lower-order parallelism.
--
--  @description
--  This package uses a combines multiple instances of lower-order parallelism
--  (e.g. two 2x parallelism) into a single higher-order parallel instance.
--  For example, this package can be used to simulate Keccak-p[1600,24]x8
--  by serially invoking 2 separate instances of Keccak-[1600,24]x4.
--
--  This package is useful in cases where a high order of parallelism is
--  required (e.g. 8x) by an API, but such an implementation is not available.
--
--  Instances of this package can be chained. For example, if you want to
--  have a fallback for 8x parallelism, but you only have a 2x implementation,
--  then you can double the 2x into 4x, then double again the 4x into 8x.
--
--  @group Parallel Keccak-f
generic
   type Permutation_State is private;
   --  Type for the parallel permutation state (e.g. Keccak-f[1600]�2).

   Base_Parallelism : Positive;
   --  The number of parallel instances for the @Permutation_Type@.
   --
   --  For example, if Permutation_State is the state for Keccak-f[1600]�4
   --  then set Base_Parallelism to 4.

   Parallel_Factor : Positive;
   --  Multiply the Base_Parallelism by this number.
   --
   --  The overall number of parallel instances will be:
   --     Base_Parallelism * Parallel_Factor.
   --
   --  For example, if this package is instantiated with Keccak-f[1600]�4
   --  and Parallel_Factor = 2, then this package will use 2x Keccak-f[1600]�4
   --  to produce an overall Keccak-f[1600]�8 parallel permutation.

   with procedure Init (S : out Permutation_State);
   --  Initializes the Permutation_State to all zeroes.

   with procedure XOR_Bits_Into_State_Separate
     (S           : in out Permutation_State;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural);
   --  XOR bits into each parallel state.

   with procedure XOR_Bits_Into_State_All
     (S           : in out Permutation_State;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural);

   with procedure Extract_Bytes (S           : in     Permutation_State;
                                 Data        :    out Types.Byte_Array;
                                 Data_Offset : in     Natural;
                                 Byte_Len    : in     Natural);
   --  Extract bytes from each parallel state.

   State_Size_Bits    : Positive;

package Keccak.Generic_Parallel_Permutation_Parallel_Fallback
is

   Num_Parallel_Instances : constant Positive := Base_Parallelism * Parallel_Factor;

   type Permutation_State_Array is
     array (0 .. Parallel_Factor - 1)
     of Permutation_State;

   --  Parallel_State is not declared as a private type as a workaround for a
   --  bug in GNATprove during flow analysis of instantiations of the
   --  generic Permute_All procedure.

   type Parallel_State is record
      States : Permutation_State_Array;
   end record;

   type State_Index is new Natural range 0 .. Num_Parallel_Instances - 1;

   procedure Init (S : out Parallel_State)
     with Global => null;

   generic
      with procedure Permute (S : in out Permutation_State);
   procedure Permute_All (S : in out Parallel_State)
     with Global => null;
   --  Apply the permutation function to each internal instance.

   procedure XOR_Bits_Into_State_Separate
     (S           : in out Parallel_State;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
     with Global => null,
     Pre => (Data'Length / Num_Parallel_Instances <= Natural'Last / 8
             and then Data'Length mod Num_Parallel_Instances = 0
             and then Data_Offset <= (Data'Length / Num_Parallel_Instances)
             and then Bit_Len <= ((Data'Length / Num_Parallel_Instances) - Data_Offset) * 8
             and then Bit_Len <= State_Size_Bits);

   procedure XOR_Bits_Into_State_All
     (S           : in out Parallel_State;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural)
     with Global => null,
     Depends => (S =>+ (Data, Bit_Len)),
     Pre => (Data'Length <= Natural'Last / 8
             and then Bit_Len <= Data'Length * 8
             and then Bit_Len <= State_Size_Bits);

   procedure Extract_Bytes (S           : in     Parallel_State;
                            Data        :    out Types.Byte_Array;
                            Data_Offset : in     Natural;
                            Byte_Len    : in     Natural)
     with Global => null,
     Pre => (Data'Length mod Num_Parallel_Instances = 0
             and then Data_Offset <= Data'Length / Num_Parallel_Instances
             and then Byte_Len <= (Data'Length / Num_Parallel_Instances) - Data_Offset
             and then Byte_Len <= State_Size_Bits / 8);
   --  Extract bytes from each parallel instance.
   --
   --  The @Data@ array is split into N equal sized chunks, where N is the
   --  number of parallel instances. Byte_Len (BL) bytes are extracted from each
   --  parallel instance and copied into each chunk, offset by @Data_Offset@ (DO)
   --  relative to the start of each chunk.
   --  Here's an illustration for 2x parallel instances:
   --
   --       DO    BL             DO    BL
   --     |--->|<----->|       |--->|<----->|
   --     +-----------------------------------------+
   --     |                    |                    | Data
   --     +-----------------------------------------+
   --     .    |  ^    |            |  ^    |
   --     .    | Write |            | Write |
   --     .    |       |            |       |
   --     .    +-----------+        +-----------+
   --     .    |  state 0  |        |  state 1  |
   --     .    +-----------+        +-----------+
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "Data is partially initialized in chunks of size 'Byte_Len'" &
        " at offset 'Data_Offset' in each chunk");

end Keccak.Generic_Parallel_Permutation_Parallel_Fallback;
