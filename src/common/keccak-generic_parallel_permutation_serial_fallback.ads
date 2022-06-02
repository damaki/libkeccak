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
--  Serial fallback implementation of the parallel permutation API.
--
--  @description
--  This package implements subprograms for a parallel permutation API,
--  but executes the permutation serially.
--
--  For example, this package can be used to simulate Keccak-p[1600,24]x8
--  by serially invoking 8 separate instances of Keccak-p[1600,24] serially.
--
--  @group Parallel Keccak-f
generic
   type Permutation_State is private;

   with procedure Init (A : out Permutation_State);

   with procedure XOR_Bits_Into_State (S       : in out Permutation_State;
                                       Data    : in     Types.Byte_Array;
                                       Bit_Len : in     Natural);

   with procedure Extract_Bytes (A    : in     Permutation_State;
                                 Data :    out Types.Byte_Array);

   State_Size_Bits    : Positive;
   --  State size of the permutation in bits.
   --
   --  E.g. for Keccak-f[1600] set State_Size_Bits to 1600.

   Parallelism : Positive;
   --  Specifies the number of simulated parallel instances.

package Keccak.Generic_Parallel_Permutation_Serial_Fallback
is
   Num_Parallel_Instances : constant Positive := Parallelism;

   type Permutation_State_Array is array (0 .. Parallelism - 1) of Permutation_State;

   --  Parallel_State is not declared as a private type as a workaround for a
   --  bug in GNATprove during flow analysis of instantiations of the
   --  generic Permute_All procedure.

   type Parallel_State is record
      States : Permutation_State_Array;
   end record;

   procedure Init (S : out Parallel_State)
     with Global => null;

   generic
      with procedure Permute (A : in out Permutation_State);
   procedure Permute_All (S : in out Parallel_State)
     with Global => null;

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

end Keccak.Generic_Parallel_Permutation_Serial_Fallback;
