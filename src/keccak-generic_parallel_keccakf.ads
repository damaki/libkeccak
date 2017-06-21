-------------------------------------------------------------------------------
-- Copyright (c) 2017, Daniel King
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
with Keccak.Types;

generic
   L : in Positive;
   -- The binary logarithm of the lane size.
   --
   -- This determines the Keccak-f state size. Possible values are:
   -- * L=0 => 1-bit lanes,  Keccak-f[25]
   -- * L=1 => 2-bit lanes,  Keccak-f[50]
   -- * L=2 => 4-bit lanes,  Keccak-f[100]
   -- * L=3 => 8-bit lanes,  Keccak-f[200]
   -- * L=4 => 16-bit lanes, Keccak-f[400]
   -- * L=5 => 32-bit lanes, Keccak-f[800]
   -- * L=6 => 64-bit lanes, Keccak-f[1600]

   type Lane_Type is mod <>;

   type VXXI_Index is range <>;

   type VXXI is private;
   --  Vector machine type.

   type VXXI_View is array (VXXI_Index) of Lane_Type;
   --  A view of the vector type, permitting individual access to each vector
   --  component.

   Vector_Width : Positive;
   --  Number of vector to components to actually use.
   --
   --  Usually, this would be set to the actual number of vector components
   --  (e.g. 4 for a 4x 32-bit vector). However, you may use a smaller number
   --  if you don't want to use the full vector width. For example, you could
   --  set Vector_Width to 2 with a 4x 32-bit vector type, to obtain 2x
   --  parallelism.

   with function Load (X : in VXXI_View) return VXXI;

   with function Store (X : in VXXI) return VXXI_View;

   with function "xor" (A, B : in VXXI) return VXXI;
   --  Calculates A xor B per vector component.

   with function Rotate_Left (A      : in VXXI;
                              Amount : in Natural) return VXXI;
   --  Calculates Rotate_Left(A) per vector component.

   with function And_Not (A, B : in VXXI) return VXXI;
   --  Calculates B and (not A) per vector component.

   with function Shift_Left (A      : in Lane_Type;
                             Amount : in Natural) return Lane_Type;

   with function Shift_Right (A      : in Lane_Type;
                              Amount : in Natural) return Lane_Type;

   --  @brief@
   --  Implements an optimized Keccak algorithm based on SIMD instruction sets
   --  such as SSE, NEON, and AVX.
   --
   --  @description@
   --  This package provides a basis for parallel implementations of Keccak
   --  for processing N separate permutations in parallel, where N is the number
   --  of vector components in the SIMD instruction set used.
   --
   --  When instantiating this package, subprograms and types are provided as
   --  formal generic parameters which implement each of the required operations
   --  for the target instruction set.
package Keccak.Generic_Parallel_KeccakF
is
   W : constant Positive := 2**L;
   --  Lane size (in bits)

   B : constant Positive := W*25;
   --  Keccak-f state size (in bits).

   Num_Parallel_Instances : constant Positive := Vector_Width;


   type Parallel_State is private;


   type Round_Index is range 0 .. 23;


   subtype Round_Count is Positive range 1 .. 24;


   procedure Init (S : out Parallel_State)
     with Global => null;


   generic
      First_Round : Round_Index := 0;
      Num_Rounds  : Round_Count := 24;
   procedure Permute_All (S : in out Parallel_State)
     with Global => null;


   procedure XOR_Bits_Into_State (S           : in out Parallel_State;
                                  Data        : in     Types.Byte_Array;
                                  Data_Offset : in     Natural;
                                  Bit_Len     : in     Natural)
     with Global => null,
     Pre => (Data'Length / Vector_Width <= Natural'Last / 8
             and then Data'Length mod Vector_Width = 0
             and then Data_Offset <= (Data'Length / Vector_Width)
             and then Bit_Len <= ((Data'Length / Vector_Width) - Data_Offset) * 8
             and then Bit_Len <= B);
   --  XOR bits into each parallel Keccak instance.
   --
   --  The @Data@ array contains the data to be XORed into all parallel
   --  instances. The bytes in @Data@ are split into equal chunks depending on
   --  the number of parallel instances. For example, for Keccak-f[1600]×2 the
   --  @Data@ array is split into 2 chunks as shown below:
   --
   --    DO    BL             DO    BL
   --  |--->|<---->|        |--->|<---->|
   --  +-----------------------------------------+
   --  |                    |                    | Data
   --  +-----------------------------------------+
   --       |      |             |      |
   --       | XOR  |             | XOR  |
   --       |  v   |             |  v   |
   --       +-----------+        +-----------+
   --       |  state 0  |        |  state 1  |
   --       +-----------+        +-----------+
   --
   --  The @Data_Offset@ determines the offset within each chunk to start
   --  reading data. @Bit_Len@ determines the number of bits to read from each
   --  chunk.
   --
   --  The data is always XORed starting at the beginning of the Keccak state.
   --
   --  Where DO = Data_Offset and BL = Bit_Len
   --
   --  @param S The parallel Keccak state to where the bits are XORed.
   --
   --  @param Data The array containing the data to XOR into the parallel state.
   --      The size of this array must be a multiple of the number of parallel
   --      instances. For Example, for Keccak-f[1600]×4 then Data'Length
   --      must be a multiple of 4.
   --
   --  @param Data_Offset Offset of the first byte(s) to read from the @Data@
   --      array.
   --
   --  @param Bit_Len The number of bits to XOR into each state.


   procedure Extract_Bytes (S           : in     Parallel_State;
                            Data        : in out Types.Byte_Array;
                            Data_Offset : in     Natural;
                            Byte_Len    : in     Natural)
     with Global => null,
     Pre => (Data'Length mod Vector_Width = 0
             and then Data_Offset <= Data'Length / Vector_Width
             and then Byte_Len <= (Data'Length / Vector_Width) - Data_Offset);
   --  Extract bytes from the Keccak state.
   --
   --  The @Data@ array is split into N equal sized chunks, where N is the
   --  number of parallel instances. The bytes extracted from each Keccak
   --  state is then copied into the each chunk, offset by @Data_Offset@.
   --  An example is shown below for Keccak-f[1600]×2 (i.e. 2 parallel
   --  instances):
   --
   --    DO    BL             DO    BL
   --  |--->|<---->|        |--->|<---->|
   --  +-----------------------------------------+
   --  |                    |                    | Data
   --  +-----------------------------------------+
   --       |  ^   |             |  ^   |
   --       | Read |             | Read |
   --       |      |             |      |
   --       +-----------+        +-----------+
   --       |  state 0  |        |  state 1  |
   --       +-----------+        +-----------+
   --
   --  The bytes are always read from the beginning of the Keccak state.
   --
   --  @param S The Keccak state to read bytes from.
   --
   --  @param Data Bytes extracted from the Keccak state are copied to this
   --     array, offset according to @Data_Offset@.
   --
   --  @param Data_Offset The offset in the @Data@ array to mark the position
   --     of the first byte in each chunk.
   --
   --  @param Byte_Len The number of bytes to read from each chunk.

private

   type X_Coord is mod 5;
   type Y_Coord is mod 5;

   type Parallel_State is array (X_Coord, Y_Coord) of VXXI_View;

end Keccak.Generic_Parallel_KeccakF;
