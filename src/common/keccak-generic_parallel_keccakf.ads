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
--  Implements an optimized Keccak-f permutation based on SIMD instruction sets
--  such as SSE, NEON, and AVX.
--
--  @description
--  This package provides a basis for parallel implementations of Keccak
--  for processing N separate permutations in parallel, where N is the number
--  of vector components in the SIMD instruction set used.
--
--  When instantiating this package, subprograms and types are provided as
--  formal generic parameters which implement each of the required operations
--  for the target instruction set.
--
--  @group Parallel Keccak-f
generic
   Lane_Size_Log : in Positive;
   --  The binary logarithm of the lane size.
   --
   --  This determines the Keccak-f state size. Allowed values are:
   --  * Lane_Size_Log = 3 => 8-bit lanes,  Keccak-f[200]
   --  * Lane_Size_Log = 4 => 16-bit lanes, Keccak-f[400]
   --  * Lane_Size_Log = 5 => 32-bit lanes, Keccak-f[800]
   --  * Lane_Size_Log = 6 => 64-bit lanes, Keccak-f[1600]

   type Lane_Type is mod <>;
   --  Lane type e.g. Unsigned_64.

   type VXXI_Index is range <>;
   --  Index type into the vector component.

   type VXXI is private;
   --  Vector type.

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
   --  parallelism (the upper 2 vector components would not be used).

   with function Load (X : in VXXI_View) return VXXI;

   with function Store (X : in VXXI) return VXXI_View;

   with function "xor" (A, State_Size_Bits : in VXXI) return VXXI;
   --  Calculates A xor State_Size_Bits per vector component.

   with function Rotate_Left (A      : in VXXI;
                              Amount : in Natural) return VXXI;
   --  Calculates Rotate_Left(A) per vector component.

   with function And_Not (A, State_Size_Bits : in VXXI) return VXXI;
   --  Calculates State_Size_Bits and (not A) per vector component.

   with function Shift_Left (A      : in Lane_Type;
                             Amount : in Natural) return Lane_Type;

   with function Shift_Right (A      : in Lane_Type;
                              Amount : in Natural) return Lane_Type;

package Keccak.Generic_Parallel_KeccakF
is
   Lane_Size_Bits : constant Positive := 2**Lane_Size_Log;
   --  Lane size (in bits, i.e. 8, 16, 32, or 64)

   State_Size_Bits : constant Positive := Lane_Size_Bits * 25;
   --  Keccak-f state size (in bits).

   Num_Parallel_Instances : constant Positive := Vector_Width;

   pragma Assert (Lane_Size_Bits mod 8 = 0,
                  "Generic_Parallel_KeccakF only supports Lane_Size_Log in 3 .. 6");

   pragma Assert (Vector_Width in 1 .. VXXI_View'Length,
                  "Vector_Width exceeds vector type's width");

   type Parallel_State is private;

   Initialized_State : constant Parallel_State;

   type Round_Index is range 0 .. 23;

   subtype Round_Count is Positive range 1 .. 24;

   procedure Init (S : out Parallel_State)
     with Global => null;
   --  Initialise the Keccak state to all zeroes.

   generic
      First_Round : Round_Index := 0;
      Num_Rounds  : Round_Count := 24;
   procedure Permute_All (S : in out Parallel_State)
     with Global => null;
   --  Applies the Keccak-f permutation function to all N parallel states.
   --
   --  This generic function is a Keccak-p permutation which is
   --  instantiated, with the number rounds, into a Keccak-f instance.

   procedure XOR_Bits_Into_State_Separate
     (S           : in out Parallel_State;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
     with Global => null,
     Pre => (Data'Length / Vector_Width <= Natural'Last / 8
             and then Data'Length mod Vector_Width = 0
             and then Data_Offset <= (Data'Length / Vector_Width)
             and then Bit_Len <= ((Data'Length / Vector_Width) - Data_Offset) * 8
             and then Bit_Len <= State_Size_Bits);
   --  XOR separate data into each parallel Keccak instance.
   --
   --  The @Data@ array contains the data to be XORed into all parallel
   --  instances. The bytes in @Data@ are split into equal chunks depending on
   --  the number of parallel instances. For example, for Keccak-f[1600]�2 the
   --  @Data@ array is split into 2 chunks as shown below:
   --
   --     . DO    BL             DO    BL
   --     |--->|<---->|        |--->|<---->|
   --     +-----------------------------------------+
   --     |                    |                    | Data
   --     +-----------------------------------------+
   --     .    |      |             |      |
   --     .    | XOR  |             | XOR  |
   --     .    |  v   |             |  v   |
   --     .    +-----------+        +-----------+
   --     .    |  state 0  |        |  state 1  |
   --     .    +-----------+        +-----------+
   --
   --  Where DO = Data_Offset and BL = Bit_Len
   --
   --  The @Data_Offset@ determines the offset within each chunk to start
   --  reading data. @Bit_Len@ determines the number of bits to read from each
   --  chunk.
   --
   --  The data is always XORed starting at the beginning of the Keccak state.
   --
   --  @param S The parallel Keccak state to where the bits are XORed.
   --
   --  @param Data The array containing the data to XOR into the parallel state.
   --      The size of this array must be a multiple of the number of parallel
   --      instances. For Example, for Keccak-f[1600]�4 then Data'Length
   --      must be a multiple of 4.
   --
   --  @param Data_Offset Offset of the first byte(s) to read from the @Data@
   --      array.
   --
   --  @param Bit_Len The number of bits to XOR into each state.

   procedure XOR_Bits_Into_State_All
     (S           : in out Parallel_State;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural)
     with Global => null,
     Depends => (S =>+ (Data, Bit_Len)),
     Pre => (Data'Length <= Natural'Last / 8
             and then Bit_Len <= Data'Length * 8
             and then Bit_Len <= State_Size_Bits);
   --  XOR the same data into all parallel Keccak instances.
   --
   --  The @Data@ array contains the data to be XORed into all parallel
   --  instances. For example, for Keccak-f[1600]�2 this would be as follows:
   --
   --     .        BL
   --     .  |<--------->|
   --     .  +----------------+
   --     .  |                | Data
   --     .  +----------------+
   --     .          /\
   --     .     XOR /  \ XOR
   --     .      v /    \ v
   --     +-----------+-----------+
   --     |  state 0  |  state 1  |
   --     +-----------+-----------+
   --
   --  Where BL = Bit_Len
   --
   --  The data is always XORed starting at the beginning of the Keccak state.
   --
   --  @param S The parallel Keccak state to where the bits are XORed.
   --
   --  @param Data The array containing the data to XOR into each parallel state.
   --
   --  @param Bit_Len The length of the data, in bits.

   procedure Extract_Bytes (S           : in     Parallel_State;
                            Data        :    out Types.Byte_Array;
                            Data_Offset : in     Natural;
                            Byte_Len    : in     Natural)
     with Global => null,
     Pre => (Data'Length mod Vector_Width = 0
             and then Data_Offset <= Data'Length / Vector_Width
             and then Byte_Len <= (Data'Length / Vector_Width) - Data_Offset
             and then Byte_Len <= State_Size_Bits / 8);
   --  Extract bytes from the Keccak state.
   --
   --  The @Data@ array is split into N equal sized chunks, where N is the
   --  number of parallel instances. The bytes extracted from each Keccak
   --  state is then copied into the each chunk, offset by @Data_Offset@.
   --  An example is shown below for Keccak-f[1600]�2 (i.e. 2 parallel
   --  instances):
   --
   --     . DO    BL             DO    BL
   --     |--->|<---->|        |--->|<---->|
   --     +-----------------------------------------+
   --     |                    |                    | Data
   --     +-----------------------------------------+
   --     .    |  ^   |             |  ^   |
   --     .    | Read |             | Read |
   --     .    |      |             |      |
   --     .    +-----------+        +-----------+
   --     .    |  state 0  |        |  state 1  |
   --     .    +-----------+        +-----------+
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
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "Data is partially initialized in chunks of size 'Byte_Len'" &
        " at offset 'Data_Offset' in each chunk");

private

   type X_Coord is mod 5;
   type Y_Coord is mod 5;

   type Parallel_State is array (X_Coord, Y_Coord) of VXXI_View;

   Initialized_State : constant Parallel_State := (others => (others => (others => 0)));

end Keccak.Generic_Parallel_KeccakF;
