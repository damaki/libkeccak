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
   type Permutation_State is private;
   type Input_State_Index is range <>;
   with procedure Init (A : out Permutation_State);
   with procedure Permute (A : in out Permutation_State);
   with procedure XOR_Bits_Into_State (S       : in out Permutation_State;
                                       Index   : in     Input_State_Index;
                                       Data    : in     Types.Byte_Array;
                                       Bit_Len : in     Natural);
   with procedure Extract_Bytes (S     : in     Permutation_State;
                                 Index : in     Input_State_Index;
                                 Data  :    out Types.Byte_Array);
   with procedure Extract_Bits (S       : in     Permutation_State;
                                Index   : in     Input_State_Index;
                                Data    :    out Types.Byte_Array;
                                Bit_Len : in     Natural);
   State_Size_Bits    : Positive;

   --  @brief@
   --  Simulates a higher-order level of parallelism from lower-order parallelism.
   --
   --  @description@
   --  This package uses a lower-order parallelism (e.g. 2x parllel) to simulate
   --  higher-order parallelism (e.g. 4x). Basically, it doubles the number
   --  of parallel instances, but runs each of the instances serially
   --  For example, it makes two 2x parallel instances look like one 4x parallel
   --  instance.
   --  For example, this package can be used to simulate Keccak-p[1600,24]×8
   --  by serially invoking 2 separate instances of Keccak-[1600,24]×4.
   --
   --  This package is useful in cases where a high order of parallelism is
   --  required (e.g. 8x) by an API, but such an implementation is not available.
   --
   --  Instances of this package can be chained. For example, if you want to
   --  have a fallback for 8x parallelism, but you only have a 2x implementation,
   --  then you can double the 2x into 4x, then double again the 4x into 8x.
package Keccak.Generic_Parallel_Permutation_Parallel_Fallback
is

   Num_Parallel_Instances : constant Positive :=
     2 * (Integer (Input_State_Index'Last) - Integer (Input_State_Index'First) + 1);

   type Parallel_State is private;

   type State_Index is new Natural range 0 .. Num_Parallel_Instances - 1;

   procedure Init (S : out Parallel_State);


   procedure Permute_All (S : in out Parallel_State);


   procedure XOR_Bits_Into_State (S       : in out Parallel_State;
                                  Index   : in     State_Index;
                                  Data    : in     Types.Byte_Array;
                                  Bit_Len : in     Natural);


   procedure Extract_Bytes (S     : in     Parallel_State;
                            Index : in     State_Index;
                            Data  :    out Types.Byte_Array);


   procedure Extract_Bits (S       : in     Parallel_State;
                           Index   : in     State_Index;
                           Data    :    out Types.Byte_Array;
                           Bit_Len : in     Natural);

private

   type Permutation_State_Array is array (0 .. 1) of Permutation_State;

   type Parallel_State is record
      States : Permutation_State_Array;
   end record;

end Keccak.Generic_Parallel_Permutation_Parallel_Fallback;
