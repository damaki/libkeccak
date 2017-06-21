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
   type KeccakF_State is private;
   with procedure Init (A : out KeccakF_State);
   with procedure XOR_Bits_Into_State (S       : in out KeccakF_State;
                                       Data    : in     Types.Byte_Array;
                                       Bit_Len : in     Natural);
   with procedure Extract_Bytes (A    : in     KeccakF_State;
                                 Data :    out Types.Byte_Array);
   with procedure Extract_Bits (A       : in     KeccakF_State;
                                Data    :    out Types.Byte_Array;
                                Bit_Len : in     Natural);
   State_Size_Bits    : Positive;

   Parallelism : Positive;
   --  Specifies the number of simulated parallel instances.

   --  @brief@
   --  Serial fallback implementation of the parallel permutation API.
   --
   --  @description@
   --  This package implements subprograms for a parallel permutation API,
   --  but implements the permutation serially.
   --
   --  For example, this package can be used to simulate Keccak-p[1600,24]×8
   --  by serially invoking 8 separate instances of Keccak-[1600,24].
package Keccak.Generic_Parallel_Permutation_Serial_Fallback
is
   Num_Parallel_Instances : constant Positive := Parallelism;

   type Parallel_State is private;

   type State_Index is new Natural range 0 .. Parallelism - 1;

   procedure Init (S : out Parallel_State)
     with Global => null;


   generic
      with procedure Permute (A : in out KeccakF_State);
   procedure Permute_All (S : in out Parallel_State)
     with Global => null;


   procedure XOR_Bits_Into_State (S       : in out Parallel_State;
                                  Index   : in     State_Index;
                                  Data    : in     Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Global => null,
     Pre => (Data'Length <= Natural'Last / 8
             and then Bit_Len <= Data'Length * 8
             and then Bit_Len <= State_Size_Bits);


   procedure Extract_Bytes (S     : in     Parallel_State;
                            Index : in     State_Index;
                            Data  :    out Types.Byte_Array)
     with Global => null,
     Pre => (Data'Length <= ((State_Size_Bits + 7) / 8));


   procedure Extract_Bits (S       : in     Parallel_State;
                           Index   : in     State_Index;
                           Data    :    out Types.Byte_Array;
                           Bit_Len : in     Natural)
     with Global => null,
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);

private

   type KeccakF_State_Array is array (State_Index) of KeccakF_State;

   type Parallel_State is record
      States : KeccakF_State_Array;
   end record;

end Keccak.Generic_Parallel_Permutation_Serial_Fallback;
