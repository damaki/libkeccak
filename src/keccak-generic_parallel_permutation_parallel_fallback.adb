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
package body Keccak.Generic_Parallel_Permutation_Parallel_Fallback
is

   function To_Input_State_Index (I : in State_Index) return Input_State_Index
   is (Input_State_Index
       (Integer (Input_State_Index'First) +
          (Integer (I) mod (Num_Parallel_Instances / 2))))
   with Inline;


   procedure Init (S : out Parallel_State)
   is
   begin
      Init (S.States (0));
      Init (S.States (1));
   end Init;


   procedure Permute_All (S : in out Parallel_State)
   is
   begin
      Permute (S.States (0));
      Permute (S.States (1));
   end Permute_All;


   procedure XOR_Bits_Into_State (S       : in out Parallel_State;
                                  Index   : in     State_Index;
                                  Data    : in     Types.Byte_Array;
                                  Bit_Len : in     Natural)
   is
   begin
      XOR_Bits_Into_State
        (S       => S.States (Integer (Index / 2)),
         Index   => To_Input_State_Index (Index),
         Data    => Data,
         Bit_Len => Bit_Len);
   end XOR_Bits_Into_State;


   procedure Extract_Bytes (S     : in     Parallel_State;
                            Index : in     State_Index;
                            Data  :    out Types.Byte_Array)
   is
   begin
      Extract_Bytes
        (S       => S.States (Integer (Index / 2)),
         Index   => To_Input_State_Index (Index),
         Data    => Data);
   end Extract_Bytes;


   procedure Extract_Bits (S       : in     Parallel_State;
                           Index   : in     State_Index;
                           Data    :    out Types.Byte_Array;
                           Bit_Len : in     Natural)
   is
   begin
      Extract_Bits
        (S       => S.States (Integer (Index / 2)),
         Index   => To_Input_State_Index (Index),
         Data    => Data,
         Bit_Len => Bit_Len);
   end Extract_Bits;

end Keccak.Generic_Parallel_Permutation_Parallel_Fallback;
