-------------------------------------------------------------------------------
-- Copyright (c) 2015, Daniel King
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

with AUnit.Assertions; use AUnit.Assertions;

package body KeccakF_Tests
is
   use type Keccak.Types.Byte_Array;

   procedure Set_Up(T : in out Test)
   is
   begin
      KeccakF.Init(T.State);
   end Set_Up;


   -- Tests that the initial state is all zeroes.
   --
   -- According to The Keccak Reference, Section 1.3, the initial state (the
   -- "root state" is all zeroes).
   procedure Test_Initial_State(T : in out Test)
   is
      State_Bytes    : Keccak.Types.Byte_Array(1 .. KeccakF.B/8);
      
      -- The initial state is expected to be all zeroes.
      Expected_State : Keccak.Types.Byte_Array(1 .. KeccakF.B/8) := (others => 0);
      
   begin
      Extract_Bytes(T.State, State_Bytes);
      
      Assert(State_Bytes = Expected_State,
             "Initial state is not all zeroes");
      
   end Test_Initial_State;
   
   
   -- Test that the XOR_Bits_Into_State procedure does not change the
   -- state when Bit_Len = 0.
   procedure Test_XOR_No_Data(T : in out Test)
   is
      Empty_Array : Keccak.Types.Byte_Array(0 .. -1) := (others => 0);
      
      Pre_State  : Keccak.Types.Byte_Array(1 .. KeccakF.B/8);
      Post_State : Keccak.Types.Byte_Array(1 .. KeccakF.B/8);
      
   begin
      Extract_Bytes(T.State, Pre_State);
      
      XOR_Bits_Into_State(T.State, Empty_Array, 0);
      
      Extract_Bytes(T.State, Post_State);
      
      Assert(Pre_State = Post_State,
             "The state was changed after XOR'ing 0 bits");
   end Test_XOR_No_Data;
   
   
   -- Test that XOR_Bits_Into_State works for each independent byte
   -- in the state.
   procedure Test_XOR_Entire_State(T : in out Test)
   is
      use type Keccak.Types.Byte;
   
      Data_To_XOR : Keccak.Types.Byte_Array(1 .. (KeccakF.B + 7)/8);
      Result      : Keccak.Types.Byte_Array(1 .. (KeccakF.B + 7)/8);
      
   begin
      -- Setup the data with a sequence: 1, 2, 3, 4, 5, ...
      -- This allows us to check that each byte is not affected by other bytes.
      for I in Natural range 0 .. Data_To_XOR'Length - 1 loop
         Data_To_XOR(Data_To_XOR'First + I) := Keccak.Types.Byte(I + 1);
      end loop;
      
      if KeccakF.B mod 8 /= 0 then
         Data_To_XOR(Data_To_XOR'Last) := Data_To_XOR(Data_To_XOR'Last) and (2**(KeccakF.B mod 8) - 1);
      end if;
      
      -- XOR in the data. After XOR'ing with a zero state we should end up
      -- with the same data after reading the Keccak state.
      XOR_Bits_Into_State(T.State, Data_To_XOR, KeccakF.B);
      Extract_Bytes(T.State, Result);
      
      for I in Data_To_XOR'Range loop
            Assert(Result(I) = Data_To_XOR(I),
                   "Expected byte at index" & Positive'Image(I) &
                   " to be" & Keccak.Types.Byte'Image(Data_To_XOR(I)) &
                   ", but instead got:" & Keccak.Types.Byte'Image(Result(I)));
      end loop;
   end Test_XOR_Entire_State;
   
   
   -- This test loops for data whose length is in the range 1 .. 1600.
   -- For each iteration, the test XORs in the data, and then reads the
   -- data and checks that the Keccak state is correct after XORing.
   procedure Test_XOR_Bit_Length(T : in out Test)
   is
      use type Keccak.Types.Byte;
   
      Data_To_XOR : Keccak.Types.Byte_Array(1 .. (KeccakF.B + 7) / 8)
         := (others => 2#1111_1111#);
      Result      : Keccak.Types.Byte_Array(1 .. (KeccakF.B + 7) / 8);
      
      Expected_Last_Byte : Keccak.Types.Byte;
      Last_Byte          : Keccak.Types.Byte;
         
   begin
      for I in Positive range 1 .. KeccakF.B loop
      
         KeccakF.Init(T.State);
         XOR_Bits_Into_State(T.State, Data_To_XOR, I);
         
         Result := (others => 0);
         Extract_Bytes(T.State, Result);
         
         -- Whole bytes
         for J in Positive range 1 .. ((I + 7) / 8) - 1 loop
            Assert(Result(J) = 255,
                   "After XORing" & Positive'Image(I) &
                   " bits, expected byte at index" & Positive'Image(J) &
                   " to be 255, but instead got:" & Keccak.Types.Byte'Image(Result(J)));
         end loop;
         
         -- Last byte, if we don't expect it to be 2#1111_1111#
         if I mod 8 /= 0 then
            Expected_Last_Byte := (2**(I mod 8) - 1);
            Last_Byte          := Result((I + 7) / 8);
            Assert(Last_Byte = Expected_Last_Byte,
                   "After XORing" & Positive'Image(I) &
                   " bits, expected last byte to be" & Keccak.Types.Byte'Image(Expected_Last_Byte) &
                   ", but instead got:" & Keccak.Types.Byte'Image(Last_Byte));
         end if;
                   
         -- Check that the remaining bytes are 0.
         for J in Positive range ((I + 7) / 8) + 1 .. Result'Last loop
            Assert(Result(J) = 0,
                   "After XORing " & Positive'Image(I) &
                   " bits, expected byte at index" & Positive'Image(J) &
                   " to be 0, but instead got: " & Keccak.Types.Byte'Image(Result(J)));
         end loop;
      end loop;
   end Test_XOR_Bit_Length;
   
   
   
   -- This procedure tests that Extract_Bytes reads the correct number of bytes
   -- for all possible lengths;
   procedure Test_Extract_Bytes(T : in out Test)
   is
      use type Keccak.Types.Byte;
   
      Data   : Keccak.Types.Byte_Array(1 .. (KeccakF.B + 7) / 8);
      Result : Keccak.Types.Byte_Array(1 .. (KeccakF.B + 7) / 8);
   
   begin
      -- Initialize each value in 'Data' with a unique value
      -- and setup the keccak state with the data
      for I in Positive range 1 .. (KeccakF.B + 7) / 8 loop
         Data(I) := Keccak.Types.Byte(I);
      end loop;
      XOR_Bits_Into_State(T.State, Data, KeccakF.B);
      
      
      -- Now call Extract_Bytes for all possible output lengths,
      -- checking that the correct data is returned.
      for I in Natural range 0 .. KeccakF.B / 8 loop
         Result := (others => 0);
         
         Extract_Bytes(T.State, Result(1 .. I));
         
         for J in Natural range 1 .. I loop
            Assert(Result(J) = Data(J),
                   "Expected byte at index" & Natural'Image(J) &
                   " to be " & Keccak.Types.Byte'Image(Data(J)) &
                   ", but instead got: " & Keccak.Types.Byte'Image(Result(J)));
         end loop;
         
         for J in Natural range I+1 .. KeccakF.B / 8 loop
            Assert(Result(J) = 0,
                   "Expected byte at index" & Natural'Image(J) &
                   " to be 0, but instead got: " & Keccak.Types.Byte'Image(Result(J)));
         end loop;
      end loop;
   
   end Test_Extract_Bytes;

end KeccakF_Tests;
