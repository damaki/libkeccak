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
with Keccak.Types;

package body Sponge_Tests
is

   procedure Set_Up(T : in out Test)
   is
   begin
      Sponge.Init(T.Ctx, Capacity);
   end Set_Up;
   
   

   -- Test that streaming works when absorbing data.
   --
   -- This test takes a 512 byte message and breaks it up into equal-sized
   -- chunks (sometimes the last chunk is smaller) and absorbs it into the
   -- sponge using multiple calls to Absorb. The test checks that for each
   -- possible chunk size (1 .. 511 byte chunks) the Sponge always produces
   -- exactly the same output.
   procedure Test_Absorb_Streaming(T : in out Test)
   is
      use type Keccak.Types.Byte_Array;
      
      Input_Data      : Keccak.Types.Byte_Array(0 .. 512);
      Baseline_Output : Keccak.Types.Byte_Array(0 .. 512);
      Output_Data     : Keccak.Types.Byte_Array(0 .. 512);
      
      Num_Chunks : Natural;
   
   begin
      -- Setup the input data
      for I in Input_Data'Range loop
         Input_Data(I) := Keccak.Types.Byte(I mod 256);
      end loop;
      
      -- Get the baseline output by passing in the entire input in 1 call to Absorb
      Sponge.Absorb(T.Ctx, Input_Data, Input_Data'Length * 8);
      Sponge.Squeeze(T.Ctx, Baseline_Output);
      
      -- Break up the Input_Data into chunks of varying sizes, and check
      -- that the squeezed output is the same as the Baseline_Output.
      for Chunk_Size in Positive range 1 .. Input_Data'Length - 1 loop
         Sponge.Init(T.Ctx, Capacity);
         
         Num_Chunks := Input_Data'Length / Chunk_Size;
      
         for N in Natural range 0 .. Num_Chunks - 1 loop
            Sponge.Absorb(T.Ctx,
                          Input_Data(N*Chunk_Size .. (N*Chunk_Size + Chunk_Size) - 1),
                          Chunk_Size * 8);
         end loop;
         
         -- Last chunk, if necessary
         if Input_Data'Length mod Chunk_Size /= 0 then
            Sponge.Absorb(T.Ctx,
                          Input_Data(Chunk_Size * (Input_Data'Length / Chunk_Size) .. Input_Data'Last),
                          (Input_Data'Length mod Chunk_Size) * 8);
         end if;
         
         -- Get the output and compare it against the baseline
         Sponge.Squeeze(T.Ctx, Output_Data);
         Assert(Output_Data = Baseline_Output,
                "Streaming test failed for" & Positive'Image(Chunk_Size) &
                " byte chunks");
      end loop;
   
   end Test_Absorb_Streaming;
   
   

   -- Test that streaming works when squeezing data.
   --
   -- This test verifies that the sponge always outputs the same data sequence,
   -- regardless of how much data is output for each call to Squeeze.
   --
   -- The test iterates through chunk sizes from 1 .. 511 bytes. For each chunk
   -- size the test squeezes 512 bytes of data. For example, for 2 byte chunks
   -- the test calls Squeeze 256 times to generate 512 bytes of output, and this
   -- output should be exactly the same as the 512 bytes generated from reading
   -- 1 bytes per call to Squeeze.
   procedure Test_Squeeze_Streaming(T : in out Test)
   is
      use type Keccak.Types.Byte_Array;
      
      Input_Data      : Keccak.Types.Byte_Array(0 .. 512);
      Baseline_Output : Keccak.Types.Byte_Array(0 .. 512);
      Output_Data     : Keccak.Types.Byte_Array(0 .. 512);
      
      Num_Chunks : Natural;
   
   begin
      -- Setup the input data
      for I in Input_Data'Range loop
         Input_Data(I) := Keccak.Types.Byte(I mod 256);
      end loop;
      
      -- Get the baseline output by passing in the entire input in 1 call to Absorb
      Sponge.Absorb(T.Ctx, Input_Data, Input_Data'Length * 8);
      Sponge.Squeeze(T.Ctx, Baseline_Output);
      
      -- Break up the Output_Data into chunks of varying sizes, and check
      -- that the squeezed output is the same as the Baseline_Output.
      for Chunk_Size in Positive range 1 .. Input_Data'Length - 1 loop
         Sponge.Init(T.Ctx, Capacity);
         Sponge.Absorb(T.Ctx, Input_Data, Input_Data'Length * 8);
         
         Num_Chunks := Input_Data'Length / Chunk_Size;
      
         for N in Natural range 0 .. Num_Chunks - 1 loop
            Sponge.Squeeze(T.Ctx,
                           Output_Data(N*Chunk_Size .. (N*Chunk_Size + Chunk_Size) - 1));
         end loop;
         
         -- Last chunk, if necessary
         if Input_Data'Length mod Chunk_Size /= 0 then
            Sponge.Squeeze(T.Ctx,
                           Output_Data(Chunk_Size * (Output_Data'Length / Chunk_Size) .. Output_Data'Last));
         end if;
         
         -- Compare it against the baseline
         Assert(Output_Data = Baseline_Output,
                "Streaming test failed for" & Positive'Image(Chunk_Size) &
                " byte chunks");
      end loop;
   
   end Test_Squeeze_Streaming;
   
   
   -- Test that Absorb_With_Suffix is equivalent to Absorb
   -- when the Suffix_Size is 0.
   procedure Test_Absorb_No_Suffix(T : in out Test)
   is
      use type Keccak.Types.Byte_Array;
      
      Input_Data      : Keccak.Types.Byte_Array(0 .. 512);
      Baseline_Output : Keccak.Types.Byte_Array(0 .. 512);
      Output_Data     : Keccak.Types.Byte_Array(0 .. 512);
      
   begin
      -- Setup the input data
      for I in Input_Data'Range loop
         Input_Data(I) := Keccak.Types.Byte(I mod 256);
      end loop;
     
      -- Test all possible bit-lengths in the range 1 .. 512
      for I in Positive range 1 .. Input_Data'Length loop
         -- Generate the baseline output with Absorb
         Sponge.Init(T.Ctx, Capacity);
         Sponge.Absorb(T.Ctx, Input_Data, I);
         Sponge.Squeeze(T.Ctx, Baseline_Output);
         
         -- Do the same with Append_With_Suffix, but squeeze to Output_Data
         Sponge.Init(T.Ctx, Capacity);
         Sponge.Absorb_With_Suffix(T.Ctx,
                                   Input_Data,
                                   I,
                                   0,  -- Suffix
                                   0); -- Suffix_Size
         Sponge.Squeeze(T.Ctx, Output_Data);
         
         pragma Assert(Output_Data = Baseline_Output,
                       "Failed with input data of" & Positive'Image(I)
                       & " bits");
         
      end loop;
   end Test_Absorb_No_Suffix;


end Sponge_Tests;
