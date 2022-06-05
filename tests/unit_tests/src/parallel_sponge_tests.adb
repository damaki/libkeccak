-------------------------------------------------------------------------------
--  Copyright (c) 2017, Daniel King
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
with Keccak.Types; use Keccak.Types;
with AUnit.Assertions; use AUnit.Assertions;

package body Parallel_Sponge_Tests
is

   Suffix     : constant Byte := 2#101#;
   Suffix_Len : constant := 3;

   overriding
   procedure Set_Up (T : in out Test)
   is
   begin
      Parallel_Sponge.Init (T.Ctx);
   end Set_Up;

   ----------------------------------------------------------------------------
   --  Test that the output of a parallel sponge with N parallel instances
   --  produces the same output as N serial sponges.
   --
   --  Each parallel instance is fed different input data to detect problems
   --  where the wrong data is fed into the wrong instance.
   procedure Test_Same_Output_As_Serial (T : in out Test)
   is
      N                : constant Positive := Parallel_Sponge.Num_Parallel_Instances;

      Input_Len        : constant := 64 * 1024;
      Output_Len       : constant := 2  * 1024;

      Input            : Byte_Array (0 .. (Input_Len * N) - 1);

      Reference_Output : Byte_Array (0 .. (Output_Len * N) - 1) := (others => 0);
      Actual_Output    : Byte_Array (0 .. (Output_Len * N) - 1) := (others => 0);

      Serial_Ctx       : Serial_Sponge.Context;

      --  Repeat the test with some different lengths.
      Test_Lengths     : constant array (Natural range <>) of Natural :=
                           (0, 1, 10, 1000, 1024, 2048, 4096, Input_Len / 2, Input_Len);

   begin

      --  Repeat the test in increasing lengths of input data, from 0 .. Input_Len bytes.
      for Test_Len of Test_Lengths loop
         --  Set up each instance to receive different data.
         --  Instance 0 has the repeating pattern 16#00#
         --  Instance 1 has the repeating pattern 16#11#
         --  Instance 2 has the repeating pattern 16#22#
         --  and so on...
         for I in 0 .. N - 1 loop
            Input ((I * Test_Len) .. (I * Test_Len) + Test_Len - 1) :=
              (others => Byte ((16#11# * I) mod 256));
         end loop;

         --  Use the serial algorithm to produce the reference output to compare
         --  against.
         for I in 0 .. N - 1 loop
            Serial_Sponge.Init (Serial_Ctx, Capacity);

            Serial_Sponge.Absorb_With_Suffix
            (Ctx        => Serial_Ctx,
               Message    => Input ((I * Test_Len) .. (I * Test_Len) + Test_Len - 1),
               Bit_Length => Test_Len * 8,
               Suffix     => Suffix,
               Suffix_Len => Suffix_Len);

            Serial_Sponge.Squeeze
              (Ctx    => Serial_Ctx,
               Digest => Reference_Output ((I * Output_Len) .. (I * Output_Len) + Output_Len - 1));
         end loop;

         --  Run the parallel sponge
         Parallel_Sponge.Init (T.Ctx);

         Parallel_Sponge.Absorb_Bytes_Separate_With_Suffix
           (Ctx        => T.Ctx,
            Data       => Input (0 .. (Test_Len * N) - 1),
            Suffix     => Suffix,
            Suffix_Len => Suffix_Len);

         Parallel_Sponge.Squeeze_Bytes_Separate
           (Ctx  => T.Ctx,
            Data => Actual_Output);

         Assert (Actual_Output = Reference_Output,
                "Output of parallel sponge does not match serial sponge for Test_Len =" &
                  Integer'Image (Test_Len));
      end loop;
   end Test_Same_Output_As_Serial;

end Parallel_Sponge_Tests;
