-------------------------------------------------------------------------------
-- Copyright (c) 2019, Daniel King
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with Keccak.Types;

with Test_Vectors; use Test_Vectors;

package body Duplex_Runner
is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Keccak.Types.Byte_Array,
      Name   => Byte_Array_Access);

   procedure Run_Tests (File_Name  : in     String;
                        Capacity   : in     Positive;
                        Num_Passed :    out Natural;
                        Num_Failed :    out Natural)
   is
      use type Keccak.Types.Byte_Array;

      package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);

      InLen_Key  : constant Unbounded_String := To_Unbounded_String("InLen");
      In_Key     : constant Unbounded_String := To_Unbounded_String("In");
      OutLen_Key : constant Unbounded_String := To_Unbounded_String("OutLen");
      Out_Key    : constant Unbounded_String := To_Unbounded_String("Out");

      Schema : Test_Vectors.Schema_Maps.Map;
      Tests  : Test_Vectors.Lists.List;

      Ctx    : Duplex.Context;

      Output : Byte_Array_Access;

      OutLen : Natural;

   begin
      Num_Passed := 0;
      Num_Failed := 0;

      --  Setup schema
      Schema.Insert (Key      => InLen_Key,
                     New_Item => Schema_Entry'(VType    => Integer_Type,
                                               Required => True,
                                               Is_List  => False));
      Schema.Insert (Key      => In_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => True,
                                               Is_List  => False));
      Schema.Insert (Key      => OutLen_Key,
                     New_Item => Schema_Entry'(VType    => Integer_Type,
                                               Required => True,
                                               Is_List  => False));
      Schema.Insert (Key      => Out_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => True,
                                               Is_List  => False));

      -- Load the test file using the file name given on the command line
      Ada.Text_IO.Put_Line("Loading file: " & File_Name);

      Test_Vectors.Load (File_Name    => File_Name,
                         Schema       => Schema,
                         Vectors_List => Tests);

      Ada.Text_IO.Put ("Running ");
      Integer_IO.Put (Integer (Tests.Length), Width => 0);
      Ada.Text_IO.Put_Line (" tests ...");

      Duplex.Init (Ctx      => Ctx,
                   Capacity => Capacity);

      -- Run each test.
      --
      -- Note: The tests must be executed in the correct order, since each
      --       KAT test continues from the state of the previous test.
      --       This is why the context is only initialized once before the
      --       tests start (see above).
      for C of Tests loop
         Output := new Keccak.Types.Byte_Array (C.Element (Out_Key).First_Element.Hex.all'Range);

         Duplex.Duplex (Ctx                 => Ctx,
                        In_Data             => C.Element (In_Key).First_Element.Hex.all,
                        In_Data_Bit_Length  => C.Element (InLen_Key).First_Element.Int,
                        Out_Data            => Output.all,
                        Out_Data_Bit_Length => C.Element (OutLen_Key).First_Element.Int);

         -- Mask any unused bits from the output.
         OutLen := C.Element (OutLen_Key).First_Element.Int;
         if OutLen mod 8 /= 0 then
            Output.all(Output.all'Last) := Output.all(Output.all'Last) and Keccak.Types.Byte((2**(OutLen mod 8)) - 1);
         end if;

         --  Check output
         if Output.all = C.Element(Out_Key).First_Element.Hex.all then
            Num_Passed := Num_Passed + 1;
         else
            Num_Failed := Num_Failed + 1;

            -- Display a message on failure to help with debugging.
            Ada.Text_IO.Put("FAILURE (Input bit-len: ");
            Integer_IO.Put(C.Element (InLen_Key).First_Element.Int, Width => 0);
            Ada.Text_IO.Put_Line(")");

            Ada.Text_IO.Put("   Expected MD: ");
            Ada.Text_IO.Put(Byte_Array_To_String (C.Element(Out_Key).First_Element.Hex.all));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put("   Actual MD:   ");
            Ada.Text_IO.Put(Byte_Array_To_String(Output.all));
            Ada.Text_IO.New_Line;
         end if;

         Free (Output);
      end loop;

   end Run_Tests;

end Duplex_Runner;
