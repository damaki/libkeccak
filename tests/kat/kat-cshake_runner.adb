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

with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Keccak.Types;

package body KAT.CSHAKE_Runner
is

   procedure Run_Tests
   is
      use type Keccak.Types.Byte_Array;
      use type Ada.Command_Line.Exit_Status;
      
      package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);
      
      Tests : CSHAKE_KAT_Vectors.Vector;
      
      Ctx      : CSHAKE.Context;
      Out_Data : Keccak.Types.Byte_Array(0 .. 1600);
      Out_Last : Keccak.Types.Index_Number;
      Out_Len  : Natural;
      
      Num_Passed  : Natural := 0;
      Num_Failed  : Natural := 0;
      
   begin
      -- The KAT file name is given on the command line.
      -- Check that the argument is present.
      if Ada.Command_Line.Argument_Count /= 1 then
         Ada.Text_IO.Put("Error: missing file name parameter");
         
         Ada.Command_Line.Set_Exit_Status(-1);
         
      else
      
         -- Load the test file using the file name given on the command line
         Ada.Text_IO.Put_Line("Loading file: " & Ada.Command_Line.Argument(1));
         Load_CSHAKE_Test_Vectors(Ada.Command_Line.Argument(1), Tests);
         
         Ada.Text_IO.Put("Running ");
         Integer_IO.Put(Integer(Tests.Length), Width => 0);
         Ada.Text_IO.Put_Line(" tests ...");
      
         -- Run each test.
         for C in Tests.Iterate loop
         
            CSHAKE.Init (Ctx           => Ctx,
                         Customization => To_String(CSHAKE_KAT_Vectors.Element(C).S_Data),
                         Function_Name => To_String(CSHAKE_KAT_Vectors.Element(C).N_Data));
                         
            CSHAKE.Update (Ctx        => Ctx,
                           Message    => CSHAKE_KAT_Vectors.Element(C).In_Data.all,
                           Bit_Length => CSHAKE_KAT_Vectors.Element(C).In_Len);
            
            Out_Len  := CSHAKE_KAT_Vectors.Element(C).Out_Len;
            Out_Last := Out_Data'First + (((Out_Len + 7) / 8) - 1);
                           
            CSHAKE.Extract (Ctx    => Ctx,
                            Digest => Out_Data (Out_Data'First .. Out_Last));
            
            -- Mask any unused bits from the output.
            if Out_Len mod 8 /= 0 then
                Out_Data(Out_Last) := Out_Data(Out_Last) and Keccak.Types.Byte((2**(Out_Len mod 8)) - 1);
            end if;
            
            if Out_Data(0 .. Out_Last) = CSHAKE_KAT_Vectors.Element(C).Out_Data.all then
               Num_Passed := Num_Passed + 1;
            else
               Num_Failed := Num_Failed + 1;
               
               -- Display a message on failure to help with debugging.
               Ada.Text_IO.Put("FAILURE (In_Len: ");
               Integer_IO.Put(CSHAKE_KAT_Vectors.Element(C).In_Len, Width => 0);
               Ada.Text_IO.Put_Line(")");
               
               Ada.Text_IO.Put("   Expected Out_Data: ");
               Ada.Text_IO.Put(Byte_Array_To_String(CSHAKE_KAT_Vectors.Element(C).Out_Data.all));
               Ada.Text_IO.New_Line;
               
               Ada.Text_IO.Put("   Actual Out_Data:   ");
               Ada.Text_IO.Put(Byte_Array_To_String(Out_Data(0 .. Out_Last)));
               Ada.Text_IO.New_Line;
            end if;
         end loop;
         
         -- Print results
         
         Ada.Text_IO.New_Line;
         
         Ada.Text_IO.Put("Passed: ");
         Integer_IO.Put(Num_Passed, Width => 0);
         Ada.Text_IO.New_Line;
         
         Ada.Text_IO.Put("Failed: ");
         Integer_IO.Put(Num_Failed, Width => 0);
         Ada.Text_IO.New_Line;
         
         -- Allow external tool (e.g. make) to detect failed tests.
         if Num_Failed > 0 then
            Ada.Command_Line.Set_Exit_Status(1);
         end if;
         
      end if;
      
   end Run_Tests;

end KAT.CSHAKE_Runner;
