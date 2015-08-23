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

with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Keccak.Types;

package body KAT.Runner
is
   procedure Run_Tests(Align_Bits : in Boolean)
   is
      use type Keccak.Types.Byte_Array;
      use type Ada.Command_Line.Exit_Status;
      
      package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);
      
      Tests : KAT_Vectors.Vector;
      
      Ctx    : Hash.Context;
      Digest : Hash.Digest_Type;
      
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
         Load_Test_Vectors(Ada.Command_Line.Argument(1), Tests, Align_Bits);
         
         Ada.Text_IO.Put("Running ");
         Integer_IO.Put(Integer(Tests.Length), Width => 0);
         Ada.Text_IO.Put_Line(" tests ...");
      
         -- Run each test.
         for C in Tests.Iterate loop
            Hash.Init(Ctx);
            
            if KAT_Vectors.Element(C).Len > 0 then
               for I in Natural range 1 .. KAT_Vectors.Element(C).Repeat loop
                  Hash.Update(Ctx,
                              KAT_Vectors.Element(C).Msg.all,
                              KAT_Vectors.Element(C).Len);
               end loop;
            end if;
            
            Hash.Final(Ctx, Digest);
            
            if Digest = KAT_Vectors.Element(C).MD.all then
               Num_Passed := Num_Passed + 1;
            else
               Num_Failed := Num_Failed + 1;
               
               -- Display a message on failure to help with debugging.
               Ada.Text_IO.Put("FAILURE (Msg bit-len: ");
               Integer_IO.Put(KAT_Vectors.Element(C).Len, Width => 0);
               Ada.Text_IO.Put_Line(")");
               
               Ada.Text_IO.Put("   Expected MD: ");
               Ada.Text_IO.Put(Byte_Array_To_String(KAT_Vectors.Element(C).MD.all));
               Ada.Text_IO.New_Line;
               
               Ada.Text_IO.Put("   Actual MD:   ");
               Ada.Text_IO.Put(Byte_Array_To_String(Digest));
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

end KAT.Runner;
