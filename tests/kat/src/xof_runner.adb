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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Keccak.Types;

with Test_Vectors; use Test_Vectors;

package body XOF_Runner
is

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Keccak.Types.Byte_Array,
      Name   => Byte_Array_Access);

   procedure Run_Tests (File_Name  : in     String;
                        Num_Passed :    out Natural;
                        Num_Failed :    out Natural)
   is
      use type Keccak.Types.Byte_Array;

      package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);

      Len_Key : constant Unbounded_String := To_Unbounded_String ("Len");
      Msg_Key : constant Unbounded_String := To_Unbounded_String ("Msg");

      Repeat_Key : constant Unbounded_String := To_Unbounded_String ("Repeat");
      Text_Key   : constant Unbounded_String := To_Unbounded_String ("Text");

      Output_Key  : constant Unbounded_String := To_Unbounded_String ("Output");

      Schema : Test_Vectors.Schema_Maps.Map;
      Tests  : Test_Vectors.Lists.List;

      Ctx    : XOF.Context;
      Digest : Byte_Array_Access := null;

      Msg : Byte_Array_Access;

      Len : Natural;

   begin
      Num_Passed := 0;
      Num_Failed := 0;

      --  Setup schema to support two types of test vector files:
      --  Long or ShortMsgKAT containing: "Len", "Msg", and "Output" fields; and
      --  ExtremelyLongMsgKAT containing: "Repeat", "Text", and "Output" fields.
      Schema.Insert (Key      => Len_Key,
                     New_Item => Schema_Entry'(VType    => Integer_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Msg_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Repeat_Key,
                     New_Item => Schema_Entry'(VType    => Integer_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Text_Key,
                     New_Item => Schema_Entry'(VType    => String_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Output_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => True,
                                               Is_List  => False));

      --  Load the test file using the file name given on the command line
      Ada.Text_IO.Put_Line ("Loading file: " & File_Name);

      Test_Vectors.Load (File_Name    => File_Name,
                         Schema       => Schema,
                         Vectors_List => Tests);

      Ada.Text_IO.Put ("Running ");
      Integer_IO.Put (Integer (Tests.Length), Width => 0);
      Ada.Text_IO.Put_Line (" tests ...");

      --  Run each test.
      for C of Tests loop
         XOF.Init (Ctx);

         if C.Contains (Len_Key) then
            --  Test vector contains "Len", "Msg", and "Output" fields

            Len := C.Element (Len_Key).First_Element.Int;

            if Len > 0 then
               Msg := new Keccak.Types.Byte_Array'(C.Element (Msg_Key).First_Element.Hex.all);

               XOF.Update (Ctx, Msg.all, Len);

               Free (Msg);
            end if;

         else
            --  Assume test vector defines "Repeat", "Text", and "Output" fields

            Msg := String_To_Byte_Array (To_String (C.Element (Text_Key).First_Element.Str));

            for I in 1 .. C.Element (Repeat_Key).First_Element.Int loop
               XOF.Update (Ctx, Msg.all);
            end loop;

            Free (Msg);
         end if;

         Digest := new Keccak.Types.Byte_Array (C.Element (Output_Key).First_Element.Hex.all'Range);

         XOF.Extract (Ctx, Digest.all);

         if Digest.all = C.Element (Output_Key).First_Element.Hex.all then
            Num_Passed := Num_Passed + 1;
         else
            Num_Failed := Num_Failed + 1;

            --  Display a message on failure to help with debugging.
            if C.Contains (Len_Key) then
               Ada.Text_IO.Put ("FAILURE (Msg bit-len: ");
               Integer_IO.Put (C.Element (Len_Key).First_Element.Int, Width => 0);
               Ada.Text_IO.Put_Line (")");
            else
               Ada.Text_IO.Put_Line ("FAILURE:");
            end if;

            Ada.Text_IO.Put ("   Expected Output: ");
            Ada.Text_IO.Put (Byte_Array_To_String (C.Element (Output_Key).First_Element.Hex.all));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put ("   Actual Output:   ");
            Ada.Text_IO.Put (Byte_Array_To_String (Digest.all));
            Ada.Text_IO.New_Line;
         end if;

         Free (Digest);
      end loop;

   end Run_Tests;

end XOF_Runner;
