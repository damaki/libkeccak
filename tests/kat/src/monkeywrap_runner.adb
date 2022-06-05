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
with Keccak.Types;

with Test_Vectors; use Test_Vectors;

package body MonkeyWrap_Runner
is

   Key_Key        : constant Unbounded_String := To_Unbounded_String ("Key");
   Nonce_Key      : constant Unbounded_String := To_Unbounded_String ("Nonce");

   AAD_Key        : constant Unbounded_String := To_Unbounded_String ("AAD");
   Plaintext_Key  : constant Unbounded_String := To_Unbounded_String ("Plaintext");
   Ciphertext_Key : constant Unbounded_String := To_Unbounded_String ("Ciphertext");
   Tag_Key        : constant Unbounded_String := To_Unbounded_String ("Tag");

   GlobalTag_Key  : constant Unbounded_String := To_Unbounded_String ("GlobalTag");

   procedure Do_Session (Ctx        : in out MonkeyWrap.Context;
                         Test_Data  : in     Test_Vectors.Test_Vector_Maps.Map;
                         Num_Passed : in out Natural;
                         Num_Failed : in out Natural);

   procedure Run_Tests (File_Name  : in     String;
                        Num_Passed :    out Natural;
                        Num_Failed :    out Natural)
   is
      use type Keccak.Types.Byte_Array;

      package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);

      Schema : Test_Vectors.Schema_Maps.Map;
      Tests  : Test_Vectors.Lists.List;

      Ctx    : MonkeyWrap.Context;

   begin
      Num_Passed := 0;
      Num_Failed := 0;

      --  Setup schema
      Schema.Insert (Key      => Key_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Nonce_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => AAD_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Plaintext_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Ciphertext_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => Tag_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
                                               Is_List  => False));
      Schema.Insert (Key      => GlobalTag_Key,
                     New_Item => Schema_Entry'(VType    => Hex_Array_Type,
                                               Required => False,
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
      --
      --  Note: The tests must be executed in the correct order, since each
      --        KAT test continues from the state of the previous test.
      --        This is why the context is only initialized once before the
      --        tests start (see above).
      for C of Tests loop
         if C.Contains (Key_Key) then

            MonkeyWrap.Init (Ctx   => Ctx,
                             Key   => C.Element (Key_Key).First_Element.Hex.all,
                             Nonce => C.Element (Nonce_Key).First_Element.Hex.all);

         elsif C.Contains (AAD_Key) then
            Do_Session (Ctx        => Ctx,
                        Test_Data  => C,
                        Num_Passed => Num_Passed,
                        Num_Failed => Num_Failed);

         elsif C.Contains (GlobalTag_Key) then
            declare
               Tag : Keccak.Types.Byte_Array
                 (C.Element (GlobalTag_Key).First_Element.Hex.all'Range);

            begin
               MonkeyWrap.Extract_Tag (Ctx => Ctx,
                                       Tag => Tag);

               --  Check output
               if Tag = C.Element (GlobalTag_Key).First_Element.Hex.all then
                  Num_Passed := Num_Passed + 1;
               else
                  Num_Failed := Num_Failed + 1;

                  --  Display a message on failure to help with debugging.
                  Ada.Text_IO.Put_Line ("FAILURE:");

                  Ada.Text_IO.Put ("   Expected GlobalTag: ");
                  Ada.Text_IO.Put
                    (Byte_Array_To_String (C.Element (GlobalTag_Key).First_Element.Hex.all));
                  Ada.Text_IO.New_Line;

                  Ada.Text_IO.Put ("   Actual GlobalTag:   ");
                  Ada.Text_IO.Put (Byte_Array_To_String (Tag));
                  Ada.Text_IO.New_Line;
               end if;

               MonkeyWrap.New_Session (Ctx);
            end;
         end if;

      end loop;

   end Run_Tests;

   procedure Do_Session (Ctx        : in out MonkeyWrap.Context;
                         Test_Data  : in     Test_Vectors.Test_Vector_Maps.Map;
                         Num_Passed : in out Natural;
                         Num_Failed : in out Natural)
   is
      use type Keccak.Types.Byte_Array;

      Ciphertext : Keccak.Types.Byte_Array
        (Test_Data.Element (Plaintext_Key).First_Element.Hex.all'Range);

      Tag        : Keccak.Types.Byte_Array
        (Test_Data.Element (Tag_Key).First_Element.Hex.all'Range);

   begin
      if Test_Data.Element (AAD_Key).First_Element.Hex.all'Length > 0 then
         MonkeyWrap.Update_Auth_Data (Ctx  => Ctx,
                                    Data => Test_Data.Element (AAD_Key).First_Element.Hex.all);
      end if;

      if Ciphertext'Length > 0 then
         MonkeyWrap.Update_Encrypt
           (Ctx        => Ctx,
            Plaintext  => Test_Data.Element (Plaintext_Key).First_Element.Hex.all,
            Ciphertext => Ciphertext);
      end if;

      MonkeyWrap.Extract_Tag (Ctx => Ctx,
                              Tag => Tag);

      --  Check output
      if Ciphertext = Test_Data.Element (Ciphertext_Key).First_Element.Hex.all then
         if Tag = Test_Data.Element (Tag_Key).First_Element.Hex.all then
            Num_Passed := Num_Passed + 1;
         else
            Num_Failed := Num_Failed + 1;

            --  Display a message on failure to help with debugging.
            Ada.Text_IO.Put_Line ("FAILURE:");

            Ada.Text_IO.Put ("   Expected Tag: ");
            Ada.Text_IO.Put
              (Byte_Array_To_String (Test_Data.Element (Tag_Key).First_Element.Hex.all));
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put ("   Actual Tag:   ");
            Ada.Text_IO.Put (Byte_Array_To_String (Tag));
            Ada.Text_IO.New_Line;
         end if;
      else
         Num_Failed := Num_Failed + 1;

         --  Display a message on failure to help with debugging.
         Ada.Text_IO.Put_Line ("FAILURE:");

         Ada.Text_IO.Put ("   Expected CT: ");
         Ada.Text_IO.Put
           (Byte_Array_To_String (Test_Data.Element (Ciphertext_Key).First_Element.Hex.all));
         Ada.Text_IO.New_Line;

         Ada.Text_IO.Put ("   Actual CT:   ");
         Ada.Text_IO.Put (Byte_Array_To_String (Ciphertext));
         Ada.Text_IO.New_Line;
      end if;

      MonkeyWrap.New_Session (Ctx);
   end Do_Session;

end MonkeyWrap_Runner;
