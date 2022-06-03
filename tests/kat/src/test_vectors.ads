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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Finalization;                      use Ada.Finalization;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Keccak.Types;

package Test_Vectors is

   type Byte_Array_Access is access Keccak.Types.Byte_Array;

   type Value_Type is
     (String_Type,
      Integer_Type,
      Hex_Array_Type);

   type Value_Choice (VType : Value_Type) is
     new Ada.Finalization.Controlled with
      record
         case VType is
            when String_Type =>
               Str : Unbounded_String;

            when Integer_Type =>
               Int : Integer;

            when Hex_Array_Type =>
               Hex : Byte_Array_Access;
         end case;
      end record;

   overriding procedure Initialize (Object : in out Value_Choice);
   overriding procedure Adjust     (Object : in out Value_Choice);
   overriding procedure Finalize   (Object : in out Value_Choice);

   package Value_Choice_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type => Value_Choice,
      "="          => "=");

   type Schema_Entry is record
      VType    : Value_Type := Integer_Type;
      Required : Boolean    := True;
      Is_List  : Boolean    := False;
   end record;

   package Schema_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Schema_Entry,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   --  Maps test vector keys to their required types.

   package Test_Vector_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Value_Choice_Lists.List,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=",
      "="             => Value_Choice_Lists."=");
   --  Stores all information relating to a single test vector.
   --  For example, the line: MD = 00112233 from the test vector file
   --  will be stored in the map with the key "MD" and the value "00112233"
   --  as a byte array or string (depending on the schema).

   package Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Test_Vector_Maps.Map,
      "="          => Test_Vector_Maps."=");
   --  A list of test vectors.

   function Hex_String_To_Byte_Array (Str : in String) return Byte_Array_Access;
   --  Convert a string of hex characters to a byte array.
   --
   --  E.g. a string "01afBC" is converted to the array (16#01#, 16#AF#, 16#BC#)

   function String_To_Byte_Array (Str : in String) return Byte_Array_Access;
   --  Convert a string to its byte array representation.
   --
   --  This is just a conversion of each Character to its Byte representation.

   function Byte_Array_To_String (Data : in Keccak.Types.Byte_Array) return String;
   --  Convert a byte array to a hex string representation.

   procedure Load (File_Name    : in     String;
                   Schema       : in     Schema_Maps.Map;
                   Vectors_List :    out Lists.List);
   --  Load test vectors from a file.
   --
   --  A test vector is in the form: Key = Value where the Value may optionally
   --  have quotes " around it.
   --
   --  For example:
   --     Len = 17
   --     Msg = 4FF400
   --     MD = 94D5B162A324674454BBADB377375DA15C3BE74225D346010AE557A9
   --
   --  Test vectors should be separated by a blank line, but is not mandatory
   --  provided that each test vector has the same keys.
   --
   --  Comments appear on their own line and start with the '#' character.
   --
   --  The schema defines which Keys are expected, the required type of the
   --  corresponding Value.

   Schema_Error : exception;

private

   procedure Parse_Line (Test_Vector  : in out Test_Vector_Maps.Map;
                         Vectors_List : in out Lists.List;
                         Schema       : in     Schema_Maps.Map;
                         Line         : in     String);

   procedure Add_Test_Vector_Key_Value_Pair (Test_Vector  : in out Test_Vector_Maps.Map;
                                             Vectors_List : in out Lists.List;
                                             Schema       : in     Schema_Maps.Map;
                                             Key          : in     Unbounded_String;
                                             Value        : in     Unbounded_String);

   procedure Append_Test_Vector (Test_Vector  : in out Test_Vector_Maps.Map;
                                 Vectors_List : in out Lists.List;
                                 Schema       : in     Schema_Maps.Map);

end Test_Vectors;
