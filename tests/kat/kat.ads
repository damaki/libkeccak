-------------------------------------------------------------------------------
-- Copyright (c) 2016, Daniel King
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

with Ada.Containers.Vectors;
with Ada.Finalization;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Keccak.Types;

package KAT
is
   type Byte_Array_Access is access Keccak.Types.Byte_Array;

   type KAT_Test is new Ada.Finalization.Controlled with record
      Line   : Natural;
      Len    : Natural;
      Repeat : Natural;
      Msg    : Byte_Array_Access;
      MD     : Byte_Array_Access;
   end record;
   
   procedure Initialize(T : in out KAT_Test);
   procedure Adjust    (T : in out KAT_Test);
   procedure Finalize  (T : in out KAT_Test);
   
   type Duplex_KAT_Test is new Ada.Finalization.Controlled with record
      Line     : Natural;
      In_Len   : Natural;
      In_Data  : Byte_Array_Access;
      Out_Len  : Natural;
      Out_Data : Byte_Array_Access;
   end record;
   
   procedure Initialize(T : in out Duplex_KAT_Test);
   procedure Adjust    (T : in out Duplex_KAT_Test);
   procedure Finalize  (T : in out Duplex_KAT_Test);
   
   
   type CSHAKE_KAT_Test is new Ada.Finalization.Controlled with record
      Line     : Natural;
      N_Data   : Unbounded_String;
      S_Data   : Unbounded_String;
      In_Len   : Natural;
      In_Data  : Byte_Array_Access;
      Out_Len  : Natural;
      Out_Data : Byte_Array_Access;
   end record;
   
   procedure Initialize(T : in out CSHAKE_KAT_Test);
   procedure Adjust    (T : in out CSHAKE_KAT_Test);
   procedure Finalize  (T : in out CSHAKE_KAT_Test);
   
   package KAT_Vectors is new Ada.Containers.Vectors(Element_Type => KAT_Test,
                                                     Index_Type   => Natural);
   
   package Duplex_KAT_Vectors is new Ada.Containers.Vectors(Element_Type => Duplex_KAT_Test,
                                                            Index_Type   => Natural);
   
   package CSHAKE_KAT_Vectors is new Ada.Containers.Vectors(Element_Type => CSHAKE_KAT_Test,
                                                            Index_Type   => Natural);
   
   procedure Load_Test_Vectors(File_Name  : in     String;
                               Tests      :    out KAT_Vectors.Vector;
                               Align_Bits : in     Boolean);
   -- Opens a text file and loads test vectors.
   --
   -- The test vectors in the file must be in one of the formats
   -- shown below (data values are examples)
   --
   --     Len = 16
   --     Msg = 02CD
   --     MD = 0123456789ABCDEF0123456789ABCDEF
   --
   -- or
   --
   --     Repeat = 16777216
   --     Text = abcdefghijklmnopqrstuvwxyz
   --     MD = 0123456789ABCDEF0123456789ABCDEF
   
   procedure Load_Duplex_Test_Vectors(File_Name : in     String;
                                      Tests     :    out Duplex_KAT_Vectors.Vector);
   
   procedure Load_CSHAKE_Test_Vectors (File_Name : in     String;
                                       Tests     :    out CSHAKE_KAT_Vectors.Vector);
                                      
   -- Helper function to convert a byte array to a hex string.
   function Byte_Array_To_String(Data : in Keccak.Types.Byte_Array) return String;
   

end KAT;
