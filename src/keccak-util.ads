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
with Interfaces;
with Keccak.Types;

package Keccak.Util
with SPARK_Mode => On
is
   use type Interfaces.Unsigned_8;

   function To_Byte_Array (Str : in String) return Types.Byte_Array
     with Inline,
     Post => (To_Byte_Array'Result'Length = Str'Length
              and To_Byte_Array'Result'First = Str'First
              and To_Byte_Array'Result'Last = Str'Last);
   -- Return the byte array representation of a string.
   --
   -- @param Str The string to convert to a byte array.

   function Left_Encode(Length : in Natural) return Types.Byte_Array
     with
       Post => (Left_Encode'Result'Length in 1 .. (Natural'Size / 8) + 2
                and Left_Encode'Result'First in 1 .. (Natural'Size / 8) + 2);
   --  Encode a length using the left_encode(n) method described in the
   --  proposed CSHAKE document from NIST.
   --
   --  Example, the length 16#ABCDEF# will be encoded as the byte array
   --  (3, 16#AB#, 16#CD#, 16#EF#)

   function Left_Encode_Bit_Length(Byte_Length : in Natural) return Types.Byte_Array
     with
       Post => (Left_Encode_Bit_Length'Result'Length in 1 .. Natural'Size + 1
                and Left_Encode_Bit_Length'Result'First in 1 .. Natural'Size + 1);

   function Right_Encode (Length : in Natural) return Types.Byte_Array
     with
       Post => (Right_Encode'Result'Length in 1 .. (Natural'Size / 8) + 2
                and Right_Encode'Result'First in 1 .. (Natural'Size / 8) + 2);
   --  Encode a length using the right_encode(n) method described in the
   --  proposed CSHAKE document from NIST.
   --
   --  Example, the length 16#ABCDEF# will be encoded as the byte array
   --  (16#AB#, 16#CD#, 16#EF#, 3)

   function Right_Encode_Bit_Length(Byte_Length : in Natural) return Types.Byte_Array
     with
       Post => (Right_Encode_Bit_Length'Result'Length in 1 .. Natural'Size + 1
                and Right_Encode_Bit_Length'Result'First in 1 .. Natural'Size + 1);

   function Right_Encode_K12 (Length : in Natural) return Types.Byte_Array
     with
       Post => (Right_Encode_K12'Result'Length in 1 .. (Natural'Size / 8) + 2
                and Right_Encode_K12'Result'First in 1 .. (Natural'Size / 8) + 2);

end Keccak.Util;
