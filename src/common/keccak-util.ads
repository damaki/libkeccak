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
with Interfaces;
with Keccak.Types;

package Keccak.Util
with SPARK_Mode => On
is
   use type Interfaces.Unsigned_8;

   procedure To_Byte_Array (Bytes :    out Types.Byte_Array;
                            Str   : in     String)
     with Inline,
     Relaxed_Initialization => Bytes,
     Global => null,
     Pre => Bytes'Length = Str'Length,
     Post => Bytes'Initialized;
   --  Return the byte array representation of a string.
   --
   --  This function uses an Ada.Unchecked_Conversion internally to convert the
   --  String to the byte array.
   --
   --  @param Str The string to convert to a byte array.

   function Left_Encode_NIST (Length : in Natural) return Types.Byte_Array
     with
       Post => (Left_Encode_NIST'Result'Length in 1 .. (Natural'Size / 8) + 2
                and Left_Encode_NIST'Result'First in 1 .. (Natural'Size / 8) + 2);
   --  Encode a length using the left_encode(n) method described in
   --  NIST SP 800-185.
   --
   --  Example, the length 16#ABCDEF# will be encoded as the byte array
   --  (3, 16#AB#, 16#CD#, 16#EF#)

   function Left_Encode_NIST_Bit_Length (Byte_Length : in Natural)
                                         return Types.Byte_Array
     with
       Post => (Left_Encode_NIST_Bit_Length'Result'Length
                  in 1 .. Natural'Size + 1

                and Left_Encode_NIST_Bit_Length'Result'First
                  in 1 .. Natural'Size + 1);
   --  A version of left_encode(n) (as defined by NIST) where the output
   --  represents the input value multiplied by 8.
   --
   --  The output of calling Left_Encode_NIST_Bit_Length (N) is equivalent
   --  to calling Left_Encode_NIST (N * 8). This function is useful to avoid
   --  doing the multiplication in the call to Left_Encode_NIST and thus
   --  avoiding the possibility of an integer overflow.
   --
   --  @param Byte_Length The length to left_encode. Usually represents a
   --     quantity of bytes, as the output represents this value multiplied by 8.

   function Right_Encode_NIST (Length : in Natural) return Types.Byte_Array
     with
       Post => (Right_Encode_NIST'Result'Length
                  in 1 .. (Natural'Size / 8) + 2

                and Right_Encode_NIST'Result'First
                  in 1 .. (Natural'Size / 8) + 2);
   --  Encode a length using the right_encode(n) method described in
   --  NIST SP 800-185.
   --
   --  Example, the length 16#ABCDEF# will be encoded as the byte array
   --  (16#AB#, 16#CD#, 16#EF#, 3)

   function Right_Encode_NIST_Long_Long (Length : in Long_Long_Integer)
                                         return Types.Byte_Array
     with
       Pre => Length >= 0,
       Post => (Right_Encode_NIST_Long_Long'Result'Length
                  in 1 .. (Long_Long_Integer'Size / 8) + 2

                and Right_Encode_NIST_Long_Long'Result'First
                  in 1 .. (Long_Long_Integer'Size / 8) + 2);
   --  Equivalent to Right_Encode_NIST, except this function accepts a much
   --  longer input range (as a Long_Long_Integer).

   function Right_Encode_NIST_Bit_Length (Byte_Length : in Natural)
                                          return Types.Byte_Array
     with
       Post => (Right_Encode_NIST_Bit_Length'Result'Length
                  in 1 .. Natural'Size + 1

                and Right_Encode_NIST_Bit_Length'Result'First
                  in 1 .. Natural'Size + 1);
   --  A version of right_encode(n) (as defined by NIST) where the output
   --  represents the input value multiplied by 8.
   --
   --  The output of calling Right_Encode_NIST_Bit_Length (N) is equivalent
   --  to calling Right_Encode_NIST (N * 8). This function is useful to avoid
   --  doing the multiplication in the call to Right_Encode_NIST and thus
   --  avoiding the possibility of an integer overflow.
   --
   --  @param Byte_Length The length to right_encode. Usually represents a
   --     quantity of bytes, as the output represents this value multiplied by 8.

   function Right_Encode_K12 (Length : in Long_Long_Integer)
                              return Types.Byte_Array
     with
       Pre => Length >= 0,
       Post => (Right_Encode_K12'Result'Length
                  in 1 .. (Long_Long_Integer'Size / 8) + 2

                and Right_Encode_K12'Result'First
                  in 1 .. (Long_Long_Integer'Size / 8) + 2);
   --  Version of right_encode(n) as defined by the KangarooTwelve document.
   --
   --  The definition of right_encode(n) in the KangarooTwelve document is
   --  different to the definition in NIST SP 800-185, and they produce different
   --  outputs.

   procedure Compare (A1          : in     Keccak.Types.Byte_Array;
                      A2          : in     Keccak.Types.Byte_Array;
                      Accumulator : in out Keccak.Types.Byte)
     with Global => null,
     Depends => (Accumulator =>+ (A1, A2)),
     Pre => A1'Length = A2'Length;
   --  Compare two arrays in constant time.
   --
   --  If the two arrays are equal, then the Accumulator will retain its
   --  initial value (e.g. zero). Otherwise, the Accumulator will be set
   --  to a non-zero value.

end Keccak.Util;
