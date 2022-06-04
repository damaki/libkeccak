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
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

--  @brief@
--  Type definitions and subprograms for AVX2 vector instructions.
package Keccak.Arch.AVX2
with SPARK_Mode => On
is

   pragma Warnings (GNATprove, Off,
                    "pragma ""Machine_Attribute"" ignored (not yet supported)");

   -------------------------
   --  4x 64-bit vectors  --
   -------------------------

   package V4DI_Vectors
   is

      type V4DI_Index is range 0 .. 3;

      type V4DI is array (V4DI_Index) of Unsigned_64
        with Alignment => 32,
        Size => 256,
        Object_Size => 256;
      pragma Machine_Attribute (V4DI, "vector_type");
      pragma Machine_Attribute (V4DI, "may_alias");

      type V4DI_View is array (V4DI_Index) of Unsigned_64
        with Alignment => 32,
        Size => 256,
        Object_Size => 256;

      function Load is new Ada.Unchecked_Conversion
        (Source => V4DI_View,
         Target => V4DI);

      function Store is new Ada.Unchecked_Conversion
        (Source => V4DI,
         Target => V4DI_View);

      function "and" (A, B : in V4DI) return V4DI
        with Global => null;
      pragma Import (Intrinsic, "and", "__builtin_ia32_andsi256");

      function "xor" (A, B : in V4DI) return V4DI
        with Global => null;
      pragma Import (Intrinsic, "xor", "__builtin_ia32_pxor256");

      function And_Not (A, B : in V4DI) return V4DI
        with Global => null;
      pragma Import (Intrinsic, And_Not, "__builtin_ia32_andnotsi256");

      function Shift_Left (A      : in V4DI;
                           Amount : in Natural) return V4DI
        with Global => null;
      pragma Import (Intrinsic, Shift_Left, "__builtin_ia32_psllqi256");

      function Shift_Right (A      : in V4DI;
                            Amount : in Natural) return V4DI
        with Global => null;
      pragma Import (Intrinsic, Shift_Right, "__builtin_ia32_psrlqi256");

      function Rotate_Left (A      : in V4DI;
                            Amount : in Natural) return V4DI
      is (Shift_Left (A, Amount) xor Shift_Right (A, 64 - Amount))
      with Inline,
      Global => null,
      Pre => Amount <= 64;

   end V4DI_Vectors;

end Keccak.Arch.AVX2;
