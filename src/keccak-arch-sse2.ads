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
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

--  @brief@
--  Type definitions and subprograms for SSE2 vector instructions.
package Keccak.Arch.SSE2
is

   type V2DI_Index is range 0 .. 1;

   type V2DI is array (V2DI_Index) of Unsigned_64
     with Alignment => 16;
   pragma Machine_Attribute (V2DI, "vector_type");
   pragma Machine_Attribute (V2DI, "may_alias");


   type V2DI_View is array (V2DI_Index) of Unsigned_64
     with Alignment => 16;


   function Load is new Ada.Unchecked_Conversion
     (Source => V2DI_View,
      Target => V2DI);


   function Store is new Ada.Unchecked_Conversion
     (Source => V2DI,
      Target => V2DI_View);


   function "and" (A, B : in V2DI) return V2DI
     with Global => null;
   pragma Import (Intrinsic, "and", "__builtin_ia32_pand128");


   function "xor" (A, B : in V2DI) return V2DI
     with Global => null;
   pragma Import (Intrinsic, "xor", "__builtin_ia32_pxor128");


   function And_Not (A, B : in V2DI) return V2DI
     with Global => null;
   pragma Import (Intrinsic, And_Not, "__builtin_ia32_pandn128");


   function Shift_Left (A      : in V2DI;
                        Amount : in Natural) return V2DI
     with Global => null;
   pragma Import (Intrinsic, Shift_Left, "__builtin_ia32_psllqi128");


   function Shift_Right (A      : in V2DI;
                         Amount : in Natural) return V2DI
     with Global => null;
   pragma Import (Intrinsic, Shift_Right, "__builtin_ia32_psrlqi128");


   function Rotate_Left (A      : in V2DI;
                         Amount : in Natural) return V2DI
   is (Shift_Left (A, Amount) xor Shift_Right (A, 64 - Amount))
   with Inline,
   Global => null;


end Keccak.Arch.SSE2;
