with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package Keccak.Arch.SSE2
is

   type V2DI is array (0 .. 1) of Unsigned_64
     with Alignment => 16;
   pragma Machine_Attribute (V2DI, "vector_type");
   pragma Machine_Attribute (V2DI, "may_alias");


   type V2DI_View is array (0 .. 1) of Unsigned_64
     with Alignment => 16;


   function Load is new Ada.Unchecked_Conversion
     (Source => V2DI_View,
      Target => V2DI);


   function Store is new Ada.Unchecked_Conversion
     (Source => V2DI,
      Target => V2DI_View);


   function "and" (A, B : in V2DI) return V2DI;
   pragma Import (Intrinsic, "and", "__builtin_ia32_pand128");


   function "xor" (A, B : in V2DI) return V2DI;
   pragma Import (Intrinsic, "xor", "__builtin_ia32_pxor128");


   function And_Not (A, B : in V2DI) return V2DI;
   pragma Import (Intrinsic, And_Not, "__builtin_ia32_pandn128");


   function Shift_Left (A      : in V2DI;
                        Amount : in Natural) return V2DI;
   pragma Import (Intrinsic, Shift_Left, "__builtin_ia32_psllqi128");


   function Shift_Right (A      : in V2DI;
                         Amount : in Natural) return V2DI;
   pragma Import (Intrinsic, Shift_Right, "__builtin_ia32_psrlqi128");


   function Rotate_Left (A      : in V2DI;
                         Amount : in Natural) return V2DI
   is (Shift_Left (A, Amount) xor Shift_Right (A, 64 - Amount))
   with Inline;


private


end Keccak.Arch.SSE2;
