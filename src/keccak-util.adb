with Ada.Unchecked_Conversion;

package body Keccak.Util
with SPARK_Mode => On
is

   function To_Byte_Array (Str : in String) return Types.Byte_Array
   is

      subtype Fixed_String is String (Str'Range);
      subtype Fixed_Byte_Array is Types.Byte_Array (Str'Range);

      function String_To_Byte_Array_Conversion is new Ada.Unchecked_Conversion
        (Source => Fixed_String,
         Target => Fixed_Byte_Array);

   begin
      return String_To_Byte_Array_Conversion (Str);
   end To_Byte_Array;

end Keccak.Util;
