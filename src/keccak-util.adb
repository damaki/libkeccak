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

   function Left_Encode(Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array(1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant(N = I - 1);

         Encoded(Encoded'Last - N) := Types.Byte(X mod 256);

         X := X / 256;
         N := N + 1;

         exit when X = 0;

      end loop;

      Encoded(Encoded'Last - N) := Types.Byte(N);

      return Encoded(Encoded'Last - N .. Encoded'Last);
   end Left_Encode;

   function Left_Encode_Bit_Length(Byte_Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array(1 .. Natural'Size + 1) := (others => 0);

      X       : Natural := Byte_Length;
      N       : Natural := 0;

   begin

      Encoded(Encoded'Last - N) := Types.Byte(X mod 256) * 8;

      X := X / 32;
      N := N + 1;

      for I in Positive range 2 .. Natural'Size loop
         pragma Loop_Invariant(N = I - 1);

         exit when X = 0;

         Encoded(Encoded'Last - N) := Types.Byte(X mod 256);

         X := X / 256;
         N := N + 1;

      end loop;

      Encoded(Encoded'Last - N) := Types.Byte(N);

      return Encoded(Encoded'Last - N .. Encoded'Last);
   end Left_Encode_Bit_Length;

   function Right_Encode(Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array(1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant(N = I - 1);

         Encoded(Encoded'Last - (N + 1)) := Types.Byte(X mod 256);

         X := X / 256;
         N := N + 1;

         exit when X = 0;

      end loop;

      Encoded(Encoded'Last) := Types.Byte(N);

      return Encoded(Encoded'Last - N .. Encoded'Last);
   end Right_Encode;

   function Right_Encode_Bit_Length(Byte_Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array(1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Byte_Length;
      N       : Natural := 0;

   begin

      Encoded(Encoded'Last - (N + 1)) := Types.Byte(X mod 256) * 8;

      X := X / 32;
      N := N + 1;

      for I in Positive range 2 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant(N = I - 1);

         exit when X = 0;

         Encoded(Encoded'Last - (N + 1)) := Types.Byte(X mod 256);

         X := X / 256;
         N := N + 1;

      end loop;

      Encoded(Encoded'Last) := Types.Byte(N);

      return Encoded(Encoded'Last - N .. Encoded'Last);
   end Right_Encode_Bit_Length;

   function Right_Encode_K12(Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array(1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant(N = I - 1);

         exit when X = 0;

         Encoded(Encoded'Last - (N + 1)) := Types.Byte(X mod 256);

         X := X / 256;
         N := N + 1;

      end loop;

      Encoded(Encoded'Last) := Types.Byte(N);

      return Encoded(Encoded'Last - N .. Encoded'Last);
   end Right_Encode_K12;

end Keccak.Util;
