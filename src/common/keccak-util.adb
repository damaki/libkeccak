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
package body Keccak.Util
with SPARK_Mode => On
is

   ---------------------
   --  To_Byte_Array  --
   ---------------------

   procedure To_Byte_Array (Bytes :    out Types.Byte_Array;
                            Str   : in     String)
   is
   begin
      for I in 0 .. Bytes'Length - 1 loop
         Bytes (Bytes'First + I) := Types.Byte (Character'Pos (Str (Str'First + I)));

         pragma Loop_Invariant (Bytes (Bytes'First .. Bytes'First + I)'Initialized);
      end loop;
   end To_Byte_Array;

   ------------------------
   --  Left_Encode_NIST  --
   ------------------------

   function Left_Encode_NIST (Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array (1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant (N = I - 1);

         Encoded (Encoded'Last - N) := Types.Byte (X mod 256);

         X := X / 256;
         N := N + 1;

         exit when X = 0;

      end loop;

      Encoded (Encoded'Last - N) := Types.Byte (N);

      return Encoded (Encoded'Last - N .. Encoded'Last);
   end Left_Encode_NIST;

   -----------------------------------
   --  Left_Encode_NIST_Bit_Length  --
   -----------------------------------

   function Left_Encode_NIST_Bit_Length (Byte_Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array (1 .. Natural'Size + 1) := (others => 0);

      X       : Natural := Byte_Length;
      N       : Natural := 0;

   begin

      Encoded (Encoded'Last - N) := Types.Byte (X mod 256) * 8;

      X := X / 32;
      N := N + 1;

      for I in Positive range 2 .. Natural'Size loop
         pragma Loop_Invariant (N = I - 1);

         exit when X = 0;

         Encoded (Encoded'Last - N) := Types.Byte (X mod 256);

         X := X / 256;
         N := N + 1;

      end loop;

      Encoded (Encoded'Last - N) := Types.Byte (N);

      return Encoded (Encoded'Last - N .. Encoded'Last);
   end Left_Encode_NIST_Bit_Length;

   -------------------------
   --  Right_Encode_NIST  --
   -------------------------

   function Right_Encode_NIST (Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array (1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant (N = I - 1);

         Encoded (Encoded'Last - (N + 1)) := Types.Byte (X mod 256);

         X := X / 256;
         N := N + 1;

         exit when X = 0;

      end loop;

      Encoded (Encoded'Last) := Types.Byte (N);

      return Encoded (Encoded'Last - N .. Encoded'Last);
   end Right_Encode_NIST;

   -----------------------------------
   --  Right_Encode_NIST_Long_Long  --
   -----------------------------------

   function Right_Encode_NIST_Long_Long (Length : in Long_Long_Integer) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array (1 .. (Long_Long_Integer'Size / 8) + 2) := (others => 0);

      X       : Long_Long_Integer := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Long_Long_Integer'Size / 8) + 1 loop
         pragma Loop_Invariant (N = I - 1);

         Encoded (Encoded'Last - (N + 1)) := Types.Byte (X mod 256);

         X := X / 256;
         N := N + 1;

         exit when X = 0;

      end loop;

      Encoded (Encoded'Last) := Types.Byte (N);

      return Encoded (Encoded'Last - N .. Encoded'Last);
   end Right_Encode_NIST_Long_Long;

   ------------------------------------
   --  Right_Encode_NIST_Bit_Length  --
   ------------------------------------

   function Right_Encode_NIST_Bit_Length (Byte_Length : in Natural) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array (1 .. (Natural'Size / 8) + 2) := (others => 0);

      X       : Natural := Byte_Length;
      N       : Natural := 0;

   begin

      Encoded (Encoded'Last - (N + 1)) := Types.Byte (X mod 256) * 8;

      X := X / 32;
      N := N + 1;

      for I in Positive range 2 .. (Natural'Size / 8) + 1 loop
         pragma Loop_Invariant (N = I - 1);

         exit when X = 0;

         Encoded (Encoded'Last - (N + 1)) := Types.Byte (X mod 256);

         X := X / 256;
         N := N + 1;

      end loop;

      Encoded (Encoded'Last) := Types.Byte (N);

      return Encoded (Encoded'Last - N .. Encoded'Last);
   end Right_Encode_NIST_Bit_Length;

   ------------------------
   --  Right_Encode_K12  --
   ------------------------

   function Right_Encode_K12 (Length : in Long_Long_Integer) return Types.Byte_Array
   is
      Encoded : Types.Byte_Array (1 .. (Long_Long_Integer'Size / 8) + 2) := (others => 0);

      X       : Long_Long_Integer := Length;
      N       : Natural := 0;

   begin

      for I in Positive range 1 .. (Long_Long_Integer'Size / 8) + 1 loop
         pragma Loop_Invariant (N = I - 1);

         exit when X = 0;

         Encoded (Encoded'Last - (N + 1)) := Types.Byte (X mod 256);

         X := X / 256;
         N := N + 1;

      end loop;

      Encoded (Encoded'Last) := Types.Byte (N);

      return Encoded (Encoded'Last - N .. Encoded'Last);
   end Right_Encode_K12;

   ---------------
   --  Compare  --
   ---------------

   procedure Compare (A1          : in     Keccak.Types.Byte_Array;
                      A2          : in     Keccak.Types.Byte_Array;
                      Accumulator : in out Keccak.Types.Byte) is
   begin
      for I in 0 .. A1'Length - 1 loop
         Accumulator := Accumulator or (A1 (A1'First + I) xor A2 (A2'First + I));
      end loop;
   end Compare;

end Keccak.Util;
