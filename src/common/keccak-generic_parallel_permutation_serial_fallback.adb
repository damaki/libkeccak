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
package body Keccak.Generic_Parallel_Permutation_Serial_Fallback
is

   ------------
   --  Init  --
   ------------

   procedure Init (S : out Parallel_State)
   is
   begin
      for I in S.States'Range loop
         Init (S.States (I));

         pragma Annotate (GNATprove, False_Positive,
                          """S.States"" might not be initialized",
                          "S.States is initialized after full loop");
      end loop;
   end Init;

   -------------------
   --  Permute_All  --
   -------------------

   procedure Permute_All (S : in out Parallel_State)
   is
   begin
      for I in S.States'Range loop
         Permute (S.States (I));
      end loop;
   end Permute_All;

   ------------------------------------
   --  XOR_Bits_Into_State_Separate  --
   ------------------------------------

   procedure XOR_Bits_Into_State_Separate
     (S           : in out Parallel_State;
      Data        : in     Types.Byte_Array;
      Data_Offset : in     Natural;
      Bit_Len     : in     Natural)
   is
      Stride : constant Natural := Data'Length / Num_Parallel_Instances;

      Pos : Types.Index_Number;

   begin
      if Bit_Len > 0 then
         pragma Assert ((Data'Length / Num_Parallel_Instances) - Data_Offset > 0);

         for I in 0 .. Num_Parallel_Instances - 1 loop
            Pos := Data'First + (Stride * I) + Data_Offset;

            XOR_Bits_Into_State
              (S       => S.States (I),
               Data    => Data (Pos .. Pos + ((Bit_Len + 7) / 8) - 1),
               Bit_Len => Bit_Len);
         end loop;
      end if;
   end XOR_Bits_Into_State_Separate;

   -------------------------------
   --  XOR_Bits_Into_State_All  --
   -------------------------------

   procedure XOR_Bits_Into_State_All
     (S           : in out Parallel_State;
      Data        : in     Types.Byte_Array;
      Bit_Len     : in     Natural)
   is
   begin
      for I in S.States'Range loop
         XOR_Bits_Into_State
           (S       => S.States (I),
            Data    => Data,
            Bit_Len => Bit_Len);
      end loop;
   end XOR_Bits_Into_State_All;

   ---------------------
   --  Extract_Bytes  --
   ---------------------

   procedure Extract_Bytes (S           : in     Parallel_State;
                            Data        : in out Types.Byte_Array;
                            Data_Offset : in     Natural;
                            Byte_Len    : in     Natural)
   is
      Stride : constant Natural := Data'Length / Num_Parallel_Instances;

      Pos : Types.Index_Number;

   begin
      if Byte_Len > 0 then
         pragma Assert ((Data'Length / Num_Parallel_Instances) - Data_Offset > 0);

         for I in 0 .. Num_Parallel_Instances - 1 loop
            Pos := Data'First + (Stride * I) + Data_Offset;

            Extract_Bytes
              (A    => S.States (I),
               Data => Data (Pos .. Pos + Byte_Len - 1));
         end loop;
      end if;
   end Extract_Bytes;

end Keccak.Generic_Parallel_Permutation_Serial_Fallback;
