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

package body Keccak.Generic_Parallel_Sponge
is

   function State_Index_Offset_From_First (I : in Natural) return State_Index
   is (State_Index (Integer (State_Index'First) + I))
   with Inline,
   Pre => I in 0 .. Num_Parallel_Instances - 1;


   procedure Init (Ctx      :    out Context;
                   Capacity : in     Positive)
   is
   begin
      Init (Ctx.Permutation_State);
      Ctx.Rate  := (State_Size - Capacity) / 8;
      Ctx.State := Absorbing;
   end Init;


   procedure Absorb_Bytes_All (Ctx        : in out Context;
                               Data       : in     Types.Byte_Array)
   is
      Block_Size : constant Natural := Data'Length / Num_Parallel_Instances;

      Rate_Bytes : constant Rate_Bytes_Number := Ctx.Rate;

      Remaining  : Natural := Block_Size;
      Offset     : Natural := 0;
      Pos        : Types.Index_Number;
      Buf_First  : Types.Index_Number;
      Buf_Last   : Types.Index_Number;

      Buffer     : Types.Byte_Array (0 .. (Rate_Bytes * Num_Parallel_Instances) - 1) := (others => 0);

   begin
      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Offset + Remaining = Block_Size);

         pragma Loop_Invariant (Offset mod Block_Size = 0);

         Pos := Data'First + Offset;

         XOR_Bits_Into_State
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

         Remaining := Remaining - Ctx.Rate;
         Offset    := Offset    + Ctx.Rate;
      end loop;


      if Remaining > 0 then
         Ctx.State := Squeezing;

         --  Apply the padding rule to the final chunk of data.
         for I in 0 .. Num_Parallel_Instances - 1 loop
            Pos := Data'First + (I * Block_Size) + Offset;

            Buf_First := (Ctx.Rate * I);
            Buf_Last  := (Ctx.Rate * I) + (Remaining - 1);

            Buffer (Buf_First .. Buf_Last) := Data (Pos .. Pos + (Remaining - 1));

            Pad (Block          => Buffer (Buf_First .. Buf_First + Ctx.Rate - 1),
                 Num_Used_Bits  => Remaining * 8,
                 Max_Bit_Length => Ctx.Rate * 8);
         end loop;

         XOR_Bits_Into_State
           (S           => Ctx.Permutation_State,
            Data        => Buffer,
            Data_Offset => 0,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);
      end if;

   end Absorb_Bytes_All;


   procedure Absorb_Bytes_All_With_Suffix
     (Ctx        : in out Context;
      Data       : in     Types.Byte_Array;
      Suffix     : in     Types.Byte;
      Suffix_Len : in     Natural)
   is
      Block_Size : constant Natural := Data'Length / Num_Parallel_Instances;

      Rate_Bytes : constant Rate_Bytes_Number := Ctx.Rate;

      Remaining  : Natural := Block_Size;
      Offset     : Natural := 0;
      Pos        : Types.Index_Number;
      Buf_First  : Types.Index_Number;
      Buf_Last   : Types.Index_Number;

      Buffer     : Types.Byte_Array (0 .. (Rate_Bytes * Num_Parallel_Instances) - 1) := (others => 0);

   begin
      Ctx.State := Squeezing;

      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Offset + Remaining = Block_Size);

         pragma Loop_Invariant (Offset mod Ctx.Rate = 0);

         Pos := Data'First + Offset;

         XOR_Bits_Into_State
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

         Remaining := Remaining - Ctx.Rate;
         Offset    := Offset    + Ctx.Rate;
      end loop;

      if Remaining > 0 then
         --  Apply the padding rule to the final chunk of data + suffix.
         for I in 0 .. Num_Parallel_Instances - 1 loop
            Pos := Data'First + (I * Block_Size) + Offset;

            Buf_First := (Ctx.Rate * I);
            Buf_Last  := (Ctx.Rate * I) + (Remaining - 1);

            Buffer (Buf_First .. Buf_Last) := Data (Pos .. Pos + (Remaining - 1));
            Buffer (Buf_Last + 1) := Suffix;

            Pad (Block          => Buffer (Buf_First .. Buf_First + Ctx.Rate - 1),
                 Num_Used_Bits  => (Remaining * 8) + Suffix_Len,
                 Max_Bit_Length => Ctx.Rate * 8);
         end loop;

         XOR_Bits_Into_State
           (S           => Ctx.Permutation_State,
            Data        => Buffer,
            Data_Offset => 0,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

      else
         --  Apply the padding rule to the suffix only.
         Buffer := (0      => Suffix,
                    others => 0);

         Pad (Block          => Buffer (0 .. Ctx.Rate - 1),
              Num_Used_Bits  => Suffix_Len,
              Max_Bit_Length => Ctx.Rate * 8);

         --  Replicate the padding for each parallel instance.
         for I in 1 .. Num_Parallel_Instances - 1 loop
            Buffer (I * Ctx.Rate .. I * Ctx.Rate + Ctx.Rate - 1) :=
              Buffer (0 .. Ctx.Rate - 1);
         end loop;

         XOR_Bits_Into_State
           (S           => Ctx.Permutation_State,
            Data        => Buffer,
            Data_Offset => 0,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

      end if;

   end Absorb_Bytes_All_With_Suffix;


   procedure Squeeze_Bytes_All (Ctx        : in out Context;
                                Data       :    out Types.Byte_Array)
   is
      Block_Size : constant Natural := Data'Length / Num_Parallel_Instances;

      Rate_Bytes : constant Rate_Bytes_Number := Ctx.Rate;

      Remaining  : Natural := Block_Size;
      Offset     : Natural := 0;
      Pos        : Types.Index_Number;

      Buffer     : Types.Byte_Array (0 .. Rate_Bytes - 1);

   begin
      --  If we're coming straight from the absorbing phase then we need to
      --  apply the padding rule before proceeding to the squeezing phase,
      if Ctx.State = Absorbing then
         Buffer := (others => 0);

         Pad (Block          => Buffer (0 .. Ctx.Rate - 1),
              Num_Used_Bits  => 0,
              Max_Bit_Length => Ctx.Rate * 8);

         --  Replicate the padding for each parallel instance.
         for I in 1 .. Num_Parallel_Instances - 1 loop
            Buffer (I * Ctx.Rate .. I * Ctx.Rate + Ctx.Rate - 1) :=
              Buffer (0 .. Ctx.Rate - 1);
         end loop;

         XOR_Bits_Into_State
           (S           => Ctx.Permutation_State,
            Data        => Buffer,
            Data_Offset => 0,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);
      end if;


      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Offset + Remaining = Block_Size);

         pragma Loop_Invariant (Offset mod Block_Size = 0);

         Pos := Data'First + Offset;

         Extract_Bytes
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Byte_Len    => Ctx.Rate);

         Permute_All (Ctx.Permutation_State);

         Remaining := Remaining - Ctx.Rate;
         Offset    := Offset    + Ctx.Rate;
      end loop;


      if Remaining > 0 then
         Ctx.State := Finished;

         Pos := Data'First + Offset;

         Extract_Bytes
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Byte_Len    => Remaining);

      end if;
   end Squeeze_Bytes_All;

end Keccak.Generic_Parallel_Sponge;
