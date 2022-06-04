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

package body Keccak.Generic_Parallel_Sponge
is

   --------------
   --  Lemmas  --
   --------------

   pragma Warnings (Off, "postcondition does not check the outcome");

   procedure Lemma_Remaining_Mod_Rate_Preserve
     (Offset, Remaining, Length : in Natural;
      Rate                      : in Positive)
     with Global => null,
     Ghost,
     Pre => (Offset <= Length
             and then Remaining <= Length
             and then Length - Remaining = Offset
             and then Remaining < Rate
             and then Offset mod Rate = 0),
     Post => ((Remaining mod Rate = 0) = (Length mod Rate = 0));

   procedure Lemma_Offset_Mod_Rate_Preserve
     (Offset : in Natural;
      Rate   : in Positive)
     with Global => null,
     Ghost,
     Pre => (Offset <= Natural'Last - Rate
             and Offset mod Rate = 0),
     Post => (Offset + Rate) mod Rate = 0;

   pragma Warnings (On, "postcondition does not check the outcome");

   procedure Lemma_Remaining_Mod_Rate_Preserve
     (Offset, Remaining, Length : in Natural;
      Rate                      : in Positive)
   is
   begin
      pragma Assert (Offset + Remaining = Length);
      pragma Assert (Remaining mod Rate = Length mod Rate);
   end Lemma_Remaining_Mod_Rate_Preserve;

   procedure Lemma_Offset_Mod_Rate_Preserve
     (Offset : in Natural;
      Rate   : in Positive)
   is
   begin
      pragma Assert ((Offset + Rate) mod Rate = 0);
   end Lemma_Offset_Mod_Rate_Preserve;

   -------------------
   --  Add_Padding  --
   -------------------

   procedure Add_Padding (Ctx : in out Context)
     with Global => null,
     Pre => State_Of (Ctx) = Absorbing,
     Post => State_Of (Ctx) = Squeezing;

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx : out Context)
   is
   begin
      Ctx.Rate  := (State_Size - Ctx.Capacity) / 8;
      Ctx.State := Absorbing;
      Init (Ctx.Permutation_State);

      pragma Assert (State_Of (Ctx) = Absorbing);
   end Init;

   -----------------------------
   --  Absorb_Bytes_Separate  --
   -----------------------------

   procedure Absorb_Bytes_Separate (Ctx        : in out Context;
                                    Data       : in     Types.Byte_Array)
   is
      Block_Size : constant Natural := Data'Length / Num_Parallel_Instances;

      Rate_Bytes : constant Rate_Bytes_Number := Ctx.Rate;

      Buffer_Size : constant Natural := Rate_Bytes * Num_Parallel_Instances;

      Remaining  : Natural := Block_Size;
      Offset     : Natural := 0;
      Pos        : Types.Index_Number;
      Buf_First  : Types.Index_Number;
      Buf_Last   : Types.Index_Number;

      Buffer     : Types.Byte_Array (0 .. Buffer_Size - 1) := (others => 0);

   begin
      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Offset + Remaining = Block_Size);

         pragma Loop_Invariant (Offset mod Ctx.Rate = 0);

         pragma Loop_Invariant (Offset <= Block_Size);

         XOR_Bits_Into_State_Separate
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

         Lemma_Offset_Mod_Rate_Preserve (Offset, Ctx.Rate);

         Offset    := Offset    + Ctx.Rate;
         Remaining := Remaining - Ctx.Rate;
      end loop;

      pragma Assert (Remaining < Ctx.Rate);
      pragma Assert (Offset mod Ctx.Rate = 0);
      pragma Assert (Offset + Remaining = Block_Size);

      Lemma_Remaining_Mod_Rate_Preserve (Offset, Remaining, Block_Size, Ctx.Rate);

      if Remaining > 0 then
         pragma Assert (Remaining mod Ctx.Rate /= 0);
         pragma Assert (Block_Size mod Ctx.Rate /= 0);

         Ctx.State := Squeezing;

         --  Apply the padding rule to the final chunk of data.
         for I in 0 .. Num_Parallel_Instances - 1 loop
            Pos := Data'First + (I * Block_Size) + Offset;

            Buf_First := (Ctx.Rate * I);
            Buf_Last  := (Ctx.Rate * I) + (Remaining - 1);

            Buffer (Buf_First .. Buf_Last) := Data (Pos .. Pos + (Remaining - 1));

            pragma Assert (Buf_First + (Ctx.Rate - 1) in Buffer'Range);

            Pad (Block          => Buffer (Buf_First .. Buf_First + (Ctx.Rate - 1)),
                 Num_Used_Bits  => Remaining * 8,
                 Max_Bit_Length => Ctx.Rate * 8);
         end loop;

         XOR_Bits_Into_State_Separate
           (S           => Ctx.Permutation_State,
            Data        => Buffer,
            Data_Offset => 0,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

      else
         --  Help prove contract case.
         pragma Assert ((Data'Length / Num_Parallel_Instances) mod (Rate_Of (Ctx) / 8) = 0);
      end if;

   end Absorb_Bytes_Separate;

   ------------------------
   --  Absorb_Bytes_All  --
   ------------------------

   procedure Absorb_Bytes_All (Ctx        : in out Context;
                               Data       : in     Types.Byte_Array)
   is
      Rate_Bytes : constant Positive := Ctx.Rate;

      Offset    : Natural := 0;
      Remaining : Natural := Data'Length;

      Pos       : Types.Index_Number;

      Buffer    : Types.Byte_Array (0 .. Rate_Bytes - 1);

   begin

      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Offset + Remaining = Data'Length);
         pragma Loop_Invariant (Offset mod Ctx.Rate = 0);
         pragma Loop_Invariant (Offset <= Data'Length);

         Pos := Data'First + Offset;

         XOR_Bits_Into_State_All
           (S       => Ctx.Permutation_State,
            Data    => Data (Pos .. Pos + Ctx.Rate - 1),
            Bit_Len => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

         Lemma_Offset_Mod_Rate_Preserve (Offset, Ctx.Rate);

         Offset    := Offset    + Ctx.Rate;
         Remaining := Remaining - Ctx.Rate;
      end loop;

      pragma Assert (Remaining < Ctx.Rate);
      pragma Assert (Offset mod Ctx.Rate = 0);
      pragma Assert (Offset + Remaining = Data'Length);

      if Remaining > 0 then
         pragma Assert (Remaining mod Ctx.Rate /= 0);
         pragma Assert (Data'Length mod Ctx.Rate /= 0);

         Ctx.State := Squeezing;

         Buffer := (others => 0);
         Buffer (0 .. Remaining - 1) := Data (Data'First + Offset .. Data'Last);

         Pad (Block          => Buffer,
              Num_Used_Bits  => Remaining * 8,
              Max_Bit_Length => Ctx.Rate * 8);

         XOR_Bits_Into_State_All
           (S       => Ctx.Permutation_State,
            Data    => Buffer,
            Bit_Len => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

      end if;

   end Absorb_Bytes_All;

   ------------------------------------
   --  Absorb_Bytes_All_With_Suffix  --
   ------------------------------------

   procedure Absorb_Bytes_All_With_Suffix
     (Ctx        : in out Context;
      Data       : in     Types.Byte_Array;
      Suffix     : in     Types.Byte;
      Suffix_Len : in     Natural)
   is
      Rate_Bytes      : constant Positive := Ctx.Rate;

      Offset          : Natural;
      Remaining       : Natural;
      Num_Full_Blocks : Natural;
      Length          : Natural;

      Buffer          : Types.Byte_Array (0 .. Rate_Bytes - 1) := (others => 0);

   begin
      Num_Full_Blocks := Data'Length / Ctx.Rate;

      --  Process full blocks of data, if available.
      if Num_Full_Blocks > 0 then
         Length := Num_Full_Blocks * Ctx.Rate;

         Absorb_Bytes_All
           (Ctx  => Ctx,
            Data => Data (Data'First .. Data'First + Length - 1));

         Offset    := Length;
         Remaining := Data'Length - Length;

      else
         Offset    := 0;
         Remaining := Data'Length;

      end if;

      Ctx.State := Squeezing;

      if Remaining > 0 then
         --  Append suffix + padding to remaining bytes

         Buffer (0 .. Remaining - 1) := Data (Data'First + Offset .. Data'Last);
         Buffer (Remaining) := Suffix;

         Pad (Block          => Buffer,
              Num_Used_Bits  => (Remaining * 8) + Suffix_Len,
              Max_Bit_Length => Ctx.Rate * 8);

      else
         --  No remaining data, just process suffix + padding
         Buffer (0) := Suffix;

         Pad (Block          => Buffer,
              Num_Used_Bits  => Suffix_Len,
              Max_Bit_Length => Ctx.Rate * 8);
      end if;

      XOR_Bits_Into_State_All
        (S       => Ctx.Permutation_State,
         Data    => Buffer,
         Bit_Len => Ctx.Rate * 8);

      Permute_All (Ctx.Permutation_State);

   end Absorb_Bytes_All_With_Suffix;

   -----------------------------------------
   --  Absorb_Bytes_Separate_With_Suffix  --
   -----------------------------------------

   procedure Absorb_Bytes_Separate_With_Suffix
     (Ctx        : in out Context;
      Data       : in     Types.Byte_Array;
      Suffix     : in     Types.Byte;
      Suffix_Len : in     Natural)
   is
      Block_Size : constant Natural := Data'Length / Num_Parallel_Instances;

      Rate_Bytes : constant Rate_Bytes_Number := Ctx.Rate;

      Buffer_Size : constant Natural := Rate_Bytes * Num_Parallel_Instances;

      Remaining  : Natural := Block_Size;
      Offset     : Natural := 0;
      Pos        : Types.Index_Number;
      Buf_First  : Types.Index_Number;
      Buf_Last   : Types.Index_Number;

      Buffer     : Types.Byte_Array (0 .. Buffer_Size - 1) := (others => 0);

   begin
      Ctx.State := Squeezing;

      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Offset + Remaining = Block_Size);

         pragma Loop_Invariant (Offset mod Ctx.Rate = 0);

         pragma Loop_Invariant (Offset <= Block_Size);

         XOR_Bits_Into_State_Separate
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Bit_Len     => Ctx.Rate * 8);

         Permute_All (Ctx.Permutation_State);

         Lemma_Offset_Mod_Rate_Preserve (Offset, Ctx.Rate);

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

            Pad (Block          => Buffer (Buf_First .. Buf_First + (Ctx.Rate - 1)),
                 Num_Used_Bits  => (Remaining * 8) + Suffix_Len,
                 Max_Bit_Length => Ctx.Rate * 8);
         end loop;

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

      end if;

      XOR_Bits_Into_State_Separate
        (S           => Ctx.Permutation_State,
         Data        => Buffer,
         Data_Offset => 0,
         Bit_Len     => Ctx.Rate * 8);

      Permute_All (Ctx.Permutation_State);

   end Absorb_Bytes_Separate_With_Suffix;

   -------------------
   --  Add_Padding  --
   -------------------

   procedure Add_Padding (Ctx : in out Context)
   is
      Rate_Bytes : constant Rate_Bytes_Number := Ctx.Rate;

      Buffer     : Types.Byte_Array (0 .. (Rate_Bytes * Num_Parallel_Instances) - 1);
   begin
      Buffer := (others => 0);

      Pad (Block          => Buffer (0 .. Ctx.Rate - 1),
           Num_Used_Bits  => 0,
           Max_Bit_Length => Ctx.Rate * 8);

         --  Replicate the padding for each parallel instance.
      for I in 1 .. Num_Parallel_Instances - 1 loop
         Buffer (I * Ctx.Rate .. I * Ctx.Rate + Ctx.Rate - 1) :=
           Buffer (0 .. Ctx.Rate - 1);
      end loop;

      XOR_Bits_Into_State_Separate
        (S           => Ctx.Permutation_State,
         Data        => Buffer,
         Data_Offset => 0,
         Bit_Len     => Ctx.Rate * 8);

      Permute_All (Ctx.Permutation_State);

      Ctx.State := Squeezing;

      pragma Assert (State_Of (Ctx) = Squeezing);
   end Add_Padding;

   ------------------------------
   --  Squeeze_Bytes_Separate  --
   ------------------------------

   procedure Squeeze_Bytes_Separate (Ctx        : in out Context;
                                     Data       :    out Types.Byte_Array)
   is
      Block_Size : constant Natural := Data'Length / Num_Parallel_Instances;

      Remaining  : Natural := Block_Size;
      Offset     : Natural := 0;

      Capacity_Old : constant Positive := Ctx.Capacity with Ghost;

   begin
      --  If we're coming straight from the absorbing phase then we need to
      --  apply the padding rule before proceeding to the squeezing phase,
      if Ctx.State = Absorbing then
         Add_Padding (Ctx);
      end if;

      while Remaining >= Ctx.Rate loop
         pragma Loop_Invariant (Ctx.Capacity = Capacity_Old);

         pragma Loop_Invariant (Offset + Remaining = Block_Size);

         pragma Loop_Invariant (Offset mod Ctx.Rate = 0);

         pragma Loop_Invariant (Offset <= Block_Size);

         Extract_Bytes
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Byte_Len    => Ctx.Rate);

         pragma Annotate
           (GNATprove, False_Positive,
            "initialization check might fail",
            "'Data' is wholly initialized by the end of this procedure");

         Permute_All (Ctx.Permutation_State);

         Lemma_Offset_Mod_Rate_Preserve (Offset, Ctx.Rate);

         Remaining := Remaining - Ctx.Rate;
         Offset    := Offset    + Ctx.Rate;
      end loop;

      pragma Assert (Remaining < Ctx.Rate);
      pragma Assert (Offset mod Ctx.Rate = 0);
      pragma Assert (Offset + Remaining = Block_Size);

      Lemma_Remaining_Mod_Rate_Preserve (Offset, Remaining, Block_Size, Ctx.Rate);

      if Remaining > 0 then
         pragma Assert (Remaining mod Ctx.Rate /= 0);
         pragma Assert (Block_Size mod Ctx.Rate /= 0);

         Ctx.State := Finished;

         Extract_Bytes
           (S           => Ctx.Permutation_State,
            Data        => Data,
            Data_Offset => Offset,
            Byte_Len    => Remaining);

         pragma Annotate
           (GNATprove, False_Positive,
            "initialization check might fail",
            "'Data' is wholly initialized by the end of this procedure");

      end if;
   end Squeeze_Bytes_Separate;

end Keccak.Generic_Parallel_Sponge;
