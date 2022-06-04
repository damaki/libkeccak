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
with Keccak.Util;

package body Keccak.Generic_Parallel_Hash
is

   ---------------------------------------
   --  Generic_Process_Parallel_Blocks  --
   ---------------------------------------

   generic
      with package SHAKE_Parallel_N is new Keccak.Generic_Parallel_XOF (<>);
   procedure Generic_Process_Parallel_Blocks
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
     with Pre => (Data'Length = Ctx.Block_Size * SHAKE_Parallel_N.Num_Parallel_Instances
                  and State_Of (Ctx) = Updating
                  and Ctx.Partial_Block_Length = 0),
     Post => (State_Of (Ctx) = Updating
              and Ctx.Input_Len = Ctx'Old.Input_Len
              and Ctx.Partial_Block_Length = Ctx'Old.Partial_Block_Length
              and Ctx.Block_Size = Ctx'Old.Block_Size);
   --  Generic procedure to process N blocks in parallel.
   --
   --  This procedure process N blocks in parallel to produce N chaining values
   --  (CV). The CVs are then processed in the outer (serial) cSHAKE.
   --
   --  @param Ctx The KangarooTwelve context.
   --
   --  @param Data Byte array containing N blocks.

   ---------------------------------------
   --  Generic_Process_Parallel_Blocks  --
   ---------------------------------------

   procedure Generic_Process_Parallel_Blocks
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
   is
      N : constant Positive := SHAKE_Parallel_N.Num_Parallel_Instances;

      Parallel_Ctx : SHAKE_Parallel_N.Context;

      CV_N : Types.Byte_Array (1 .. CV_Size_Bytes * N);

   begin
      --  Process N blocks in parallel and produce N chaining values.
      SHAKE_Parallel_N.Init (Parallel_Ctx);
      SHAKE_Parallel_N.Update_Separate  (Parallel_Ctx, Data);
      SHAKE_Parallel_N.Extract_Separate (Parallel_Ctx, CV_N);

      pragma Unused (Parallel_Ctx);

      --  Process the chaining values with the outer CSHAKE.
      CSHAKE_Serial.Update
        (Ctx        => Ctx.Outer_CSHAKE,
         Message    => CV_N);
   end Generic_Process_Parallel_Blocks;

   ------------------------------
   --  Generic Instantiations  --
   ------------------------------

   procedure Process_8_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (SHAKE_Parallel_8);

   procedure Process_4_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (SHAKE_Parallel_4);

   procedure Process_2_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (SHAKE_Parallel_2);

   -----------------------
   --  Process_1_Block  --
   -----------------------

   procedure Process_1_Block
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
     with Pre => (Data'Length <= Ctx.Block_Size
                  and State_Of (Ctx) = Updating
                  and Ctx.Partial_Block_Length = 0),
     Post => (State_Of (Ctx) = Updating
              and Ctx.Input_Len = Ctx'Old.Input_Len
              and Ctx.Partial_Block_Length = Ctx'Old.Partial_Block_Length
              and Ctx.Block_Size = Ctx'Old.Block_Size);
   --  Processes a single block using a serial CSHAKE.

   -----------------------
   --  Process_1_Block  --
   -----------------------

   procedure Process_1_Block
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
   is
      Serial_Ctx : SHAKE_Serial.Context;

      CV : Types.Byte_Array (1 .. CV_Size_Bytes);

   begin
      --  Process N blocks in parallel and produce N changing values.
      SHAKE_Serial.Init   (Serial_Ctx);
      SHAKE_Serial.Update (Serial_Ctx, Data);
      SHAKE_Serial.Extract (Serial_Ctx, CV);

      pragma Unreferenced (Serial_Ctx);

      --  Process the chaining values with the outer CSHAKE.
      CSHAKE_Serial.Update
        (Ctx        => Ctx.Outer_CSHAKE,
         Message    => CV);
   end Process_1_Block;

   ----------------------------
   --  Add_To_Partial_Block  --
   ----------------------------

   procedure Add_To_Partial_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
     with Pre => ((if Ctx.Partial_Block_Length > 0
                   then Max_Input_Length (Ctx) >= Data'Length)
                  and State_Of (Ctx) = Updating),
     Post => (Added <= Ctx.Block_Size
              and Added <= Data'Length
              and (if Added < Data'Length then Ctx.Partial_Block_Length = 0)
              and State_Of (Ctx) = Updating
              and Num_Bytes_Processed (Ctx) = Num_Bytes_Processed (Ctx'Old) + Byte_Count (Added)
              and Ctx.Block_Size = Ctx'Old.Block_Size),
     Contract_Cases =>
       (Ctx.Partial_Block_Length = 0 and Data'Length = 0 =>
          Added = 0 and Ctx.Partial_Block_Length = 0,

        Ctx.Partial_Block_Length = 0 and Data'Length > 0 =>
          Added = 0 and Ctx.Partial_Block_Length = 0,

        Ctx.Partial_Block_Length > 0 and Data'Length = 0 =>
          Added = 0 and Ctx.Partial_Block_Length = Ctx'Old.Partial_Block_Length,

        Ctx.Partial_Block_Length > 0 and Data'Length > 0 =>
          Added > 0);

   ----------------------------
   --  Add_To_Partial_Block  --
   ----------------------------

   procedure Add_To_Partial_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
   is
      Free_In_Block : Natural;
      CV            : Types.Byte_Array (1 .. CV_Size_Bytes);

   begin
      if Ctx.Partial_Block_Length > 0 then
         Free_In_Block := Ctx.Block_Size - Ctx.Partial_Block_Length;

         if Data'Length >= Free_In_Block then
            SHAKE_Serial.Update
              (Ctx     => Ctx.Partial_Block_CSHAKE,
               Message => Data (Data'First .. Data'First + (Free_In_Block - 1)));

            pragma Warnings
              (GNATprove, Off,
               """Ctx.Partial_Block_CSHAKE"" is set by ""Extract"" but not used after the call",
               Reason => "No further data needs to be extracted before Init");

            SHAKE_Serial.Extract
              (Ctx    => Ctx.Partial_Block_CSHAKE,
               Digest => CV);

            pragma Warnings (GNATprove, On);

            CSHAKE_Serial.Update
              (Ctx     => Ctx.Outer_CSHAKE,
               Message => CV);

            SHAKE_Serial.Init (Ctx.Partial_Block_CSHAKE);

            Ctx.Partial_Block_Length := 0;
            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Free_In_Block);

            Added := Free_In_Block;

         else
            SHAKE_Serial.Update
              (Ctx     => Ctx.Partial_Block_CSHAKE,
               Message => Data);

            Ctx.Partial_Block_Length := Ctx.Partial_Block_Length + Data'Length;
            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Data'Length);

            Added := Data'Length;

         end if;

      else
         Added := 0;

      end if;
   end Add_To_Partial_Block;

   ----------------------------------
   --  Process_Last_Partial_Block  --
   ----------------------------------

   procedure Process_Last_Partial_Block (Ctx : in out Context)
     with Global => null,
     Pre => State_Of (Ctx) = Updating,
     Post => (State_Of (Ctx) = Updating
              and Ctx.Partial_Block_Length = 0);

   ----------------------------------
   --  Process_Last_Partial_Block  --
   ----------------------------------

   procedure Process_Last_Partial_Block (Ctx : in out Context)
   is
      CV : Types.Byte_Array (0 .. CV_Size_Bytes - 1);

   begin
      if Ctx.Partial_Block_Length > 0 then
         SHAKE_Serial.Extract (Ctx.Partial_Block_CSHAKE, CV);
         CSHAKE_Serial.Update  (Ctx.Outer_CSHAKE,         CV);
         SHAKE_Serial.Init    (Ctx.Partial_Block_CSHAKE);

         Ctx.Partial_Block_Length := 0;
      end if;
   end Process_Last_Partial_Block;

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx           :    out Context;
                   Block_Size    : in     Block_Size_Number;
                   Customization : in     String)
   is
   begin
      Ctx.Partial_Block_Length := 0;
      Ctx.Block_Size           := Block_Size;
      Ctx.Input_Len            := 0;
      Ctx.Finished             := False;

      CSHAKE_Serial.Init (Ctx           => Ctx.Outer_CSHAKE,
                          Customization => Customization,
                          Function_Name => "ParallelHash");

      SHAKE_Serial.Init (Ctx.Partial_Block_CSHAKE);

      CSHAKE_Serial.Update
        (Ctx     => Ctx.Outer_CSHAKE,
         Message => Util.Left_Encode_NIST (Block_Size));

   end Init;

   --------------
   --  Update  --
   --------------

   procedure Update (Ctx  : in out Context;
                     Data : in     Types.Byte_Array)
   is
      Initial_Bytes_Processed : constant Byte_Count := Num_Bytes_Processed (Ctx);
      Initial_Block_Size      : constant Positive   := Ctx.Block_Size;

      Remaining               : Natural;
      Offset                  : Natural;
      Pos                     : Types.Index_Number;

   begin
      Add_To_Partial_Block (Ctx, Data, Offset);

      Remaining := Data'Length - Offset;

      if Remaining > 0 then
         pragma Assert (Ctx.Partial_Block_Length = 0);
         pragma Assert (Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset));
         pragma Assert (Byte_Count (Remaining) <= Max_Input_Length (Ctx));

         --  Process blocks of 8 in parallel
         while Remaining >= Ctx.Block_Size * 8 loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (State_Of (Ctx) = Updating);
            pragma Loop_Invariant (Ctx.Input_Len = Initial_Bytes_Processed + Byte_Count (Offset));
            pragma Loop_Invariant (Byte_Count (Remaining) <= Byte_Count'Last - Ctx.Input_Len);
            pragma Loop_Invariant (Ctx.Partial_Block_Length = 0);
            pragma Loop_Invariant (Ctx.Block_Size = Initial_Block_Size);

            Pos := Data'First + Offset;

            Process_8_Parallel_Blocks
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + ((Ctx.Block_Size * 8) - 1)));

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Ctx.Block_Size * 8);

            Offset    := Offset    + (Ctx.Block_Size * 8);
            Remaining := Remaining - (Ctx.Block_Size * 8);
         end loop;

         pragma Assert_And_Cut
           (Offset + Remaining = Data'Length
            and State_Of (Ctx) = Updating
            and Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset)
            and Byte_Count (Remaining) <= Max_Input_Length (Ctx)
            and Ctx.Partial_Block_Length = 0
            and Ctx.Block_Size = Initial_Block_Size
            and Remaining < Ctx.Block_Size * 8);

      --  Process blocks of 4 in parallel
         while Remaining >= Ctx.Block_Size * 4 loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (State_Of (Ctx) = Updating);
            pragma Loop_Invariant (Ctx.Input_Len = Initial_Bytes_Processed + Byte_Count (Offset));
            pragma Loop_Invariant (Byte_Count (Remaining) <= Byte_Count'Last - Ctx.Input_Len);
            pragma Loop_Invariant (Ctx.Partial_Block_Length = 0);
            pragma Loop_Invariant (Ctx.Block_Size = Initial_Block_Size);

            Pos := Data'First + Offset;

            Process_4_Parallel_Blocks
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + ((Ctx.Block_Size * 4) - 1)));

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Ctx.Block_Size * 4);

            Offset    := Offset    + (Ctx.Block_Size * 4);
            Remaining := Remaining - (Ctx.Block_Size * 4);
         end loop;

         pragma Assert_And_Cut
           (Offset + Remaining = Data'Length
            and State_Of (Ctx) = Updating
            and Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset)
            and Byte_Count (Remaining) <= Max_Input_Length (Ctx)
            and Ctx.Partial_Block_Length = 0
            and Ctx.Block_Size = Initial_Block_Size
            and Remaining < Ctx.Block_Size * 4);

      --  Process blocks of 2 in parallel
         while Remaining >= Ctx.Block_Size * 2 loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (State_Of (Ctx) = Updating);
            pragma Loop_Invariant (Ctx.Input_Len = Initial_Bytes_Processed + Byte_Count (Offset));
            pragma Loop_Invariant (Byte_Count (Remaining) <= Byte_Count'Last - Ctx.Input_Len);
            pragma Loop_Invariant (Ctx.Partial_Block_Length = 0);
            pragma Loop_Invariant (Ctx.Block_Size = Initial_Block_Size);

            Pos := Data'First + Offset;

            Process_2_Parallel_Blocks
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + ((Ctx.Block_Size * 2) - 1)));

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Ctx.Block_Size * 2);

            Offset    := Offset    + (Ctx.Block_Size * 2);
            Remaining := Remaining - (Ctx.Block_Size * 2);
         end loop;

         pragma Assert_And_Cut
           (Offset + Remaining = Data'Length
            and State_Of (Ctx) = Updating
            and Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset)
            and Byte_Count (Remaining) <= Max_Input_Length (Ctx)
            and Ctx.Partial_Block_Length = 0
            and Ctx.Block_Size = Initial_Block_Size
            and Remaining < Ctx.Block_Size * 2);

      --  Process single blocks
         if Remaining >= Ctx.Block_Size then
            Pos := Data'First + Offset;

            Process_1_Block
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + (Ctx.Block_Size - 1)));

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Ctx.Block_Size);

            Offset    := Offset    + Ctx.Block_Size;
            Remaining := Remaining - Ctx.Block_Size;
         end if;

         pragma Assert (Offset + Remaining = Data'Length);
         pragma Assert (State_Of (Ctx) = Updating);
         pragma Assert (Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset));
         pragma Assert (Remaining < Ctx.Block_Size);

      --  Process remaining data.
         if Remaining > 0 then
            SHAKE_Serial.Update
              (Ctx     => Ctx.Partial_Block_CSHAKE,
               Message => Data (Data'First + Offset .. Data'Last));

            Ctx.Partial_Block_Length := Remaining;

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Remaining);
         end if;
      end if;
   end Update;

   --------------
   --  Finish  --
   --------------

   procedure Finish (Ctx  : in out Context;
                     Data :    out Types.Byte_Array)
   is
      Nb_Blocks : Long_Long_Integer;

   begin
      Nb_Blocks := Long_Long_Integer (Ctx.Input_Len) / Long_Long_Integer (Ctx.Block_Size);
      if Ctx.Partial_Block_Length > 0 then
         pragma Assert (Ctx.Block_Size > 1);
         pragma Assert (Nb_Blocks < Long_Long_Integer'Last);

         Nb_Blocks := Nb_Blocks + 1;
      end if;

      Process_Last_Partial_Block (Ctx);

      Ctx.Finished := True;

      CSHAKE_Serial.Update (Ctx     => Ctx.Outer_CSHAKE,
                            Message => Util.Right_Encode_NIST_Long_Long (Nb_Blocks));

      CSHAKE_Serial.Update (Ctx     => Ctx.Outer_CSHAKE,
                            Message => Util.Right_Encode_NIST_Bit_Length (Data'Length));

      CSHAKE_Serial.Extract (Ctx    => Ctx.Outer_CSHAKE,
                             Digest => Data);
   end Finish;

   ---------------
   --  Extract  --
   ---------------

   procedure Extract (Ctx  : in out Context;
                      Data :    out Types.Byte_Array)
   is
      Nb_Blocks : Long_Long_Integer;

   begin
      if State_Of (Ctx) = Updating then

         Nb_Blocks := Long_Long_Integer (Ctx.Input_Len) / Long_Long_Integer (Ctx.Block_Size);

         if Ctx.Partial_Block_Length > 0 then
            pragma Assert (Ctx.Block_Size > 1);
            pragma Assert (Nb_Blocks < Long_Long_Integer'Last);

            Nb_Blocks := Nb_Blocks + 1;
         end if;

         Process_Last_Partial_Block (Ctx);

         CSHAKE_Serial.Update (Ctx     => Ctx.Outer_CSHAKE,
                               Message => Util.Right_Encode_NIST_Long_Long (Nb_Blocks));

         CSHAKE_Serial.Update (Ctx     => Ctx.Outer_CSHAKE,
                               Message => Util.Right_Encode_NIST (0));
      end if;

      CSHAKE_Serial.Extract (Ctx    => Ctx.Outer_CSHAKE,
                             Digest => Data);
   end Extract;

end Keccak.Generic_Parallel_Hash;
