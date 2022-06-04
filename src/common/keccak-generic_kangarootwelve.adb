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

package body Keccak.Generic_KangarooTwelve
is

   --------------------------
   --  Suffix definitions  --
   --------------------------

   Suffix_110_62  : constant Types.Byte_Array (1 .. 8) := (1 => 2#011#, others => 0);
   Suffix_110     : constant Types.Byte_Array (1 .. 1) := (1 => 2#011#);
   Suffix_11      : constant Types.Byte_Array (1 .. 1) := (1 => 2#11#);
   Suffix_FFFF_01 : constant Types.Byte_Array (1 .. 3) := (16#FF#, 16#FF#, 2#10#);

   ---------------------------------------
   --  Generic_Process_Parallel_Blocks  --
   ---------------------------------------

   generic
      with package XOF_Parallel_N is new Keccak.Generic_Parallel_XOF (<>);
   procedure Generic_Process_Parallel_Blocks
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
     with Pre => (Data'Length = Block_Size_Bytes * XOF_Parallel_N.Num_Parallel_Instances
                  and State_Of (Ctx) = Updating
                  and Ctx.Partial_Block_Length = 0),
     Post => (State_Of (Ctx) = Updating
              and Ctx.Input_Len = Ctx'Old.Input_Len
              and Ctx.Partial_Block_Length = Ctx'Old.Partial_Block_Length);
   --  Generic procedure to process N blocks in parallel.
   --
   --  This procedure process N blocks in parallel to produce N chaining values
   --  (CV). The CVs are then processed in the outer (serial) XOF.
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
      N : constant Positive := XOF_Parallel_N.Num_Parallel_Instances;

      Par_Ctx : XOF_Parallel_N.Context;

      CV_N : Types.Byte_Array (1 .. CV_Size_Bytes * N);

   begin
      --  Process N blocks in parallel and produce N chaining values.
      XOF_Parallel_N.Init    (Par_Ctx);
      XOF_Parallel_N.Update_Separate  (Par_Ctx, Data);
      XOF_Parallel_N.Extract_Separate (Par_Ctx, CV_N);

      pragma Unused (Par_Ctx);

      --  Process the chaining values with the outer XOF.
      XOF_Serial.Update
        (Ctx        => Ctx.Outer_XOF,
         Message    => CV_N);
   end Generic_Process_Parallel_Blocks;

   ------------------------------
   --  Generic instantiations  --
   ------------------------------

   procedure Process_8_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (XOF_Parallel_8);

   procedure Process_4_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (XOF_Parallel_4);

   procedure Process_2_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (XOF_Parallel_2);

   -----------------------
   --  Process_1_Block  --
   -----------------------

   procedure Process_1_Block
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
     with Pre => (Data'Length <= Block_Size_Bytes
                  and State_Of (Ctx) = Updating
                  and Ctx.Partial_Block_Length = 0),
     Post => (State_Of (Ctx) = Updating
              and Ctx.Input_Len = Ctx'Old.Input_Len
              and Ctx.Partial_Block_Length = Ctx'Old.Partial_Block_Length);
   --  Processes a single block using a serial XOF.

   -----------------------
   --  Process_1_Block  --
   -----------------------

   procedure Process_1_Block
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
   is
      Serial_Ctx : XOF_Serial.Context;

      CV : Types.Byte_Array (1 .. CV_Size_Bytes);

   begin
      --  Process N blocks in parallel and produce N changing values.
      XOF_Serial.Init   (Serial_Ctx);
      XOF_Serial.Update (Serial_Ctx, Data);
      XOF_Serial.Update (Serial_Ctx, Suffix_110, 3);

      XOF_Serial.Extract (Serial_Ctx, CV);

      pragma Unreferenced (Serial_Ctx);

      --  Process the chaining values with the outer XOF.
      XOF_Serial.Update
        (Ctx        => Ctx.Outer_XOF,
         Message    => CV);
   end Process_1_Block;

   --------------------------
   --  Add_To_First_Block  --
   --------------------------

   procedure Add_To_First_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
     with Pre => (Ctx.Input_Len < Block_Size_Bytes
                  and State_Of (Ctx) = Updating),
     Post => (Added <= Block_Size_Bytes
              and Added <= Data'Length
              and (if Added < Data'Length then Ctx.Partial_Block_Length = 0)
              and State_Of (Ctx) = Updating
              and Num_Bytes_Processed (Ctx) = Num_Bytes_Processed (Ctx'Old) + Byte_Count (Added));
   --  Add bytes to the first block.
   --
   --  The first block is processed differently from the others, as it is
   --  processed in the Ctx.Outer_XOF, in line with the chaining values. The
   --  first block does not produce a chaining value.
   --
   --  Other blocks (after the first one) are hashed separately to produce
   --  chaining values.

   --------------------------
   --  Add_To_First_Block  --
   --------------------------

   procedure Add_To_First_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
   is
      Free_In_Block : Natural;

   begin
      if Ctx.Partial_Block_Length > 0 then
         Free_In_Block := Block_Size_Bytes - Ctx.Partial_Block_Length;

         if Data'Length >= Free_In_Block then
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data (Data'First .. Data'First + (Free_In_Block - 1)));

            --  Add suffix '110^62' bits
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Suffix_110_62);

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Free_In_Block);
            Ctx.Partial_Block_Length := 0;

            Added := Free_In_Block;

         else
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data);

            Ctx.Partial_Block_Length := Ctx.Partial_Block_Length + Data'Length;
            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Data'Length);

            Added := Data'Length;

         end if;

      else

         if Data'Length >= Block_Size_Bytes then
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data (Data'First .. Data'First + (Block_Size_Bytes - 1)));

            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Suffix_110_62);

            Added := Block_Size_Bytes;

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Block_Size_Bytes);

         else
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data);

            Ctx.Partial_Block_Length := Data'Length;

            Added := Data'Length;

            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Data'Length);

         end if;

      end if;
   end Add_To_First_Block;

   ----------------------------
   --  Add_To_Partial_Block  --
   ----------------------------

   procedure Add_To_Partial_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
     with Pre => (Ctx.Input_Len >= Block_Size_Bytes
                  and Max_Input_Length (Ctx) >= Byte_Count (Data'Length)
                  and State_Of (Ctx) = Updating),
     Post => (Added <= Block_Size_Bytes
              and Added <= Data'Length
              and (if Added < Data'Length then Ctx.Partial_Block_Length = 0)
              and State_Of (Ctx) = Updating
              and Num_Bytes_Processed (Ctx) = Num_Bytes_Processed (Ctx'Old) + Byte_Count (Added)),
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
         Free_In_Block := Block_Size_Bytes - Ctx.Partial_Block_Length;

         if Data'Length >= Free_In_Block then
            XOF_Serial.Update
              (Ctx     => Ctx.Partial_Block_XOF,
               Message => Data (Data'First .. Data'First + (Free_In_Block - 1)));

            --  Add suffix '110' bits
            XOF_Serial.Update
              (Ctx        => Ctx.Partial_Block_XOF,
               Message    => Suffix_110,
               Bit_Length => 3);

            pragma Warnings
              (GNATprove, Off,
               """Ctx.Partial_Block_XOF"" is set by ""Extract"" but not used after the call",
               Reason => "No further data needs to be extracted before Init");

            XOF_Serial.Extract
              (Ctx    => Ctx.Partial_Block_XOF,
               Digest => CV);

            pragma Warnings (GNATprove, Off);

            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => CV);

            XOF_Serial.Init (Ctx.Partial_Block_XOF);

            Ctx.Partial_Block_Length := 0;
            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Free_In_Block);

            Added := Free_In_Block;

         else
            XOF_Serial.Update
              (Ctx     => Ctx.Partial_Block_XOF,
               Message => Data);

            Ctx.Partial_Block_Length := Ctx.Partial_Block_Length + Data'Length;
            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Data'Length);

            Added := Data'Length;

         end if;

      else
         Added := 0;

      end if;
   end Add_To_Partial_Block;

   ---------------------
   --  Update_String  --
   ---------------------

   procedure Update_String (Ctx : in out Context;
                            Str : in     String)
     with Global => null,
     Pre => (State_Of (Ctx) = Updating
             and Byte_Count (Str'Length) <= Max_Input_Length (Ctx)),
     Post => (State_Of (Ctx) = Updating
              and Max_Input_Length (Ctx) = Max_Input_Length (Ctx'Old) - Byte_Count (Str'Length));

   ---------------------
   --  Update_String  --
   ---------------------

   procedure Update_String (Ctx : in out Context;
                            Str : in     String)
   is
      Buffer : Types.Byte_Array (1 .. 128) with Relaxed_Initialization;

      Remaining : Natural := Str'Length;
      Offset    : Natural := 0;

      Max_Input_Length_Old : constant Byte_Count := Max_Input_Length (Ctx) with Ghost;

   begin
      while Remaining >= Buffer'Length loop
         pragma Loop_Variant (Increases => Offset,
                              Decreases => Remaining);
         pragma Loop_Invariant (Offset + Remaining = Str'Length);
         pragma Loop_Invariant (State_Of (Ctx) = Updating);
         pragma Loop_Invariant (Max_Input_Length (Ctx) =
                                  Max_Input_Length_Old - Byte_Count (Offset));

         Util.To_Byte_Array (Buffer, Str (Str'First + Offset ..
                                          Str'First + Offset + (Buffer'Length - 1)));
         Update (Ctx, Buffer);

         Offset    := Offset    + Buffer'Length;
         Remaining := Remaining - Buffer'Length;
      end loop;

      if Remaining > 0 then
         Util.To_Byte_Array (Buffer (1 .. Remaining), Str (Str'First + Offset .. Str'Last));

         Update (Ctx, Buffer (1 .. Remaining));
      end if;
   end Update_String;

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx : out Context)
   is
   begin
      Ctx.Partial_Block_Length := 0;
      Ctx.Input_Len            := 0;
      Ctx.Finished             := False;

      XOF_Serial.Init (Ctx.Outer_XOF);
      XOF_Serial.Init (Ctx.Partial_Block_XOF);
   end Init;

   --------------
   --  Update  --
   --------------

   procedure Update (Ctx  : in out Context;
                     Data : in     Types.Byte_Array)
   is
      Initial_Bytes_Processed : constant Byte_Count := Num_Bytes_Processed (Ctx);

      Remaining               : Natural;
      Offset                  : Natural;
      Pos                     : Types.Index_Number;

   begin

      if Ctx.Input_Len < Block_Size_Bytes then
         Add_To_First_Block (Ctx, Data, Offset);
      else
         Add_To_Partial_Block (Ctx, Data, Offset);
      end if;

      Remaining := Data'Length - Offset;

      if Remaining > 0 then
         pragma Assert (Ctx.Partial_Block_Length = 0);
         pragma Assert (Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset));

         --  Process blocks of 8 in parallel
         while Remaining >= Block_Size_Bytes * 8 loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (State_Of (Ctx) = Updating);
            pragma Loop_Invariant (Ctx.Input_Len = Initial_Bytes_Processed + Byte_Count (Offset));
            pragma Loop_Invariant (Byte_Count (Remaining) <= Byte_Count'Last - Ctx.Input_Len);
            pragma Loop_Invariant (Ctx.Partial_Block_Length = 0);

            Pos := Data'First + Offset;

            Process_8_Parallel_Blocks
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + ((Block_Size_Bytes * 8) - 1)));

            Ctx.Input_Len := Ctx.Input_Len + (Block_Size_Bytes * 8);

            Offset    := Offset    + (Block_Size_Bytes * 8);
            Remaining := Remaining - (Block_Size_Bytes * 8);
         end loop;

      --  Process blocks of 4 in parallel
         while Remaining >= Block_Size_Bytes * 4 loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (State_Of (Ctx) = Updating);
            pragma Loop_Invariant (Ctx.Input_Len = Initial_Bytes_Processed + Byte_Count (Offset));
            pragma Loop_Invariant (Byte_Count (Remaining) <= Byte_Count'Last - Ctx.Input_Len);
            pragma Loop_Invariant (Ctx.Partial_Block_Length = 0);

            Pos := Data'First + Offset;

            Process_4_Parallel_Blocks
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + ((Block_Size_Bytes * 4) - 1)));

            Ctx.Input_Len := Ctx.Input_Len + (Block_Size_Bytes * 4);

            Offset    := Offset    + (Block_Size_Bytes * 4);
            Remaining := Remaining - (Block_Size_Bytes * 4);
         end loop;

      --  Process blocks of 2 in parallel
         while Remaining >= Block_Size_Bytes * 2 loop
            pragma Loop_Invariant (Offset + Remaining = Data'Length);
            pragma Loop_Invariant (State_Of (Ctx) = Updating);
            pragma Loop_Invariant (Ctx.Input_Len = Initial_Bytes_Processed + Byte_Count (Offset));
            pragma Loop_Invariant (Byte_Count (Remaining) <= Byte_Count'Last - Ctx.Input_Len);
            pragma Loop_Invariant (Ctx.Partial_Block_Length = 0);

            Pos := Data'First + Offset;

            Process_2_Parallel_Blocks
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + ((Block_Size_Bytes * 2) - 1)));

            Ctx.Input_Len := Ctx.Input_Len + (Block_Size_Bytes * 2);

            Offset    := Offset    + (Block_Size_Bytes * 2);
            Remaining := Remaining - (Block_Size_Bytes * 2);
         end loop;

      --  Process single blocks
         if Remaining >= Block_Size_Bytes then
            Pos := Data'First + Offset;

            pragma Assert (Byte_Count (Remaining) <= Max_Input_Length (Ctx));
            pragma Assert (Ctx.Partial_Block_Length = 0);

            Process_1_Block
              (Ctx  => Ctx,
               Data => Data (Pos .. Pos + (Block_Size_Bytes - 1)));

            Ctx.Input_Len := Ctx.Input_Len + Block_Size_Bytes;

            Offset    := Offset    + Block_Size_Bytes;
            Remaining := Remaining - Block_Size_Bytes;
         end if;

         pragma Assert (Offset + Remaining = Data'Length);
         pragma Assert (State_Of (Ctx) = Updating);
         pragma Assert (Num_Bytes_Processed (Ctx) = Initial_Bytes_Processed + Byte_Count (Offset));

      --  Process remaining data.
         if Remaining > 0 then
            XOF_Serial.Update
              (Ctx     => Ctx.Partial_Block_XOF,
               Message => Data (Data'First + Offset .. Data'Last));

            Ctx.Partial_Block_Length := Remaining;
            Ctx.Input_Len := Ctx.Input_Len + Byte_Count (Remaining);
         end if;
      end if;
   end Update;

   --------------
   --  Finish  --
   --------------

   procedure Finish (Ctx           : in out Context;
                     Customization : in     String)
   is

      Nb_Blocks : Long_Long_Integer;

   begin
      Update_String (Ctx, Customization);
      Update (Ctx, Util.Right_Encode_K12 (Customization'Length));

      pragma Assert (Num_Bytes_Processed (Ctx) > 0);

      Ctx.Finished := True;

      if Ctx.Input_Len < Block_Size_Bytes then
         XOF_Serial.Update
           (Ctx        => Ctx.Outer_XOF,
            Message    => Suffix_11,
            Bit_Length => 2);

      else
            --  Produce final CV for final block
            --  (if the last block is a partial block)
         if Ctx.Partial_Block_Length > 0 then
            Ctx.Partial_Block_Length := 0;
            Nb_Blocks := 1 + (Long_Long_Integer (Ctx.Input_Len) / Block_Size_Bytes);

            XOF_Serial.Update
              (Ctx        => Ctx.Partial_Block_XOF,
               Message    => Suffix_110,
               Bit_Length => 3);

            declare
               CV : Types.Byte_Array (1 .. CV_Size_Bytes);
            begin

               pragma Warnings
                 (GNATprove, Off,
                  """Ctx.Partial_Block_XOF"" is set by ""Extract"" but not used after call",
                  Reason => "No further data needs to be extracted before Init");

               XOF_Serial.Extract
                 (Ctx    => Ctx.Partial_Block_XOF,
                  Digest => CV);

               pragma Warnings (GNATprove, On);

               XOF_Serial.Update
                 (Ctx     => Ctx.Outer_XOF,
                  Message => CV);
            end;

               --  Preserve type invariant
            XOF_Serial.Init (Ctx.Partial_Block_XOF);

         else
            Nb_Blocks := Long_Long_Integer (Ctx.Input_Len) / Block_Size_Bytes;
         end if;

         XOF_Serial.Update
           (Ctx     => Ctx.Outer_XOF,
            Message => Util.Right_Encode_K12 (Nb_Blocks - 1));

         XOF_Serial.Update
           (Ctx        => Ctx.Outer_XOF,
            Message    => Suffix_FFFF_01,
            Bit_Length => 18);
      end if;
   end Finish;

   ---------------
   --  Extract  --
   ---------------

   procedure Extract (Ctx  : in out Context;
                      Data :    out Types.Byte_Array)
   is
   begin
      XOF_Serial.Extract
        (Ctx    => Ctx.Outer_XOF,
         Digest => Data);
   end Extract;

end Keccak.Generic_KangarooTwelve;
