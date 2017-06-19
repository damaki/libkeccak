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
with Keccak.Util;

package body Keccak.Generic_KangarooTwelve
is

   Suffix_110_62  : constant Types.Byte_Array (1 .. 8) := (1 => 2#011#, others => 0);
   Suffix_110     : constant Types.Byte_Array (1 .. 1) := (1 => 2#011#);
   Suffix_11      : constant Types.Byte_Array (1 .. 1) := (1 => 2#11#);
   Suffix_FFFF_01 : constant Types.Byte_Array (1 .. 3) := (16#FF#, 16#FF#, 2#10#);

   generic
      with package XOF_Parallel_N is new Keccak.Generic_Parallel_XOF (<>);
   procedure Generic_Process_Parallel_Blocks
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
     with Pre => Data'Length = Block_Size_Bytes * XOF_Parallel_N.Num_Parallel_Instances;


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
      XOF_Parallel_N.Update  (Par_Ctx, Data);

      pragma Warnings (GNATprove, Off,
                       "unused assignment to ""Par_Ctx""",
                       Reason => "No further data needs to be extracted");

      XOF_Parallel_N.Extract (Par_Ctx, CV_N);

      pragma Warnings (GNATprove, On);

      --  Process the chaining values with the outer XOF.
      XOF_Serial.Update
        (Ctx        => Ctx.Outer_XOF,
         Message    => CV_N);
   end Generic_Process_Parallel_Blocks;


   procedure Process_8_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (XOF_Parallel_8);

   procedure Process_4_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (XOF_Parallel_4);

   procedure Process_2_Parallel_Blocks
   is new Generic_Process_Parallel_Blocks (XOF_Parallel_2);


   procedure Process_1_Block
     (Ctx  : in out Context;
      Data : in     Types.Byte_Array)
     with Pre => Data'Length <= Block_Size_Bytes
   is
      Serial_Ctx : XOF_Serial.Context;

      CV: Types.Byte_Array (1 .. CV_Size_Bytes);

   begin
      --  Process N blocks in parallel and produce N changing values.
      XOF_Serial.Init   (Serial_Ctx);
      XOF_Serial.Update (Serial_Ctx, Data);
      XOF_Serial.Update (Serial_Ctx, Suffix_110, 3);

      pragma Warnings (GNATprove, Off,
                       "unused assignment to ""Serial_Ctx""",
                       Reason => "No further data needs to be extracted");

      XOF_Serial.Extract (Serial_Ctx, CV);

      pragma Warnings (GNATprove, On);

      --  Process the chaining values with the outer XOF.
      XOF_Serial.Update
        (Ctx        => Ctx.Outer_XOF,
         Message    => CV);
   end Process_1_Block;


   procedure Add_To_First_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
     with Pre => Ctx.Nb_Blocks = 0,
     Post => (Added <= Block_Size_Bytes
              and Added <= Data'Length
              and (if Added < Data'Length then Ctx.Partial_Block_Length = 0))
   is
      Pos           : Types.Index_Number;
      Free_In_Block : Natural;

   begin
      if Ctx.Partial_Block_Length > 0 then
         Free_In_Block := Block_Size_Bytes - Ctx.Partial_Block_Length;

         Pos := Data'First;

         if Data'Length >= Free_In_Block then
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data (Pos .. Pos + (Free_In_Block - 1)));


            --  Add suffix '110^62' bits
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Suffix_110_62);

            Ctx.Nb_Blocks            := 1;
            Ctx.Partial_Block_Length := 0;

            Added := Free_In_Block;

         else
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data);

            Ctx.Partial_Block_Length := Ctx.Partial_Block_Length + Data'Length;

            Added := Data'Length;

         end if;

      else

         if Data'Length >= Block_Size_Bytes then
            Pos := Data'First;

            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data (Pos .. Pos + (Block_Size_Bytes - 1)));

            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Suffix_110_62);

            Added := Block_Size_Bytes;

            Ctx.Nb_Blocks := 1;

         else
            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => Data);

            Ctx.Partial_Block_Length := Data'Length;

            Added := Data'Length;

         end if;

      end if;
   end Add_To_First_Block;


   procedure Add_To_Partial_Block
     (Ctx   : in out Context;
      Data  : in     Types.Byte_Array;
      Added :    out Natural)
     with Pre => Ctx.Nb_Blocks > 0,
     Post => (Added <= Block_Size_Bytes
              and Added <= Data'Length
              and (if Added < Data'Length then Ctx.Partial_Block_Length = 0))
   is
      Pos           : Types.Index_Number;
      Free_In_Block : Natural;
      CV            : Types.Byte_Array (1 .. CV_Size_Bytes);

   begin
      if Ctx.Partial_Block_Length > 0 then
         Free_In_Block := Block_Size_Bytes - Ctx.Partial_Block_Length;

         Pos := Data'First;

         if Data'Length >= Free_In_Block then
            XOF_Serial.Update
              (Ctx     => Ctx.Partial_Block_XOF,
               Message => Data (Pos .. Pos + (Free_In_Block - 1)));

            --  Add suffix '110' bits
            XOF_Serial.Update
              (Ctx        => Ctx.Partial_Block_XOF,
               Message    => Suffix_110,
               Bit_Length => 3);

            pragma Warnings
              (GNATprove, Off,
               "unused assignment",
               Reason => "No further data needs to be extracted before Init");

            XOF_Serial.Extract
              (Ctx    => Ctx.Partial_Block_XOF,
               Digest => CV);

            pragma Warnings (GNATprove, On);

            XOF_Serial.Update
              (Ctx     => Ctx.Outer_XOF,
               Message => CV);

            XOF_Serial.Init (Ctx.Partial_Block_XOF);

            Ctx.Nb_Blocks            := Ctx.Nb_Blocks + 1;
            Ctx.Partial_Block_Length := 0;

            Added := Free_In_Block;

         else
            XOF_Serial.Update
              (Ctx     => Ctx.Partial_Block_XOF,
               Message => Data);

            Ctx.Partial_Block_Length := Ctx.Partial_Block_Length + Data'Length;

            Added := Data'Length;

         end if;

      end if;
   end Add_To_Partial_Block;


   procedure Init (Ctx : out Context)
   is
   begin
      XOF_Serial.Init (Ctx.Outer_XOF);
      XOF_Serial.Init (Ctx.Partial_Block_XOF);

      Ctx.Nb_Blocks            := 0;
      Ctx.Partial_Block_Length := 0;
      Ctx.Finished             := False;
   end Init;


   procedure Update (Ctx  : in out Context;
                     Data : in     Types.Byte_Array)
   is
      Remaining : Natural;
      Offset    : Natural;
      Pos       : Types.Index_Number;

   begin

      if Ctx.Nb_Blocks = 0 then
         Add_To_First_Block (Ctx, Data, Offset);
      else
         Add_To_Partial_Block (Ctx, Data, Offset);
      end if;

      Remaining := Data'Length - Offset;

      --  Process blocks of 8 in parallel
      while Remaining >= Block_Size_Bytes * 8 loop
         pragma Loop_Invariant (Offset + Remaining = Data'Length);

         Pos := Data'First + Offset;

         Process_8_Parallel_Blocks
           (Ctx  => Ctx,
            Data => Data (Pos .. Pos + ((Block_Size_Bytes * 8) - 1)));

         Ctx.Nb_Blocks := Ctx.Nb_Blocks + 8;

         Offset    := Offset    + (Block_Size_Bytes * 8);
         Remaining := Remaining - (Block_Size_Bytes * 8);
      end loop;


      --  Process blocks of 4 in parallel
      while Remaining >= Block_Size_Bytes * 4 loop
         pragma Loop_Invariant (Offset + Remaining = Data'Length);

         Pos := Data'First + Offset;

         Process_4_Parallel_Blocks
           (Ctx  => Ctx,
            Data => Data (Pos .. Pos + ((Block_Size_Bytes * 4) - 1)));

         Ctx.Nb_Blocks := Ctx.Nb_Blocks + 4;

         Offset    := Offset    + (Block_Size_Bytes * 4);
         Remaining := Remaining - (Block_Size_Bytes * 4);
      end loop;


      --  Process blocks of 2 in parallel
      while Remaining >= Block_Size_Bytes * 2 loop
         pragma Loop_Invariant (Offset + Remaining = Data'Length);

         Pos := Data'First + Offset;

         Process_2_Parallel_Blocks
           (Ctx  => Ctx,
            Data => Data (Pos .. Pos + ((Block_Size_Bytes * 2) - 1)));

         Ctx.Nb_Blocks := Ctx.Nb_Blocks + 2;

         Offset    := Offset    + (Block_Size_Bytes * 2);
         Remaining := Remaining - (Block_Size_Bytes * 2);
      end loop;


      --  Process single blocks
      if Remaining >= Block_Size_Bytes then
         Pos := Data'First + Offset;

         Process_1_Block
           (Ctx  => Ctx,
            Data => Data (Pos .. Pos + (Block_Size_Bytes - 1)));

         Ctx.Nb_Blocks := Ctx.Nb_Blocks + 1;

         Offset    := Offset    + Block_Size_Bytes;
         Remaining := Remaining - Block_Size_Bytes;
      end if;

      --  Process remaining data.
      if Remaining > 0 then
         XOF_Serial.Update
           (Ctx     => Ctx.Partial_Block_XOF,
            Message => Data (Data'First + Offset .. Data'Last));

         Ctx.Partial_Block_Length := Remaining;
      end if;
   end Update;


   procedure Finish (Ctx           : in out Context;
                     Customization : in     String)
   is
   begin
      Update (Ctx, Util.To_Byte_Array (Customization));
      Update (Ctx, Util.Right_Encode_K12 (Customization'Length));

      Ctx.Finished := True;

      if Ctx.Nb_Blocks = 0 then
         XOF_Serial.Update
           (Ctx        => Ctx.Outer_XOF,
            Message    => Suffix_11,
            Bit_Length => 2);

      else
            --  Produce final CV for final block
            --  (if the last block is a partial block)
         if Ctx.Partial_Block_Length > 0 then
            Ctx.Nb_Blocks := Ctx.Nb_Blocks + 1;

            XOF_Serial.Update
              (Ctx        => Ctx.Partial_Block_XOF,
               Message    => Suffix_110,
               Bit_Length => 3);

            declare
               CV : Types.Byte_Array (1 .. CV_Size_Bytes);
            begin

               pragma Warnings
                 (GNATprove, Off,
                  "unused assignment",
                  Reason => "No further data needs to be extracted");

               XOF_Serial.Extract
                 (Ctx    => Ctx.Partial_Block_XOF,
                  Digest => CV);

               pragma Warnings (GNATprove, Off);

               XOF_Serial.Update
                 (Ctx     => Ctx.Outer_XOF,
                  Message => CV);
            end;

               --  Preserve type invariant
            XOF_Serial.Init (Ctx.Partial_Block_XOF);
         end if;

         XOF_Serial.Update
           (Ctx     => Ctx.Outer_XOF,
            Message => Util.Right_Encode_K12 (Ctx.Nb_Blocks - 1));

         XOF_Serial.Update
           (Ctx        => Ctx.Outer_XOF,
            Message    => Suffix_FFFF_01,
            Bit_Length => 18);
      end if;
   end Finish;


   procedure Extract (Ctx  : in out Context;
                      Data :    out Types.Byte_Array)
   is
   begin
      XOF_Serial.Extract
        (Ctx    => Ctx.Outer_XOF,
         Digest => Data);
   end Extract;

end Keccak.Generic_KangarooTwelve;
