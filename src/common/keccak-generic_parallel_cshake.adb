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
with Keccak.Util; use Keccak.Util;

package body Keccak.Generic_Parallel_CSHAKE
is

   ---------------------------
   --  Process_Full_Blocks  --
   ---------------------------

   procedure Process_Full_Blocks
     (Ctx          : in out Context;
      Block        : in out Types.Byte_Array;
      Input        : in     Types.Byte_Array;
      Block_Offset : in out Natural)
     with Global => null,
     Pre => (Block_Offset < Block'Length
             and Block'First = 0
             and Block'Length = Rate / 8
             and State_Of (Ctx) = Updating),
     Post => (Block_Offset < Block'Length
              and State_Of (Ctx) = Updating);

   procedure Process_Full_Blocks_String
     (Ctx          : in out Context;
      Block        : in out Types.Byte_Array;
      Input        : in     String;
      Block_Offset : in out Natural)
     with Global => null,
     Pre => (Block_Offset < Block'Length
             and Block'First = 0
             and Block'Length = Rate / 8
             and State_Of (Ctx) = Updating),
     Post => (Block_Offset < Block'Length
              and State_Of (Ctx) = Updating);

   ---------------------------
   --  Process_Full_Blocks  --
   ---------------------------

   procedure Process_Full_Blocks
     (Ctx          : in out Context;
      Block        : in out Types.Byte_Array;
      Input        : in     Types.Byte_Array;
      Block_Offset : in out Natural)
   is
      use type XOF.States;

      Block_Length    : constant Natural := Block'Length - Block_Offset;
      Input_Remaining : Natural := Input'Length;
      Input_Offset    : Natural := 0;

      Length                       : Natural;
      Num_Full_Blocks              : Natural;

      Pos                          : Types.Index_Number;

   begin

      if Block_Offset > 0 then
         --  Merge first bytes of Input with the last bytes currently in
         --  the block.

         if Input_Remaining < Block_Length then
            --  Not enough for a full block.

            Block (Block_Offset .. Block_Offset + Input_Remaining - 1) :=
              Input (Input'First .. Input'First + Input_Remaining - 1);

            Input_Offset    := Input'Length;
            Block_Offset    := Block_Offset    + Input_Remaining;
            Input_Remaining := 0;

         else
            --  We have enough for a full block

            Block (Block_Offset .. Block'Last) :=
              Input (Input'First .. Input'First + Block_Length - 1);

            XOF.Update_All (Ctx.XOF_Ctx, Block);

            Input_Offset    := Input_Offset    + Block_Length;
            Input_Remaining := Input_Remaining - Block_Length;

            Block_Offset    := 0;

         end if;
      end if;

      pragma Assert_And_Cut
        (Input_Offset + Input_Remaining = Input'Length
         and Block_Offset < Block'Length
         and Block'Length = Rate / 8
         and State_Of (Ctx) = Updating
         and XOF.State_Of (Ctx.XOF_Ctx) = XOF.Updating
         and (if Input_Remaining > 0 then Block_Offset = 0));

      --  Now process as many full blocks from Input as we can.
      Num_Full_Blocks := Input_Remaining / Block'Length;
      if Num_Full_Blocks > 0 then
         Pos := Input'First + Input_Offset;

         Length := Num_Full_Blocks * Block'Length;

         XOF.Update_All (Ctx.XOF_Ctx, Input (Pos .. Pos + Length - 1));

         Input_Offset    := Input_Offset    + Length;
         Input_Remaining := Input_Remaining - Length;
      end if;

      pragma Assert_And_Cut
        (Input_Offset + Input_Remaining = Input'Length
         and Block_Offset < Block'Length
         and Block'Length = Rate / 8
         and State_Of (Ctx) = Updating
         and (if Input_Remaining > 0 then Block_Offset = 0)
         and Input_Remaining < Block'Length);

      --  Store any leftover bytes in the block
      if Input_Remaining > 0 then
         Pos := Input'First + Input_Offset;

         Block (0 .. Input_Remaining - 1) := Input (Pos .. Input'Last);

         Block_Offset := Input_Remaining;
      end if;

   end Process_Full_Blocks;

   ---------------------------------
   -- Process_Full_Blocks_String  --
   ---------------------------------

   procedure Process_Full_Blocks_String
     (Ctx          : in out Context;
      Block        : in out Types.Byte_Array;
      Input        : in     String;
      Block_Offset : in out Natural)
   is
      Bytes : Types.Byte_Array (1 .. 128);

      Remaining : Natural := Input'Length;
      Offset    : Natural := 0;

   begin

      while Remaining >= Bytes'Length loop
         pragma Loop_Invariant (Offset + Remaining = Input'Length);

         To_Byte_Array (Bytes, Input (Input'First + Offset ..
                                      Input'First + Offset + Bytes'Length - 1));

         Process_Full_Blocks (Ctx, Block, Bytes, Block_Offset);

         Offset    := Offset + Bytes'Length;
         Remaining := Remaining - Bytes'Length;
      end loop;

      if Remaining > 0 then
         To_Byte_Array (Bytes (1 .. Remaining), Input (Input'First + Offset .. Input'Last));

         Process_Full_Blocks (Ctx, Block, Bytes (1 .. Remaining), Block_Offset);
      end if;

   end Process_Full_Blocks_String;

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx           :    out Context;
                   Customization : in     String;
                   Function_Name : in     String)
   is
      Rate_Bytes : constant Positive := Rate / 8;

      Block        : Types.Byte_Array (0 .. Rate_Bytes - 1) := (others => 0);

      Block_Offset : Natural;

   begin
      XOF.Init (Ctx.XOF_Ctx);

      --  We need to make sure that the data length for each call to
      --  XOF.Update_Separate is a multiple of the rate in order to keep the XOF
      --  in the "Updating" state. This requires packing the encoded
      --  rate, customization string, and function name into a block which is
      --  the length of the rate.
      --
      --  +------+---------------+---------------+
      --  | rate | Function_Name | Customization |
      --  +------+---------------+---------------+
      --  |<-------------->|
      --        Rate

      Block_Offset := 0;

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Left_Encode_NIST (Rate_Bytes),
         Block_Offset => Block_Offset);

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Left_Encode_NIST_Bit_Length (Function_Name'Length),
         Block_Offset => Block_Offset);

      Process_Full_Blocks_String
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Function_Name,
         Block_Offset => Block_Offset);

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Left_Encode_NIST_Bit_Length (Customization'Length),
         Block_Offset => Block_Offset);

      Process_Full_Blocks_String
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Customization,
         Block_Offset => Block_Offset);

      if Block_Offset > 0 then
         --  Need to add padding zeroes to leftover data
         Block (Block_Offset .. Block'Last) := (others => 0);

         XOF.Update_All (Ctx.XOF_Ctx, Block);
      end if;

   end Init;

   -----------------------
   --  Update_Separate  --
   -----------------------

   procedure Update_Separate (Ctx  : in out Context;
                              Data : in     Types.Byte_Array)
   is
   begin
      XOF.Update_Separate (Ctx.XOF_Ctx, Data);
   end Update_Separate;

   -----------------------
   --  Extract_Separate --
   -----------------------

   procedure Extract_Separate (Ctx  : in out Context;
                               Data :    out Types.Byte_Array)
   is
   begin
      XOF.Extract_Separate (Ctx.XOF_Ctx, Data);
   end Extract_Separate;

end Keccak.Generic_Parallel_CSHAKE;
