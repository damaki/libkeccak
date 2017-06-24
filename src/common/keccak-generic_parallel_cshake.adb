with Keccak.Util; use Keccak.Util;

package body Keccak.Generic_Parallel_CSHAKE
is


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


   procedure Process_Full_Blocks
     (Ctx          : in out Context;
      Block        : in out Types.Byte_Array;
      Input        : in     Types.Byte_Array;
      Block_Offset : in out Natural)
   is
      use type XOF.States;

      Block_Remaining : Natural := Block'Length - Block_Offset;
      Input_Remaining : Natural := Input'Length;
      Input_Offset    : Natural := 0;

      Length                       : Natural;
      Num_Full_Blocks              : Natural;

      Pos                          : Types.Index_Number;

   begin

      if Block_Offset > 0 then
         --  Merge first bytes of Input with the last bytes currently in
         --  the block.

         if Input_Remaining < Block_Remaining then
            --  Not enough for a full block.

            Block (Block_Offset .. Block_Offset + Input_Remaining - 1) :=
              Input (Input'First .. Input'First + Input_Remaining - 1);

            Input_Offset    := Input'Length;
            Block_Offset    := Block_Offset    + Input_Remaining;
            Input_Remaining := 0;

         else
            --  We have enough for a full block

            Block (Block_Offset .. Block'Last) :=
              Input (Input'First .. Input'First + Block_Remaining - 1);

            XOF.Update_All (Ctx.XOF_Ctx, Block);

            Input_Offset    := Input_Offset    + Block_Remaining;
            Input_Remaining := Input_Remaining - Block_Remaining;

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
         Input        => Left_Encode (Rate_Bytes),
         Block_Offset => Block_Offset);

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Left_Encode_Bit_Length(Function_Name'Length),
         Block_Offset => Block_Offset);

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => To_Byte_Array (Function_Name),
         Block_Offset => Block_Offset);

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => Left_Encode_Bit_Length(Customization'Length),
         Block_Offset => Block_Offset);

      Process_Full_Blocks
        (Ctx          => Ctx,
         Block        => Block,
         Input        => To_Byte_Array (Customization),
         Block_Offset => Block_Offset);

      if Block_Offset > 0 then
         --  Need to add padding zeroes to leftover data
         Block (Block_Offset .. Block'Last) := (others => 0);

         XOF.Update_All (Ctx.XOF_Ctx, Block);
      end if;

   end Init;


   procedure Update_Separate (Ctx  : in out Context;
                              Data : in     Types.Byte_Array)
   is
   begin
      XOF.Update_Separate (Ctx.XOF_Ctx, Data);
   end Update_Separate;


   procedure Extract_Separate (Ctx  : in out Context;
                               Data :    out Types.Byte_Array)
   is
   begin
      XOF.Extract_Separate (Ctx.XOF_Ctx, Data);
   end Extract_Separate;

end Keccak.Generic_Parallel_CSHAKE;
