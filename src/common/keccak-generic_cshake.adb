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

package body Keccak.Generic_CSHAKE
is

   procedure Update_String (XOF_Ctx : in out XOF.Context;
                            Input   : in     String)
     with Global => null,
     Pre  => XOF.State_Of (XOF_Ctx) = XOF.Updating,
     Post => XOF.State_Of (XOF_Ctx) = XOF.Updating;

   procedure Update_String (XOF_Ctx : in out XOF.Context;
                            Input   : in     String)
   is
      Buffer : Types.Byte_Array (1 .. 128) with Relaxed_Initialization;

      Remaining : Natural := Input'Length;
      Offset    : Natural := 0;

   begin

      while Remaining >= Buffer'Length loop
         pragma Loop_Invariant (XOF.State_Of (XOF_Ctx) = XOF.Updating);
         pragma Loop_Invariant (Offset + Remaining = Input'Length);

         Util.To_Byte_Array (Buffer,
                             Input (Input'First + Offset ..
                                    Input'First + Offset + (Buffer'Length - 1)));

         XOF.Update (XOF_Ctx, Buffer);

         Offset    := Offset + Buffer'Length;
         Remaining := Remaining - Buffer'Length;
      end loop;

      if Remaining > 0 then
         Util.To_Byte_Array (Buffer (1 .. Remaining),
                             Input (Input'First + Offset .. Input'Last));

         XOF.Update (XOF_Ctx, Buffer (1 .. Remaining));
      end if;
   end Update_String;

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx           :    out Context;
                   Customization : in     String := "";
                   Function_Name : in     String := "")
   is

      Rate_Bytes : constant Positive := Rate / 8;

      Encoded_Customization_Length : constant Byte_Array
        := Util.Left_Encode_NIST_Bit_Length (Customization'Length);

      Encoded_Function_Name_Length : constant Byte_Array
        := Util.Left_Encode_NIST_Bit_Length (Function_Name'Length);

      Encoded_Rate                 : constant Byte_Array
        := Util.Left_Encode_NIST (Rate_Bytes);

      Zeroes : constant Types.Byte_Array (1 .. Rate_Bytes) := (others => 0);

      Padding_Length : Natural;

   begin
      XOF.Init (Ctx.XOF_Ctx);

      XOF.Update (Ctx     => Ctx.XOF_Ctx,
                  Message => Encoded_Rate);

      XOF.Update (Ctx     => Ctx.XOF_Ctx,
                  Message => Encoded_Function_Name_Length);
      Update_String (Ctx.XOF_Ctx, Function_Name);

      XOF.Update (Ctx     => Ctx.XOF_Ctx,
                  Message => Encoded_Customization_Length);
      Update_String (Ctx.XOF_Ctx, Customization);

      Padding_Length :=
        Encoded_Rate'Length mod Rate_Bytes +
        Encoded_Customization_Length'Length mod Rate_Bytes +
        Encoded_Function_Name_Length'Length mod Rate_Bytes +
        Customization'Length mod Rate_Bytes +
        Function_Name'Length mod Rate_Bytes;

      Padding_Length := Rate_Bytes - (Padding_Length mod Rate_Bytes);

      if Padding_Length mod Rate_Bytes /= 0 then
         XOF.Update (Ctx        => Ctx.XOF_Ctx,
                     Message    => Zeroes (1 .. Padding_Length));
      end if;
   end Init;

   --------------
   --  Update  --
   --------------

   procedure Update (Ctx     : in out Context;
                     Message : in     Byte_Array)
   is
   begin
      XOF.Update (Ctx.XOF_Ctx, Message);
   end Update;

   --------------
   --  Update  --
   --------------

   procedure Update (Ctx        : in out Context;
                     Message    : in     Byte_Array;
                     Bit_Length : in     Natural)
   is
   begin
      XOF.Update (Ctx.XOF_Ctx, Message, Bit_Length);
   end Update;

   ---------------
   --  Extract  --
   ---------------

   procedure Extract (Ctx    : in out Context;
                      Digest :    out Byte_Array)
   is
   begin
      XOF.Extract (Ctx.XOF_Ctx, Digest);
   end Extract;

end Keccak.Generic_CSHAKE;
