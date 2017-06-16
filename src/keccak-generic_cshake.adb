-------------------------------------------------------------------------------
-- Copyright (c) 2016, Daniel King
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

package body Keccak.Generic_CSHAKE
is

   procedure Init(Ctx           :    out Context;
                  Customization : in     String := "";
                  Function_Name : in     String := "")
   is

      Rate_Bytes : constant Positive := Rate / 8;

      Encoded_Customization_Length : constant Byte_Array
        := Util.Left_Encode_Bit_Length(Customization'Length);

      Encoded_Function_Name_Length : constant Byte_Array
        := Util.Left_Encode_Bit_Length(Function_Name'Length);

      Encoded_Rate                 : constant Byte_Array
        := Util.Left_Encode (Rate_Bytes);

      Zeroes : constant Types.Byte_Array(1 .. Rate_Bytes) := (others => 0);

      Padding_Length : Natural;

   begin
      XOF.Init(Ctx.XOF_Ctx);

      XOF.Update(Ctx     => Ctx.XOF_Ctx,
                 Message => Encoded_Rate);

      XOF.Update(Ctx     => Ctx.XOF_Ctx,
                 Message => Encoded_Function_Name_Length);
      XOF.Update(Ctx     => Ctx.XOF_Ctx,
                 Message => Util.To_Byte_Array(Function_Name));

      XOF.Update(Ctx     => Ctx.XOF_Ctx,
                 Message => Encoded_Customization_Length);
      XOF.Update(Ctx     => Ctx.XOF_Ctx,
                 Message => Util.To_Byte_Array(Customization));

      Padding_Length :=
        Encoded_Rate'Length mod Rate_Bytes +
        Encoded_Customization_Length'Length mod Rate_Bytes +
        Encoded_Function_Name_Length'Length mod Rate_Bytes +
        Customization'Length mod Rate_Bytes +
        Function_Name'Length mod Rate_Bytes;

      Padding_Length := Rate_Bytes - (Padding_Length mod Rate_Bytes);

      if Padding_Length mod Rate_Bytes /= 0 then
         XOF.Update(Ctx        => Ctx.XOF_Ctx,
                    Message    => Zeroes(1 .. Padding_Length));
      end if;
   end Init;

   procedure Update(Ctx     : in out Context;
                    Message : in     Byte_Array)
   is
   begin
      XOF.Update(Ctx.XOF_Ctx, Message);
   end Update;

   procedure Update(Ctx        : in out Context;
                    Message    : in     Byte_Array;
                    Bit_Length : in     Natural)
   is
   begin
      XOF.Update(Ctx.XOF_Ctx, Message, Bit_Length);
   end Update;

   procedure Extract(Ctx    : in out Context;
                     Digest :    out Byte_Array)
   is
   begin
      XOF.Extract(Ctx.XOF_Ctx, Digest);
   end Extract;

end Keccak.Generic_CSHAKE;
