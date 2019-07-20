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

package body Keccak.Generic_KMAC
is

   procedure Init (Ctx           :    out Context;
                   Key           : in     Types.Byte_Array;
                   Customization : in     String)
   is
      Rate_Bytes : constant Positive := Rate / 8;

      Encoded_Key_Length : constant Types.Byte_Array
        := Util.Left_Encode_NIST_Bit_Length (Key'Length);

      Encoded_Rate       : constant Types.Byte_Array
        := Util.Left_Encode_NIST (Rate_Bytes);

      Zeroes : constant Types.Byte_Array (1 .. Rate_Bytes) := (others => 0);

      Padding_Length : Natural;

   begin
      KMAC_CSHAKE.Init (Ctx           => Ctx.CSHAKE_Ctx,
                        Customization => Customization,
                        Function_Name => "KMAC");

      Ctx.Finished := False;

      KMAC_CSHAKE.Update
        (Ctx     => Ctx.CSHAKE_Ctx,
         Message => Encoded_Rate);

      KMAC_CSHAKE.Update
        (Ctx     => Ctx.CSHAKE_Ctx,
         Message => Encoded_Key_Length);
      KMAC_CSHAKE.Update
        (Ctx     => Ctx.CSHAKE_Ctx,
         Message => Key);

      Padding_Length :=
        Encoded_Rate'Length mod Rate_Bytes +
        Encoded_Key_Length'Length mod Rate_Bytes +
        Key'Length mod Rate_Bytes;

      Padding_Length := Rate_Bytes - (Padding_Length mod Rate_Bytes);

      if Padding_Length mod Rate_Bytes /= 0 then
         KMAC_CSHAKE.Update
           (Ctx     => Ctx.CSHAKE_Ctx,
            Message => Zeroes (1 .. Padding_Length));
      end if;
   end Init;

   procedure Update (Ctx     : in out Context;
                     Message : in     Types.Byte_Array)
   is
   begin
      KMAC_CSHAKE.Update (Ctx     => Ctx.CSHAKE_Ctx,
                          Message => Message);
   end Update;

   procedure Finish (Ctx : in out Context;
                     MAC :    out Types.Byte_Array)
   is
   begin
      Ctx.Finished := True;

      --  Encode and process the output length
      KMAC_CSHAKE.Update
        (Ctx     => Ctx.CSHAKE_Ctx,
         Message => Util.Right_Encode_NIST_Bit_Length (MAC'Length));

      KMAC_CSHAKE.Extract
        (Ctx    => Ctx.CSHAKE_Ctx,
         Digest => MAC);
   end Finish;

   procedure Extract (Ctx : in out Context;
                      MAC :    out Types.Byte_Array)
   is
   begin
      if State_Of (Ctx) = Updating then
         KMAC_CSHAKE.Update
           (Ctx     => Ctx.CSHAKE_Ctx,
            Message => Util.Right_Encode_NIST (0));
      end if;

      KMAC_CSHAKE.Extract
        (Ctx    => Ctx.CSHAKE_Ctx,
         Digest => MAC);
   end Extract;

end Keccak.Generic_KMAC;
