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

package body Keccak.Generic_Tuple_Hash
is

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx           :    out Context;
                   Customization : in     String := "")
   is
   begin
      Ctx.Finished := False;

      CSHAKE.Init (Ctx           => Ctx.Ctx,
                   Customization => Customization,
                   Function_Name => "TupleHash");
   end Init;

   -------------------------
   --  Update_Tuple_Item  --
   -------------------------

   procedure Update_Tuple_Item (Ctx  : in out Context;
                                Item : in     Byte_Array)
   is
   begin
      CSHAKE.Update (Ctx     => Ctx.Ctx,
                     Message => Left_Encode_NIST_Bit_Length (Item'Length));

      CSHAKE.Update (Ctx     => Ctx.Ctx,
                     Message => Item);
   end Update_Tuple_Item;

   --------------
   --  Finish  --
   --------------

   procedure Finish (Ctx     : in out Context;
                     Digest  :    out Byte_Array)
   is
   begin
      CSHAKE.Update (Ctx     => Ctx.Ctx,
                     Message => Right_Encode_NIST_Bit_Length (Digest'Length));

      CSHAKE.Extract (Ctx    => Ctx.Ctx,
                      Digest => Digest);

      Ctx.Finished := True;
   end Finish;

   ---------------
   --  Extract  --
   ---------------

   procedure Extract (Ctx    : in out Context;
                      Digest :    out Byte_Array)
   is
   begin
      if State_Of (Ctx) = Updating then
         CSHAKE.Update
           (Ctx     => Ctx.Ctx,
            Message => Util.Right_Encode_NIST (0));
      end if;

      CSHAKE.Extract
        (Ctx    => Ctx.Ctx,
         Digest => Digest);
   end Extract;

end Keccak.Generic_Tuple_Hash;
