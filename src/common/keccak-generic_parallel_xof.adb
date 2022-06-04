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
package body Keccak.Generic_Parallel_XOF
is

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx : out Context)
   is
   begin
      XOF_Sponge.Init (Ctx.Sponge_Ctx);
   end Init;

   -----------------------
   --  Update_Separate  --
   -----------------------

   procedure Update_Separate (Ctx  : in out Context;
                              Data : in     Types.Byte_Array)
   is
      Block_Size : constant Natural := (Data'Length / Num_Parallel_Instances);
   begin
      pragma Assume (Rate = XOF_Sponge.Rate_Of (Ctx.Sponge_Ctx));

      if Block_Size mod (Rate / 8) = 0 then
         XOF_Sponge.Absorb_Bytes_Separate (Ctx.Sponge_Ctx, Data);

      else
         XOF_Sponge.Absorb_Bytes_Separate_With_Suffix
           (Ctx        => Ctx.Sponge_Ctx,
            Data       => Data,
            Suffix     => Suffix,
            Suffix_Len => Suffix_Size);
      end if;
   end Update_Separate;

   ------------------
   --  Update_All  --
   ------------------

   procedure Update_All (Ctx        : in out Context;
                         Data       : in     Types.Byte_Array)
   is
   begin
      pragma Assume (Rate = XOF_Sponge.Rate_Of (Ctx.Sponge_Ctx));

      if Data'Length mod (Rate / 8) = 0 then
         XOF_Sponge.Absorb_Bytes_All (Ctx.Sponge_Ctx, Data);

      else
         XOF_Sponge.Absorb_Bytes_All_With_Suffix
           (Ctx        => Ctx.Sponge_Ctx,
            Data       => Data,
            Suffix     => Suffix,
            Suffix_Len => Suffix_Size);
      end if;
   end Update_All;

   ------------------------
   --  Extract_Separate  --
   ------------------------

   procedure Extract_Separate (Ctx  : in out Context;
                               Data :    out Types.Byte_Array)
   is
      Empty_Array : constant Types.Byte_Array (1 .. 0) := (others => 0);

   begin
      if State_Of (Ctx) = Updating then
         XOF_Sponge.Absorb_Bytes_Separate_With_Suffix
           (Ctx        => Ctx.Sponge_Ctx,
            Data       => Empty_Array,
            Suffix     => Suffix,
            Suffix_Len => Suffix_Size);
      end if;

      XOF_Sponge.Squeeze_Bytes_Separate (Ctx.Sponge_Ctx, Data);
   end Extract_Separate;

end Keccak.Generic_Parallel_XOF;
