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

package body Keccak.Generic_XOF
is

   ------------
   --  Init  --
   ------------

   procedure Init (Ctx : out Context)
   is
   begin
      XOF_Sponge.Init (Ctx.Sponge_Ctx, Capacity, Permutation_Initial_Value);
      Ctx.Update_Complete := False;
   end Init;

   --------------
   --  Update  --
   --------------

   procedure Update (Ctx        : in out Context;
                     Message    : in     Keccak.Types.Byte_Array;
                     Bit_Length : in     Natural)
   is
      Num_Bytes : constant Natural := (Bit_Length + 7) / 8;

   begin
      pragma Assert (Num_Bytes <= Message'Length);

      if Num_Bytes > 0 then
         if Bit_Length mod 8 = 0 then
            XOF_Sponge.Absorb (Ctx.Sponge_Ctx,
                               Message (Message'First ..
                                        Message'First + (Num_Bytes - 1)),
                               Bit_Length);

         else
            XOF_Sponge.Absorb_With_Suffix (Ctx.Sponge_Ctx,
                                           Message (Message'First ..
                                                    Message'First + (Num_Bytes - 1)),
                                           Bit_Length,
                                           Suffix,
                                           Suffix_Size);

            Ctx.Update_Complete := True;
         end if;
      end if;
   end Update;

   --------------
   --  Update  --
   --------------

   procedure Update (Ctx     : in out Context;
                     Message : in     Byte_Array)
   is
      Max_Chunk_Len : constant := (Natural'Last / 8) - 1;

      Remaining     : Natural  := Message'Length;
      Offset        : Natural  := 0;

   begin
      while Remaining >= Max_Chunk_Len loop
         pragma Loop_Variant (Decreases => Remaining);
         pragma Loop_Invariant (Remaining + Offset = Message'Length
                                and State_Of (Ctx) = Updating);

         Update (Ctx,
                 Message (Message'First + Offset .. Message'First + Offset + (Max_Chunk_Len - 1)),
                 Max_Chunk_Len * 8);

         Remaining := Remaining - Max_Chunk_Len;
         Offset    := Offset    + Max_Chunk_Len;
      end loop;

      if Remaining > 0 then
         pragma Assert_And_Cut (Remaining < Natural'Last / 8
                                and Offset + Remaining = Message'Length
                                and State_Of (Ctx) = Updating);

         Update (Ctx,
                 Message (Message'First + Offset .. Message'Last),
                 Remaining * 8);

         pragma Assert (State_Of (Ctx) = Updating);
      end if;
   end Update;

   ---------------
   --  Extract  --
   ---------------

   procedure Extract (Ctx    : in out Context;
                      Digest :    out Byte_Array)
   is
      Empty_Array : constant Keccak.Types.Byte_Array (0 .. -1) := (others => 0);

   begin
      if State_Of (Ctx) = Updating then
         XOF_Sponge.Absorb_With_Suffix (Ctx.Sponge_Ctx,
                                        Empty_Array,
                                        0,
                                        Suffix,
                                        Suffix_Size);
      end if;

      XOF_Sponge.Squeeze (Ctx.Sponge_Ctx, Digest);
   end Extract;

end Keccak.Generic_XOF;
