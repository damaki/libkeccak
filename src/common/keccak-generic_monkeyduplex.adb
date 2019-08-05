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

with Interfaces; use Interfaces;

package body Keccak.Generic_MonkeyDuplex is

   -------------
   --  Start  --
   -------------

   procedure Start (Ctx      :    out Context;
                    Rate     : in     Rate_Bits_Number;
                    Data     : in     Keccak.Types.Byte_Array;
                    Bit_Len  : in     Natural) is
   begin
      Ctx.Rate := Rate;
      Init_State (Ctx.State);

      XOR_Bits_Into_State (A       => Ctx.State,
                           Data    => Data,
                           Bit_Len => Bit_Len);

      XOR_Padding_Into_State (A         => Ctx.State,
                              First_Bit => Bit_Len,
                              Last_Bit  => State_Size_Bits - 1);

      Permute_Start (Ctx.State);
   end Start;

   -------------
   --  Start  --
   -------------

   function Start (Rate     : in     Rate_Bits_Number;
                   Data     : in     Keccak.Types.Byte_Array;
                   Bit_Len  : in     Natural) return Context is
   begin
      return Ctx : Context do
         Start (Ctx, Rate, Data, Bit_Len);
      end return;
   end Start;

   ------------
   --  Step  --
   ------------

   procedure Step (Ctx                 : in out Context;
                   In_Data             : in     Keccak.Types.Byte_Array;
                   In_Data_Bit_Length  : in     Natural;
                   Suffix              : in     Keccak.Types.Byte;
                   Suffix_Bit_Length   : in     Natural;
                   Out_Data            :    out Keccak.Types.Byte_Array;
                   Out_Data_Bit_Length : in     Natural) is
   begin
      Step_Mute (Ctx                => Ctx,
                 In_Data            => In_Data,
                 In_Data_Bit_Length => In_Data_Bit_Length,
                 Suffix             => Suffix,
                 Suffix_Bit_Length  => Suffix_Bit_Length);

      Extract_Bits (A       => Ctx.State,
                    Data    => Out_Data,
                    Bit_Len => Out_Data_Bit_Length);
   end Step;

   -----------------
   --  Step_Mute  --
   -----------------

   procedure Step_Mute (Ctx                 : in out Context;
                        In_Data             : in     Keccak.Types.Byte_Array;
                        In_Data_Bit_Length  : in     Natural;
                        Suffix              : in     Keccak.Types.Byte;
                        Suffix_Bit_Length   : in     Natural) is

      First_Suffix_Byte : constant Natural := In_Data_Bit_Length / 8;
      Last_Suffix_Byte  : constant Natural := (In_Data_Bit_Length + Suffix_Bit_Length - 1) / 8;

   begin
      XOR_Bits_Into_State (Ctx.State, In_Data, In_Data_Bit_Length);

      XOR_Byte_Into_State (A      => Ctx.State,
                           Offset => First_Suffix_Byte,
                           Value  => Shift_Left (Suffix, In_Data_Bit_Length mod 8));

      if Last_Suffix_Byte /= First_Suffix_Byte then
         XOR_Byte_Into_State (A      => Ctx.State,
                              Offset => Last_Suffix_Byte,
                              Value  => Shift_Right (Suffix, 8 - (In_Data_Bit_Length mod 8)));
      end if;

      XOR_Padding_Into_State (A         => Ctx.State,
                              First_Bit => In_Data_Bit_Length + Suffix_Bit_Length,
                              Last_Bit  => Rate_Of (Ctx) - 1);

      Permute_Step (Ctx.State);

   end Step_Mute;

   --------------
   --  Stride  --
   --------------

   procedure Stride (Ctx                 : in out Context;
                     In_Data             : in     Keccak.Types.Byte_Array;
                     In_Data_Bit_Length  : in     Natural;
                     Suffix              : in     Keccak.Types.Byte;
                     Suffix_Bit_Length   : in     Natural;
                     Out_Data            :    out Keccak.Types.Byte_Array;
                     Out_Data_Bit_Length : in     Natural) is

      First_Suffix_Byte : constant Natural := In_Data_Bit_Length / 8;
      Last_Suffix_Byte  : constant Natural := (In_Data_Bit_Length + Suffix_Bit_Length - 1) / 8;

   begin
      XOR_Bits_Into_State (Ctx.State, In_Data, In_Data_Bit_Length);

      XOR_Byte_Into_State (A      => Ctx.State,
                           Offset => First_Suffix_Byte,
                           Value  => Shift_Left (Suffix, In_Data_Bit_Length mod 8));

      if Last_Suffix_Byte /= First_Suffix_Byte then
         XOR_Byte_Into_State (A      => Ctx.State,
                              Offset => Last_Suffix_Byte,
                              Value  => Shift_Right (Suffix, 8 - (In_Data_Bit_Length mod 8)));
      end if;

      XOR_Padding_Into_State (A         => Ctx.State,
                              First_Bit => In_Data_Bit_Length + Suffix_Bit_Length,
                              Last_Bit  => Rate_Of (Ctx) - 1);

      Permute_Stride (Ctx.State);

      Extract_Bits (A       => Ctx.State,
                    Data    => Out_Data,
                    Bit_Len => Out_Data_Bit_Length);
   end Stride;

end Keccak.Generic_MonkeyDuplex;
