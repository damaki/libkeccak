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

with Keccak.Types;

generic
   --  Size of the Duplex state in bits (e.g. 1600 for Keccak[1600])
   State_Size_Bits : Positive;

   type State_Type is private;

   with procedure Init_State (A : out State_Type);

   with procedure Permute_Start (A : in out State_Type);
   with procedure Permute_Step (A : in out State_Type);
   with procedure Permute_Stride (A : in out State_Type);

   --  Procedure to XOR bits into the generic state.
   with procedure XOR_Bits_Into_State (A       : in out State_Type;
                                       Data    : in     Keccak.Types.Byte_Array;
                                       Bit_Len : in     Natural);

   with procedure XOR_Byte_Into_State (A      : in out State_Type;
                                       Offset : in Natural;
                                       Value  : in Keccak.Types.Byte);

   with procedure Extract_Bits (A       : in     State_Type;
                                Data    :    out Keccak.Types.Byte_Array;
                                Bit_Len : in     Natural);

   with procedure XOR_Padding_Into_State (A         : in out State_Type;
                                          First_Bit : in     Natural;
                                          Last_Bit  : in     Natural);

   Min_Padding_Bits : Natural;

package Keccak.Generic_MonkeyDuplex is

   subtype Rate_Bits_Number is Positive range Min_Padding_Bits + 1 .. State_Size_Bits - 1;

   type Context is private;

   procedure Start (Ctx      :    out Context;
                    Rate     : in     Rate_Bits_Number;
                    Data     : in     Keccak.Types.Byte_Array;
                    Bit_Len  : in     Natural)
     with Global => null,
     Pre => (Bit_Len <= State_Size_Bits - Min_Padding_Bits
             and then Data'Length >= (Bit_Len + 7) / 8),
     Post => Rate_Of (Ctx) = Rate;

   function Start (Rate     : in     Rate_Bits_Number;
                   Data     : in     Keccak.Types.Byte_Array;
                   Bit_Len  : in     Natural) return Context
     with Global => null,
     Pre => (Bit_Len <= State_Size_Bits - Min_Padding_Bits
             and then Data'Length >= (Bit_Len + 7) / 8),
     Post => Rate_Of (Start'Result) = Rate;

   procedure Step (Ctx                 : in out Context;
                   In_Data             : in     Keccak.Types.Byte_Array;
                   In_Data_Bit_Length  : in     Natural;
                   Suffix              : in     Keccak.Types.Byte;
                   Suffix_Bit_Length   : in     Natural;
                   Out_Data            :    out Keccak.Types.Byte_Array;
                   Out_Data_Bit_Length : in     Natural)
     with Global => null,
     Depends => (Ctx      => (Ctx,
                              In_Data,
                              In_Data_Bit_Length,
                              Suffix,
                              Suffix_Bit_Length),
                 Out_Data => (Ctx,
                              In_Data,
                              In_Data_Bit_Length,
                              Suffix,
                              Suffix_Bit_Length,
                              Out_Data,
                              Out_Data_Bit_Length)),
     Pre => (In_Data_Bit_Length <= Rate_Of (Ctx) - Min_Padding_Bits
             and then Suffix_Bit_Length <= 8
             and then In_Data_Bit_Length + Suffix_Bit_Length <= Rate_Of (Ctx) - Min_Padding_Bits
             and then In_Data'Length >= (In_Data_Bit_Length + 7) / 8
             and then Out_Data_Bit_Length <= Rate_Of (Ctx)
             and then Out_Data'Length = (Out_Data_Bit_Length + 7) / 8),
     Post => (Rate_Of (Ctx) = Rate_Of (Ctx'Old));

   procedure Step_Mute (Ctx                 : in out Context;
                        In_Data             : in     Keccak.Types.Byte_Array;
                        In_Data_Bit_Length  : in     Natural;
                        Suffix              : in     Keccak.Types.Byte;
                        Suffix_Bit_Length   : in     Natural)
     with Global => null,
     Depends => (Ctx      => (Ctx,
                              In_Data,
                              In_Data_Bit_Length,
                              Suffix,
                              Suffix_Bit_Length)),
     Pre => (In_Data_Bit_Length <= Rate_Of (Ctx) - Min_Padding_Bits
             and then Suffix_Bit_Length <= 8
             and then In_Data_Bit_Length + Suffix_Bit_Length <= Rate_Of (Ctx) - Min_Padding_Bits
             and then In_Data'Length >= (In_Data_Bit_Length + 7) / 8),
     Post => (Rate_Of (Ctx) = Rate_Of (Ctx'Old));

   procedure Stride (Ctx                 : in out Context;
                     In_Data             : in     Keccak.Types.Byte_Array;
                     In_Data_Bit_Length  : in     Natural;
                     Suffix              : in     Keccak.Types.Byte;
                     Suffix_Bit_Length   : in     Natural;
                     Out_Data            :    out Keccak.Types.Byte_Array;
                     Out_Data_Bit_Length : in     Natural)
     with Global => null,
     Depends => (Ctx      => (Ctx,
                              In_Data,
                              In_Data_Bit_Length,
                              Suffix,
                              Suffix_Bit_Length),
                 Out_Data => (Ctx,
                              In_Data,
                              In_Data_Bit_Length,
                              Suffix,
                              Suffix_Bit_Length,
                              Out_Data,
                              Out_Data_Bit_Length)),
     Pre => (In_Data_Bit_Length <= Rate_Of (Ctx) - Min_Padding_Bits
             and then Suffix_Bit_Length <= 8
             and then In_Data_Bit_Length + Suffix_Bit_Length <= Rate_Of (Ctx) - Min_Padding_Bits
             and then In_Data'Length >= (In_Data_Bit_Length + 7) / 8
             and then Out_Data_Bit_Length <= Rate_Of (Ctx)
             and then Out_Data'Length = (Out_Data_Bit_Length + 7) / 8),
     Post => (Rate_Of (Ctx) = Rate_Of (Ctx'Old));

   function Rate_Of (Ctx : in Context) return Rate_Bits_Number
     with Inline,
     Global => null;

private

   type Context is record
      State : State_Type;
      Rate  : Rate_Bits_Number;
   end record
     with Predicate => Rate > Min_Padding_Bits;

   function Rate_Of (Ctx : in Context) return Rate_Bits_Number is
     (Ctx.Rate);

end Keccak.Generic_MonkeyDuplex;
