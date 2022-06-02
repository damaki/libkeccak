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

--  @summary
--  Subprograms for operating on Keccak-f states with a line size less than 8 bits.
--
--  @group Keccak-f
generic
package Keccak.Generic_KeccakF.Bit_Lanes
is

   pragma Assert
     (Lane_Size_Bits in 1 | 2 | 4,
      "Bit_Lanes can only be used with lane sizes that 1, 2, or 4 bits wide");

   procedure XOR_Bits_Into_State (A       : in out State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Global => null,
     Depends => (A =>+ (Data, Bit_Len)),
     Pre => (Data'Length <= Natural'Last / 8
             and then Bit_Len <= Data'Length * 8
             and then Bit_Len <= State_Size_Bits);

   procedure XOR_Bits_Into_State (A       : in out Lane_Complemented_State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Inline,
     Global => null,
     Depends => (A =>+ (Data, Bit_Len)),
     Pre => (Data'Length <= Natural'Last / 8
             and then Bit_Len <= Data'Length * 8
             and then Bit_Len <= State_Size_Bits);

   procedure Extract_Bytes (A    : in     State;
                            Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ A),
     Pre => Data'Length <= ((State_Size_Bits + 7) / 8);

   procedure Extract_Bytes (A    : in     Lane_Complemented_State;
                            Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ A),
     Pre => Data'Length <= ((State_Size_Bits + 7) / 8);

   procedure Extract_Bits (A       : in     State;
                           Data    :    out Keccak.Types.Byte_Array;
                           Bit_Len : in     Natural)
     with Global => null,
     Depends => (Data =>+ (A, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);

   procedure Extract_Bits (A       : in     Lane_Complemented_State;
                           Data    :    out Keccak.Types.Byte_Array;
                           Bit_Len : in     Natural)
     with Global => null,
     Depends => (Data =>+ (A, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);

end Keccak.Generic_KeccakF.Bit_Lanes;
