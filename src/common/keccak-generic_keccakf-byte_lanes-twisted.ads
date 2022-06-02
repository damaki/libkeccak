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

--  @summary
--  Variants for twisted Keccak-p permutations.
--
--  @group Keccak-f
generic
   --  Bit-wise left shift for Lane_Type.
   with function Shift_Left (Value  : in Lane_Type;
                             Amount : in Natural) return Lane_Type;

   --  Bit-wise right shift for Lane_Type.
   with function Shift_Right (Value  : in Lane_Type;
                              Amount : in Natural) return Lane_Type;
package Keccak.Generic_KeccakF.Byte_Lanes.Twisted is

   procedure XOR_Bits_Into_State_Twisted (A       : in out State;
                                          Data    : in     Keccak.Types.Byte_Array;
                                          Bit_Len : in     Natural)
     with Global => null,
     Depends => (A =>+ (Data, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then (Bit_Len + 7) / 8 <= Data'Length);
   --  Version of XOR_Bits_Into_State for Twisted Keccak-p

   procedure XOR_Bits_Into_State_Twisted (A       : in out Lane_Complemented_State;
                                          Data    : in     Keccak.Types.Byte_Array;
                                          Bit_Len : in     Natural)
     with Inline,
     Global => null,
     Depends => (A =>+ (Data, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then (Bit_Len + 7) / 8 <= Data'Length);
   --  Version of XOR_Bits_Into_State for Twisted Keccak-p

   procedure XOR_Byte_Into_State_Twisted (A       : in out State;
                                          Offset  : in     Natural;
                                          Value   : in     Keccak.Types.Byte)
     with Global => null,
     Depends => (A =>+ (Offset, Value)),
     Pre => Offset < (State_Size_Bits + 7) / 8;

   procedure XOR_Byte_Into_State_Twisted (A       : in out Lane_Complemented_State;
                                          Offset  : in     Natural;
                                          Value   : in     Keccak.Types.Byte)
     with Global => null,
     Depends => (A =>+ (Offset, Value)),
     Pre => Offset < (State_Size_Bits + 7) / 8;

   procedure Extract_Bytes_Twisted (A    : in     State;
                                    Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ A),
     Pre => Data'Length <= ((State_Size_Bits + 7) / 8);
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "GNATprove issues a false positive due to the use of loops to initialize Data");
   --  Twisted version of Extract_Bytes

   procedure Extract_Bytes_Twisted (A    : in     Lane_Complemented_State;
                                    Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ A),
     Pre => Data'Length <= ((State_Size_Bits + 7) / 8);
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "GNATprove issues a false positive due to the use of loops to initialize Data");
   --  Twisted version of Extract_Bytes

   procedure Extract_Bits_Twisted (A       : in     State;
                                   Data    :    out Keccak.Types.Byte_Array;
                                   Bit_Len : in     Natural)
     with Global => null,
     Depends => (Data =>+ (A, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);
   --  Twisted version of Extract_Bits

   procedure Extract_Bits_Twisted (A       : in     Lane_Complemented_State;
                                   Data    :    out Keccak.Types.Byte_Array;
                                   Bit_Len : in     Natural)
     with Global => null,
     Depends => (Data =>+ (A, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then Data'Length = (Bit_Len + 7) / 8);
   --  Twisted version of Extract_Bits

end Keccak.Generic_KeccakF.Byte_Lanes.Twisted;
