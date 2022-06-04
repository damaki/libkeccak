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
--  Subprograms for operating on Keccak-f states with a line size greater than
--  or equal to 8 bits.
--
--  @group Keccak-f
generic
   --  Bit-wise left shift for Lane_Type.
   with function Shift_Left (Value  : in Lane_Type;
                             Amount : in Natural) return Lane_Type;

   --  Bit-wise right shift for Lane_Type.
   with function Shift_Right (Value  : in Lane_Type;
                              Amount : in Natural) return Lane_Type;
package Keccak.Generic_KeccakF.Byte_Lanes
is

   pragma Assert
     (Lane_Size_Bits mod 8 = 0,
      "Byte_Lanes can only be used with lane sizes that are a multiple of 8");

   procedure XOR_Bits_Into_State (A       : in out State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Global => null,
     Depends => (A =>+ (Data, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then (Bit_Len + 7) / 8 <= Data'Length);
   --  XOR bits into the Keccak-f state.
   --
   --  The data is XOR'ed into the first Bit_Len bits of the state.
   --
   --     |<-------->| Bit_Len
   --     +----------+
   --     |          | Data
   --     +----------+
   --     |    XOR   |
   --     V          V
   --     +----------------------+
   --     |                      | Keccak State
   --     +----------------------+
   --
   --  @param A The Keccak-f state which is XORed with the Data.
   --
   --  @param Data Byte array containing the data to XOR into the state.
   --
   --  @param Bit_Len The number of bits to XOR into the Keccak-f state. This
   --     value cannot be larger than the bit-length of the 'Data' array, and
   --     cannot be larger than the Keccak-f state size.

   procedure XOR_Bits_Into_State (A       : in out Lane_Complemented_State;
                                  Data    : in     Keccak.Types.Byte_Array;
                                  Bit_Len : in     Natural)
     with Inline,
     Global => null,
     Depends => (A =>+ (Data, Bit_Len)),
     Pre => (Bit_Len <= State_Size_Bits
             and then (Bit_Len + 7) / 8 <= Data'Length);
   --  XOR bits into the lane complemented version of the Keccak-f state.
   --
   --  The data is XOR'ed into the first Bit_Len bits of the state.
   --
   --     |<-------->| Bit_Len
   --     +----------+
   --     |          | Data
   --     +----------+
   --     |    XOR   |
   --     V          V
   --     +----------------------+
   --     |                      | Keccak State
   --     +----------------------+
   --
   --  @param A The Keccak-f state which is XORed with the Data.
   --
   --  @param Data Byte array containing the data to XOR into the state.
   --
   --  @param Bit_Len The number of bits to XOR into the Keccak-f state. This
   --     value cannot be larger than the bit-length of the 'Data' array, and
   --     cannot be larger than the Keccak-f state size.

   procedure XOR_Byte_Into_State (A       : in out State;
                                  Offset  : in     Natural;
                                  Value   : in     Keccak.Types.Byte)
     with Global => null,
     Depends => (A =>+ (Offset, Value)),
     Pre => Offset < (State_Size_Bits + 7) / 8;

   procedure XOR_Byte_Into_State (A       : in out Lane_Complemented_State;
                                  Offset  : in     Natural;
                                  Value   : in     Keccak.Types.Byte)
     with Global => null,
     Depends => (A =>+ (Offset, Value)),
     Pre => Offset < (State_Size_Bits + 7) / 8;

   procedure Extract_Bytes (A    : in     State;
                            Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ A),
     Pre => Data'Length <= ((State_Size_Bits + 7) / 8);
   --  Copy bytes from the Keccak-f state.
   --
   --  The bytes are extracted starting at the beginning of the Keccak state.
   --
   --     +----------------------+
   --     |                      | Keccak State
   --     +----------------------+
   --     |          |
   --     V          V
   --     +----------+
   --     |          | Data
   --     +----------+
   --
   --  @param A The Keccak-f state to read.
   --
   --  @param Data The bytes from the Keccak-f state are copied to this buffer.
   --    Note that the buffer can be smaller than the state size if fewer bytes
   --    are needed.
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "Data is fully initialized via a loop");

   procedure Extract_Bytes (A    : in     Lane_Complemented_State;
                            Data :    out Keccak.Types.Byte_Array)
     with Global => null,
     Depends => (Data =>+ A),
     Pre => Data'Length <= ((State_Size_Bits + 7) / 8);
   --  Copy bytes from the lane complemented Keccak-f state.
   --
   --  The bytes are extracted starting at the beginning of the Keccak state.
   --
   --     +----------------------+
   --     |                      | Keccak State
   --     +----------------------+
   --     |          |
   --     V          V
   --     +----------+
   --     |          | Data
   --     +----------+
   --
   --  @param A The Keccak-f state to read.
   --
   --  @param Data The bytes from the Keccak-f state are copied to this buffer.
   --    Note that the buffer can be smaller than the state size if fewer bytes
   --    are needed.
   pragma Annotate
     (GNATprove, False_Positive,
      """Data"" might not be initialized",
      "GNATprove issues a false positive due to the use of loops to initialize Data");

   procedure Extract_Bits (A       : in     State;
                           Data    :    out Keccak.Types.Byte_Array;
                           Bit_Len : in     Natural)
   --  Copy bits from the Keccak-f state.
   --
   --  The bits are extracted starting at the beginning of the Keccak state.
   --
   --     +----------------------+
   --     |                      | Keccak State
   --     +----------------------+
   --     |          |
   --     V          V
   --     +----------+
   --     |          | Data
   --     +----------+
   --     |<-------->| Bit_Len
   --
   --  @param A The Keccak-f state to read.
   --
   --  @param Data The bits from the Keccak-f state are copied to this buffer.
   --    Note that the buffer can be smaller than the state size if fewer bits
   --    are needed.
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
   --  Copy bits from the lane complemented Keccak-f state.
   --
   --  The bits are extracted starting at the beginning of the Keccak state.
   --
   --     +----------------------+
   --     |                      | Keccak State
   --     +----------------------+
   --     |          |
   --     V          V
   --     +----------+
   --     |          | Data
   --     +----------+
   --     |<-------->| Bit_Len
   --
   --  @param A The Keccak-f state to read.
   --
   --  @param Data The bits from the Keccak-f state are copied to this buffer.
   --    Note that the buffer can be smaller than the state size if fewer bits
   --    are needed.

end Keccak.Generic_KeccakF.Byte_Lanes;
