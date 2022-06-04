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

with Keccak.Generic_MonkeyDuplex;
with Keccak.Generic_MonkeyWrap;
with Keccak.Generic_KeccakF.Byte_Lanes.Twisted;
with Keccak.Keccak_200;
with Keccak.Keccak_200.Rounds_12;
with Keccak.Keccak_400;
with Keccak.Keccak_400.Rounds_12;
with Keccak.Keccak_800;
with Keccak.Keccak_800.Rounds_12;
with Keccak.Keccak_1600;
with Keccak.Keccak_1600.Rounds_12;
with Keccak.Padding;
with Interfaces;

pragma Elaborate_All (Keccak.Generic_MonkeyDuplex);
pragma Elaborate_All (Keccak.Generic_MonkeyWrap);
pragma Elaborate_All (Keccak.Generic_KeccakF.Byte_Lanes.Twisted);

package Ketje
with SPARK_Mode => On
is

   --  @private
   package Implementation is

      use Keccak;

      ----------------
      --  Ketje Jr  --
      ----------------

      procedure Permute_Jr_Step is new Keccak_200.KeccakF_200_Permutation.Permute
        (Num_Rounds  => 1);

      procedure Permute_Jr_Stride is new Keccak_200.KeccakF_200_Permutation.Permute
        (Num_Rounds  => 6);

      package Twisted_Lanes_200 is new Keccak_200.KeccakF_200_Lanes.Twisted
        (Shift_Left  => Interfaces.Shift_Left,
         Shift_Right => Interfaces.Shift_Right);

      procedure XOR_Padding_Into_State is new Keccak.Padding.XOR_Pad101_Into_State
        (State_Size_Bits     => 200,
         State_Type          => Keccak_200.State,
         XOR_Byte_Into_State => Twisted_Lanes_200.XOR_Byte_Into_State_Twisted);

      package MonkeyDuplex_Jr is new Keccak.Generic_MonkeyDuplex
        (State_Size_Bits        => 200,
         State_Type             => Keccak_200.State,
         Init_State             => Keccak_200.KeccakF_200.Init,
         Permute_Start          => Keccak_200.Rounds_12.Permute,
         Permute_Step           => Permute_Jr_Step,
         Permute_Stride         => Permute_Jr_Stride,
         XOR_Bits_Into_State    => Twisted_Lanes_200.XOR_Bits_Into_State_Twisted,
         XOR_Byte_Into_State    => Twisted_Lanes_200.XOR_Byte_Into_State_Twisted,
         Extract_Bits           => Twisted_Lanes_200.Extract_Bits_Twisted,
         XOR_Padding_Into_State => XOR_Padding_Into_State,
         Min_Padding_Bits       => Keccak.Padding.Pad101_Min_Bits);

      ----------------
      --  Ketje Sr  --
      ----------------

      procedure Permute_Sr_Step is new Keccak_400.KeccakF_400_Permutation.Permute
        (Num_Rounds  => 1);

      procedure Permute_Sr_Stride is new Keccak_400.KeccakF_400_Permutation.Permute
        (Num_Rounds  => 6);

      package Twisted_Lanes_400 is new Keccak_400.KeccakF_400_Lanes.Twisted
        (Shift_Left  => Interfaces.Shift_Left,
         Shift_Right => Interfaces.Shift_Right);

      procedure XOR_Padding_Into_State is new Keccak.Padding.XOR_Pad101_Into_State
        (State_Size_Bits     => 400,
         State_Type          => Keccak_400.State,
         XOR_Byte_Into_State => Twisted_Lanes_400.XOR_Byte_Into_State_Twisted);

      package MonkeyDuplex_Sr is new Keccak.Generic_MonkeyDuplex
        (State_Size_Bits        => 400,
         State_Type             => Keccak_400.State,
         Init_State             => Keccak_400.KeccakF_400.Init,
         Permute_Start          => Keccak_400.Rounds_12.Permute,
         Permute_Step           => Permute_Sr_Step,
         Permute_Stride         => Permute_Sr_Stride,
         XOR_Bits_Into_State    => Twisted_Lanes_400.XOR_Bits_Into_State_Twisted,
         XOR_Byte_Into_State    => Twisted_Lanes_400.XOR_Byte_Into_State_Twisted,
         Extract_Bits           => Twisted_Lanes_400.Extract_Bits_Twisted,
         XOR_Padding_Into_State => XOR_Padding_Into_State,
         Min_Padding_Bits       => Keccak.Padding.Pad101_Min_Bits);

      -------------------
      --  Ketje Minor  --
      -------------------

      procedure Permute_Minor_Step is new Keccak_800.KeccakF_800_Permutation.Permute
        (Num_Rounds  => 1);

      procedure Permute_Minor_Stride is new Keccak_800.KeccakF_800_Permutation.Permute
        (Num_Rounds  => 6);

      package Twisted_Lanes_800 is new Keccak_800.KeccakF_800_Lanes.Twisted
        (Shift_Left  => Interfaces.Shift_Left,
         Shift_Right => Interfaces.Shift_Right);

      procedure XOR_Padding_Into_State is new Keccak.Padding.XOR_Pad101_Into_State
        (State_Size_Bits     => 800,
         State_Type          => Keccak_800.State,
         XOR_Byte_Into_State => Twisted_Lanes_800.XOR_Byte_Into_State_Twisted);

      package MonkeyDuplex_Minor is new Keccak.Generic_MonkeyDuplex
        (State_Size_Bits        => 800,
         State_Type             => Keccak_800.State,
         Init_State             => Keccak_800.KeccakF_800.Init,
         Permute_Start          => Keccak_800.Rounds_12.Permute,
         Permute_Step           => Permute_Minor_Step,
         Permute_Stride         => Permute_Minor_Stride,
         XOR_Bits_Into_State    => Twisted_Lanes_800.XOR_Bits_Into_State_Twisted,
         XOR_Byte_Into_State    => Twisted_Lanes_800.XOR_Byte_Into_State_Twisted,
         Extract_Bits           => Twisted_Lanes_800.Extract_Bits_Twisted,
         XOR_Padding_Into_State => XOR_Padding_Into_State,
         Min_Padding_Bits       => Keccak.Padding.Pad101_Min_Bits);

      -------------------
      --  Ketje Major  --
      -------------------

      procedure Permute_Major_Step is new Keccak_1600.KeccakF_1600_Permutation.Permute
        (Num_Rounds  => 1);

      procedure Permute_Major_Stride is new Keccak_1600.KeccakF_1600_Permutation.Permute
        (Num_Rounds  => 6);

      package Twisted_Lanes_1600 is new Keccak_1600.KeccakF_1600_Lanes.Twisted
        (Shift_Left  => Interfaces.Shift_Left,
         Shift_Right => Interfaces.Shift_Right);

      procedure XOR_Padding_Into_State is new Keccak.Padding.XOR_Pad101_Into_State
        (State_Size_Bits     => 1600,
         State_Type          => Keccak_1600.State,
         XOR_Byte_Into_State => Twisted_Lanes_1600.XOR_Byte_Into_State_Twisted);

      package MonkeyDuplex_Major is new Keccak.Generic_MonkeyDuplex
        (State_Size_Bits        => 1600,
         State_Type             => Keccak_1600.State,
         Init_State             => Keccak_1600.KeccakF_1600.Init,
         Permute_Start          => Keccak_1600.Rounds_12.Permute,
         Permute_Step           => Permute_Major_Step,
         Permute_Stride         => Permute_Major_Stride,
         XOR_Bits_Into_State    => Twisted_Lanes_1600.XOR_Bits_Into_State_Twisted,
         XOR_Byte_Into_State    => Twisted_Lanes_1600.XOR_Byte_Into_State_Twisted,
         Extract_Bits           => Twisted_Lanes_1600.Extract_Bits_Twisted,
         XOR_Padding_Into_State => XOR_Padding_Into_State,
         Min_Padding_Bits       => Keccak.Padding.Pad101_Min_Bits);

   end Implementation;

   -----------------------
   --  Ketje Instances  --
   -----------------------

   package Jr is new Keccak.Generic_MonkeyWrap
     (Block_Size_Bytes => 16 / 8,
      MonkeyDuplex     => Implementation.MonkeyDuplex_Jr);

   package Sr is new Keccak.Generic_MonkeyWrap
     (Block_Size_Bytes => 32 / 8,
      MonkeyDuplex     => Implementation.MonkeyDuplex_Sr);

   package Minor is new Keccak.Generic_MonkeyWrap
     (Block_Size_Bytes => 128 / 8,
      MonkeyDuplex     => Implementation.MonkeyDuplex_Minor);

   package Major is new Keccak.Generic_MonkeyWrap
     (Block_Size_Bytes => 256 / 8,
      MonkeyDuplex     => Implementation.MonkeyDuplex_Major);

end Ketje;
