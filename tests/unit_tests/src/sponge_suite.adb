-------------------------------------------------------------------------------
--  Copyright (c) 2016, Daniel King
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

with Sponge_Tests;
with Keccak.Keccak_1600.Rounds_24;
with Keccak.Keccak_800.Rounds_22;
with Keccak.Keccak_400.Rounds_20;
with Keccak.Keccak_200.Rounds_18;
with Keccak.Keccak_100.Rounds_16;
with Keccak.Keccak_50.Rounds_14;
with Keccak.Keccak_25.Rounds_12;
with Gimli.Sponge;
with AUnit.Test_Caller;

package body Sponge_Suite
is
   --  For these tests we need to choose values for the Capacity such that the
   --  rate will be a multiple of 8 (required precondition for Sponge.Init).

   package Sponge_1600_Tests is new Sponge_Tests
      (Keccak.Keccak_1600.Rounds_24.Sponge, 512);

   package Sponge_800_Tests is new Sponge_Tests
      (Keccak.Keccak_800.Rounds_22.Sponge, 512);

   package Sponge_400_Tests is new Sponge_Tests
      (Keccak.Keccak_400.Rounds_20.Sponge, 256);

   package Sponge_200_Tests is new Sponge_Tests
      (Keccak.Keccak_200.Rounds_18.Sponge, 128);

   package Sponge_100_Tests is new Sponge_Tests
      (Keccak.Keccak_100.Rounds_16.Sponge, 60);

   package Sponge_50_Tests is new Sponge_Tests
      (Keccak.Keccak_50.Rounds_14.Sponge, 26);

   package Sponge_25_Tests is new Sponge_Tests
      (Keccak.Keccak_25.Rounds_12.Sponge, 9);

   package Sponge_Gimli_Tests is new Sponge_Tests
      (Gimli.Sponge, 256);

   package Caller_1600 is new AUnit.Test_Caller (Sponge_1600_Tests.Test);
   package Caller_800  is new AUnit.Test_Caller (Sponge_800_Tests.Test);
   package Caller_400  is new AUnit.Test_Caller (Sponge_400_Tests.Test);
   package Caller_200  is new AUnit.Test_Caller (Sponge_200_Tests.Test);
   package Caller_100  is new AUnit.Test_Caller (Sponge_100_Tests.Test);
   package Caller_50   is new AUnit.Test_Caller (Sponge_50_Tests.Test);
   package Caller_25   is new AUnit.Test_Caller (Sponge_25_Tests.Test);
   package Caller_Gimli is new AUnit.Test_Caller (Sponge_Gimli_Tests.Test);

   function Suite return Access_Test_Suite
   is

      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: Input streaming test",
             Sponge_1600_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: Output streaming test",
             Sponge_1600_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_1600_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_1600_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: Test absorbing 0 bits",
             Sponge_1600_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: Test absorbing only suffix bits",
             Sponge_1600_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_1600.Create
            ("Sponge[Keccak-f[1600], pad10*1, 512]: Test suffix bits packing",
             Sponge_1600_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: Input streaming test",
             Sponge_800_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: Output streaming test",
             Sponge_800_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_800_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_800_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: Test absorbing 0 bits",
             Sponge_800_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: Test absorbing only suffix bits",
             Sponge_800_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_800.Create
            ("Sponge[Keccak-f[800], pad10*1, 512]: Test suffix bits packing",
             Sponge_800_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: Input streaming test",
             Sponge_400_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: Output streaming test",
             Sponge_400_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_400_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_400_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: Test absorbing 0 bits",
             Sponge_400_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: Test absorbing only suffix bits",
             Sponge_400_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_400.Create
            ("Sponge[Keccak-f[400], pad10*1, 256]: Test suffix bits packing",
             Sponge_400_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: Input streaming test",
             Sponge_200_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: Output streaming test",
             Sponge_200_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_200_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_200_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: Test absorbing 0 bits",
             Sponge_200_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: Test absorbing only suffix bits",
             Sponge_200_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_200.Create
            ("Sponge[Keccak-f[200], pad10*1, 128]: Test suffix bits packing",
             Sponge_200_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: Input streaming test",
             Sponge_100_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: Output streaming test",
             Sponge_100_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_100_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_100_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: Test absorbing 0 bits",
             Sponge_100_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: Test absorbing only suffix bits",
             Sponge_100_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_100.Create
            ("Sponge[Keccak-f[100], pad10*1, 60]: Test suffix bits packing",
             Sponge_100_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: Input streaming test",
             Sponge_50_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: Output streaming test",
             Sponge_50_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_50_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_50_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: Test absorbing 0 bits",
             Sponge_50_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: Test absorbing only suffix bits",
             Sponge_50_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_50.Create
            ("Sponge[Keccak-f[50], pad10*1, 26]: Test suffix bits packing",
             Sponge_50_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: Input streaming test",
             Sponge_25_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: Output streaming test",
             Sponge_25_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_25_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_25_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: Test absorbing 0 bits",
             Sponge_25_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: Test absorbing only suffix bits",
             Sponge_25_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_25.Create
            ("Sponge[Keccak-f[25], pad10*1, 9]: Test suffix bits packing",
             Sponge_25_Tests.Test_Suffix_Packing'Access));

      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: Input streaming test",
             Sponge_Gimli_Tests.Test_Absorb_Streaming'Access));
      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: Output streaming test",
             Sponge_Gimli_Tests.Test_Squeeze_Streaming'Access));
      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: " &
               "Absorb same as Absorb_With_Suffix with 0 suffix bits",
             Sponge_Gimli_Tests.Test_Absorb_No_Suffix'Access));
      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: " &
               "Absorb_With_Suffix is same as Absorb with suffix bits in message",
             Sponge_Gimli_Tests.Test_Suffix_Bits'Access));
      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: Test absorbing 0 bits",
             Sponge_Gimli_Tests.Test_Null_Absorb'Access));
      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: Test absorbing only suffix bits",
             Sponge_Gimli_Tests.Test_Absorb_Suffix_Only'Access));
      Ret.Add_Test
         (Caller_Gimli.Create
            ("Sponge[Gimli, pad10*1, 256]: Test suffix bits packing",
             Sponge_Gimli_Tests.Test_Suffix_Packing'Access));

      return Ret;
   end Suite;

end Sponge_Suite;
