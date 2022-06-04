-------------------------------------------------------------------------------
--  Copyright (c) 2017, Daniel King
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

with Parallel_Sponge_Tests;
with Keccak.Keccak_1600.Rounds_24;
with Keccak.Parallel_Keccak_1600.Rounds_24;
with AUnit.Test_Caller;

package body Parallel_Sponge_Suite
is
   --  For these tests we need to choose values for the Capacity such that the
   --  rate will be a multiple of 8 (required precondition for Sponge.Init).

   --  We use KangarooTwelve's instances of the sponges for testing, as these
   --  are actually used in the library.

   package Parallel_Sponge_1600_P2_Tests is new Parallel_Sponge_Tests
     (Serial_Sponge   => Keccak.Keccak_1600.Rounds_24.Sponge,
      Parallel_Sponge => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P2,
      Capacity        => 256);

   package Parallel_Sponge_1600_P4_Tests is new Parallel_Sponge_Tests
     (Serial_Sponge   => Keccak.Keccak_1600.Rounds_24.Sponge,
      Parallel_Sponge => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P4,
      Capacity        => 256);

   package Parallel_Sponge_1600_P8_Tests is new Parallel_Sponge_Tests
     (Serial_Sponge   => Keccak.Keccak_1600.Rounds_24.Sponge,
      Parallel_Sponge => Keccak.Parallel_Keccak_1600.Rounds_24.Parallel_Sponge_P8,
      Capacity        => 256);

   package Caller_1600_P2 is new AUnit.Test_Caller (Parallel_Sponge_1600_P2_Tests.Test);
   package Caller_1600_P4 is new AUnit.Test_Caller (Parallel_Sponge_1600_P4_Tests.Test);
   package Caller_1600_P8 is new AUnit.Test_Caller (Parallel_Sponge_1600_P8_Tests.Test);

   function Suite return Access_Test_Suite
   is

      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      Ret.Add_Test
        (Caller_1600_P2.Create
           ("Sponge[Keccak-f[1600], pad10*1, 512]�2: " &
              "Parallel sponge output same as serial sponge",
            Parallel_Sponge_1600_P2_Tests.Test_Same_Output_As_Serial'Access));

      Ret.Add_Test
        (Caller_1600_P4.Create
           ("Sponge[Keccak-f[1600], pad10*1, 512]�4: " &
              "Parallel sponge output same as serial sponge",
            Parallel_Sponge_1600_P4_Tests.Test_Same_Output_As_Serial'Access));

      Ret.Add_Test
        (Caller_1600_P8.Create
           ("Sponge[Keccak-f[1600], pad10*1, 512]�8: " &
              "Parallel sponge output same as serial sponge",
            Parallel_Sponge_1600_P8_Tests.Test_Same_Output_As_Serial'Access));

      return Ret;
   end Suite;

end Parallel_Sponge_Suite;
