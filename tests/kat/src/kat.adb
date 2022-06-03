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

with Ada.Command_Line;
with Ada.Text_IO;

with CSHAKE_Runner;
with Duplex_Runner;
with Hash_Runner;
with MonkeyWrap_Runner;
with KMAC_Runner;
with ParallelHash_Runner;
with TupleHash_Runner;
with XOF_Runner;

with CSHAKE;
with Keccak.Keccak_1600.Rounds_24;
with Ketje;
with KMAC;
with Parallel_Hash;
with SHA3;
with SHAKE;
with Tuple_Hash;
with Gimli.Hash;
with Ascon.Hash;
with Ascon.XOF;

procedure KAT
is
   use type Ada.Command_Line.Exit_Status;

   package SHA3_224_Runner is new Hash_Runner (SHA3.SHA3_224);
   package SHA3_256_Runner is new Hash_Runner (SHA3.SHA3_256);
   package SHA3_384_Runner is new Hash_Runner (SHA3.SHA3_384);
   package SHA3_512_Runner is new Hash_Runner (SHA3.SHA3_512);

   package SHAKE_128_Runner is new XOF_Runner (SHAKE.SHAKE128);
   package SHAKE_256_Runner is new XOF_Runner (SHAKE.SHAKE256);

   package Keccak_224_Runner is new Hash_Runner (SHA3.Keccak_224);
   package Keccak_256_Runner is new Hash_Runner (SHA3.Keccak_256);
   package Keccak_384_Runner is new Hash_Runner (SHA3.Keccak_384);
   package Keccak_512_Runner is new Hash_Runner (SHA3.Keccak_512);

   package CSHAKE128_Runner is new CSHAKE_Runner (CSHAKE.CSHAKE128);
   package CSHAKE256_Runner is new CSHAKE_Runner (CSHAKE.CSHAKE256);

   package Duplex_1600_24_Runner is new Duplex_Runner (Keccak.Keccak_1600.Rounds_24.Duplex);

   package KMAC128_Runner is new KMAC_Runner (KMAC.KMAC128);
   package KMAC256_Runner is new KMAC_Runner (KMAC.KMAC256);

   package ParallelHash128_Runner is new ParallelHash_Runner (Parallel_Hash.ParallelHash128);
   package ParallelHash256_Runner is new ParallelHash_Runner (Parallel_Hash.ParallelHash256);

   package TupleHash128_Runner is new TupleHash_Runner (Tuple_Hash.Tuple_Hash_128);
   package TupleHash256_Runner is new TupleHash_Runner (Tuple_Hash.Tuple_Hash_256);

   package KetjeJr_Runner is new MonkeyWrap_Runner (Ketje.Jr);
   package KetjeSr_Runner is new MonkeyWrap_Runner (Ketje.Sr);
   package KetjeMinor_Runner is new MonkeyWrap_Runner (Ketje.Minor);
   package KetjeMajor_Runner is new MonkeyWrap_Runner (Ketje.Major);

   package GimliHash_Runner is new Hash_Runner (Gimli.Hash);

   package AsconHash_Runner is new Hash_Runner (Ascon.Hash);
   package AsconXOF_Runner is new XOF_Runner (Ascon.XOF);

   package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);

   Num_Passed : Natural;
   Num_Failed : Natural;

   Success : Boolean := True;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("usage: " & Ada.Command_Line.Command_Name & " ALGO FILE");

      Ada.Command_Line.Set_Exit_Status (-1);
   else

      declare
         Algo : constant String := Ada.Command_Line.Argument (1);

      begin

         if Algo = "SHA3-224" then
            SHA3_224_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Align_Bits => False,
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         elsif Algo = "SHA3-256" then
            SHA3_256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Align_Bits => False,
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         elsif Algo = "SHA3-384" then
            SHA3_384_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Align_Bits => False,
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         elsif Algo = "SHA3-512" then
            SHA3_512_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Align_Bits => False,
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         elsif Algo = "SHAKE128" then
            SHAKE_128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         elsif Algo = "SHAKE256" then
            SHAKE_256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         elsif Algo = "Keccak-224" then
            Keccak_224_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                         Align_Bits => True,
                                         Num_Passed => Num_Passed,
                                         Num_Failed => Num_Failed);
         elsif Algo = "Keccak-256" then
            Keccak_256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                         Align_Bits => True,
                                         Num_Passed => Num_Passed,
                                         Num_Failed => Num_Failed);
         elsif Algo = "Keccak-384" then
            Keccak_384_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                         Align_Bits => True,
                                         Num_Passed => Num_Passed,
                                         Num_Failed => Num_Failed);
         elsif Algo = "Keccak-512" then
            Keccak_512_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                         Align_Bits => True,
                                         Num_Passed => Num_Passed,
                                         Num_Failed => Num_Failed);
         elsif Algo = "cSHAKE128" then
            CSHAKE128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                        Num_Passed => Num_Passed,
                                        Num_Failed => Num_Failed);
         elsif Algo = "cSHAKE256" then
            CSHAKE256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                        Num_Passed => Num_Passed,
                                        Num_Failed => Num_Failed);
         elsif Algo = "Duplexc574" then
            Duplex_1600_24_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                             Capacity   => 574,
                                             Num_Passed => Num_Passed,
                                             Num_Failed => Num_Failed);
         elsif Algo = "Duplexc573" then
            Duplex_1600_24_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                             Capacity   => 573,
                                             Num_Passed => Num_Passed,
                                             Num_Failed => Num_Failed);
         elsif Algo = "KMAC128" then
            KMAC128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                      XOF        => False,
                                      Num_Passed => Num_Passed,
                                      Num_Failed => Num_Failed);
         elsif Algo = "KMAC256" then
            KMAC256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                      XOF        => False,
                                      Num_Passed => Num_Passed,
                                      Num_Failed => Num_Failed);
         elsif Algo = "KMACXOF128" then
            KMAC128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                      XOF        => True,
                                      Num_Passed => Num_Passed,
                                      Num_Failed => Num_Failed);
         elsif Algo = "KMACXOF256" then
            KMAC256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                      XOF        => True,
                                      Num_Passed => Num_Passed,
                                      Num_Failed => Num_Failed);
         elsif Algo = "ParallelHash128" then
            ParallelHash128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                              XOF        => False,
                                              Num_Passed => Num_Passed,
                                              Num_Failed => Num_Failed);
         elsif Algo = "ParallelHash256" then
            ParallelHash256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                              XOF        => False,
                                              Num_Passed => Num_Passed,
                                              Num_Failed => Num_Failed);
         elsif Algo = "ParallelHashXOF128" then
            ParallelHash128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                              XOF        => True,
                                              Num_Passed => Num_Passed,
                                              Num_Failed => Num_Failed);
         elsif Algo = "ParallelHashXOF256" then
            ParallelHash256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                              XOF        => True,
                                              Num_Passed => Num_Passed,
                                              Num_Failed => Num_Failed);
         elsif Algo = "TupleHash128" then
            TupleHash128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                           XOF        => False,
                                           Num_Passed => Num_Passed,
                                           Num_Failed => Num_Failed);
         elsif Algo = "TupleHash256" then
            TupleHash256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                           XOF        => False,
                                           Num_Passed => Num_Passed,
                                           Num_Failed => Num_Failed);
         elsif Algo = "TupleHashXOF128" then
            TupleHash128_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                           XOF        => True,
                                           Num_Passed => Num_Passed,
                                           Num_Failed => Num_Failed);
         elsif Algo = "TupleHashXOF256" then
            TupleHash256_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                           XOF        => True,
                                           Num_Passed => Num_Passed,
                                           Num_Failed => Num_Failed);
         elsif Algo = "KetjeJr" then
            KetjeJr_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                      Num_Passed => Num_Passed,
                                      Num_Failed => Num_Failed);
         elsif Algo = "KetjeSr" then
            KetjeSr_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                      Num_Passed => Num_Passed,
                                      Num_Failed => Num_Failed);
         elsif Algo = "KetjeMinor" then
            KetjeMinor_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                         Num_Passed => Num_Passed,
                                         Num_Failed => Num_Failed);
         elsif Algo = "KetjeMajor" then
            KetjeMajor_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                         Num_Passed => Num_Passed,
                                         Num_Failed => Num_Failed);
         elsif Algo = "GimliHash" then
            GimliHash_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                        Align_Bits => False,
                                        Num_Passed => Num_Passed,
                                        Num_Failed => Num_Failed);
         elsif Algo = "AsconHash" then
            AsconHash_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                        Align_Bits => False,
                                        Num_Passed => Num_Passed,
                                        Num_Failed => Num_Failed);
         elsif Algo = "AsconXOF" then
            AsconXOF_Runner.Run_Tests (File_Name  => Ada.Command_Line.Argument (2),
                                       Num_Passed => Num_Passed,
                                       Num_Failed => Num_Failed);
         else
            Ada.Text_IO.Put_Line ("Unknown algorithm: " & Algo);
            Ada.Command_Line.Set_Exit_Status (-1);

            Success := False;

         end if;

         --  Print results

         if Success then
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put ("Passed: ");
            Integer_IO.Put (Num_Passed, Width => 0);
            Ada.Text_IO.New_Line;

            Ada.Text_IO.Put ("Failed: ");
            Integer_IO.Put (Num_Failed, Width => 0);
            Ada.Text_IO.New_Line;

            if Num_Failed > 0 then
               Ada.Command_Line.Set_Exit_Status (1);
            end if;
         end if;
      end;

   end if;

end KAT;
