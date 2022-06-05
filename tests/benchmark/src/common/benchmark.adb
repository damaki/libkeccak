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

with Timing;                        use Timing;
with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Float_Text_IO;
with Interfaces;                    use Interfaces;
with KangarooTwelve;
with MarsupilamiFourteen;
with Keccak.Parallel_Keccak_1600;
with Keccak.Parallel_Keccak_1600.Rounds_24;
with Keccak.Parallel_Keccak_1600.Rounds_12;
with Keccak.Generic_KangarooTwelve;
with Keccak.Generic_MonkeyWrap;
with Keccak.Generic_Parallel_Hash;
with Keccak.Keccak_25;
with Keccak.Keccak_25.Rounds_12;
with Keccak.Keccak_50;
with Keccak.Keccak_50.Rounds_14;
with Keccak.Keccak_100;
with Keccak.Keccak_100.Rounds_16;
with Keccak.Keccak_200;
with Keccak.Keccak_200.Rounds_18;
with Keccak.Keccak_400;
with Keccak.Keccak_400.Rounds_20;
with Keccak.Keccak_800;
with Keccak.Keccak_800.Rounds_22;
with Keccak.Keccak_1600;
with Keccak.Keccak_1600.Rounds_24;
with Keccak.Keccak_1600.Rounds_12;
with Keccak.Types;
with Keccak.Generic_XOF;
with Keccak.Generic_Hash;
with Keccak.Generic_Duplex;
with Parallel_Hash;
with SHA3;
with SHAKE;
with RawSHAKE;
with Ketje;
with Gimli;
with Gimli.Hash;
with Ascon;
with Ascon.Permutations;
with Ascon.Hash;

procedure Benchmark
is
   Benchmark_Data_Size : constant := 512 * 1024; --  size of the benchmark data in bytes
   Repeat              : constant := 200;  --  number of benchmark iterations

   --  Use a 1 MiB data chunk as an input to the algorithms.
   --  A separate buffer is used as an output buffer for Ketje to avoid aliasing.
   type Byte_Array_Access is access Keccak.Types.Byte_Array;

   Data_Chunk_1 : constant Byte_Array_Access :=
     new Keccak.Types.Byte_Array (1 .. Benchmark_Data_Size);

   Data_Chunk_2 : constant Byte_Array_Access :=
     new Keccak.Types.Byte_Array (1 .. Benchmark_Data_Size);

   package Cycles_Count_IO is new Ada.Text_IO.Modular_IO (Cycles_Count);

   ---------------------------
   -- Print_Cycles_Per_Byte --
   ---------------------------

   procedure Print_Cycles_Per_Byte (Data_Size : in Natural;
                                    Cycles    : in Cycles_Count)
   is
      CPB : Long_Float;

   begin
      CPB := Long_Float (Cycles) / Long_Float (Data_Size);

      Ada.Long_Float_Text_IO.Put
        (Item => CPB,
         Fore => 0,
         Aft  => 1,
         Exp  => 0);

      Ada.Text_IO.Put (" cycles/byte");
      Ada.Text_IO.New_Line;
   end Print_Cycles_Per_Byte;

   ------------------
   -- Print_Cycles --
   ------------------

   procedure Print_Cycles (Cycles : in Cycles_Count)
   is
   begin
      Cycles_Count_IO.Put (Cycles, Width => 0);
      Ada.Text_IO.Put (" cycles");
      Ada.Text_IO.New_Line;
   end Print_Cycles;

   ----------------------------------------------------------------------------
   --  Hash_Benchmark
   --
   --  Generic procedure to run a benchmark for any hash algorithm (e.g. SHA3-224,
   --  Keccak-256, etc...).
   ----------------------------------------------------------------------------

   generic
       Name : String;
       with package Hash_Package is new Keccak.Generic_Hash (<>);
   procedure Hash_Benchmark;

   procedure Hash_Benchmark
   is
      Ctx   : Hash_Package.Context;
      Digest : Hash_Package.Digest_Type;

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

   begin
      Ada.Text_IO.Put (Name & ": ");

      Timing.Calibrate;

      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         Hash_Package.Init (Ctx);

         Hash_Package.Update (Ctx, Data_Chunk_1.all, Data_Chunk_1.all'Length * 8);

         Hash_Package.Final (Ctx, Digest);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);
   end Hash_Benchmark;

   ----------------------------------------------------------------------------
   --  XOF_Benchmark
   --
   --  Generic procedure to run a benchmark for any XOF algorithm (e.g. SHAKE128,
   --  RawSHAKE256, etc...).
   ----------------------------------------------------------------------------
   generic
       Name : String;
       with package XOF_Package is new Keccak.Generic_XOF (<>);
   procedure XOF_Benchmark;

   procedure XOF_Benchmark
   is
      Ctx    : XOF_Package.Context;

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

   begin
      Ada.Text_IO.Put (Name & " (Absorbing): ");

      Timing.Calibrate;

      --  Benchmark Absorbing
      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         XOF_Package.Init (Ctx);

         XOF_Package.Update (Ctx, Data_Chunk_1.all, Data_Chunk_1.all'Length * 8);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);

      Min_Cycles := Cycles_Count'Last;

      Ada.Text_IO.Put (Name & " (Squeezing): ");

      Timing.Calibrate;

      --  Benchmark squeezing
      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         XOF_Package.Extract (Ctx, Data_Chunk_1.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);
   end XOF_Benchmark;

   ----------------------------------------------------------------------------
   --  Duplex_Benchmark
   --
   --  Generic procedure to run a benchmark for any Duplex algorithm.
   ----------------------------------------------------------------------------
   generic
      Name : String;
      Capacity : Positive;
      with package Duplex is new Keccak.Generic_Duplex (<>);
   procedure Duplex_Benchmark;

   procedure Duplex_Benchmark
   is
      Ctx : Duplex.Context;

      Out_Data : Keccak.Types.Byte_Array (1 .. 1600 / 8);

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

   begin
      Ada.Text_IO.Put (Name & ": ");

      Duplex.Init (Ctx, Capacity);

      Timing.Calibrate;

      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         Duplex.Duplex (Ctx,
                        Data_Chunk_1.all (1 .. Duplex.Rate_Of (Ctx) / 8),
                        Duplex.Rate_Of (Ctx) - Duplex.Min_Padding_Bits,
                        Out_Data (1 .. Duplex.Rate_Of (Ctx) / 8),
                        Duplex.Rate_Of (Ctx) - Duplex.Min_Padding_Bits);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles (Min_Cycles);

   end Duplex_Benchmark;

   ----------------------------------------------------------------------------
   --  KeccakF_Benchmark
   --
   --  Generic procedure to run a benchmark for a KeccakF permutation.
   ----------------------------------------------------------------------------
   generic
      Name : String;
      type State_Type is private;
      with procedure Init (A : out State_Type);
      with procedure Permute (A : in out State_Type);
   procedure KeccakF_Benchmark;

   procedure KeccakF_Benchmark
   is
      State : State_Type;

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

      Num_Iterations : constant Natural := Repeat * 100;

   begin
      Ada.Text_IO.Put (Name & ": ");

      Init (State);

      Timing.Calibrate;

      for I in Positive range 1 .. Num_Iterations loop
         Start_Measurement (Start_Time);

         Permute (State);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles (Min_Cycles);

   end KeccakF_Benchmark;

   ----------------------------------------------------------------------------
   --  K12_Benchmark
   --
   --  Generic procedure to run a benchmark for a KangarooTwelve
   ----------------------------------------------------------------------------
   generic
      Name : String;
      with package K12 is new Keccak.Generic_KangarooTwelve (<>);
   procedure K12_Benchmark;

   procedure K12_Benchmark
   is
      Ctx    : K12.Context;

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

   begin
      Ada.Text_IO.Put (Name & " (Absorbing): ");

      Timing.Calibrate;

      --  Benchmark Absorbing
      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         K12.Init (Ctx);

         K12.Update (Ctx, Data_Chunk_1.all);

         K12.Finish (Ctx, "");

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);

      Min_Cycles := Cycles_Count'Last;
      Ada.Text_IO.Put (Name & " (Squeezing): ");

      Timing.Calibrate;

      --  Benchmark squeezing
      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         K12.Extract (Ctx, Data_Chunk_1.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);
   end K12_Benchmark;

   ----------------------------------------------------------------------------
   --  ParallelHash_Benchmark
   --
   --  Generic procedure to run a benchmark for a ParallelHash
   ----------------------------------------------------------------------------
   generic
      Name : String;
      with package ParallelHash is new Keccak.Generic_Parallel_Hash (<>);
   procedure ParallelHash_Benchmark;

   procedure ParallelHash_Benchmark
   is
      Ctx    : ParallelHash.Context;

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

   begin
      Ada.Text_IO.Put (Name & " (Absorbing): ");

      Timing.Calibrate;

      --  Benchmark Absorbing
      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         ParallelHash.Init (Ctx, 8192, "");

         ParallelHash.Update (Ctx, Data_Chunk_1.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);

      Min_Cycles := Cycles_Count'Last;
      Ada.Text_IO.Put (Name & " (Squeezing): ");

      Timing.Calibrate;

      --  Benchmark squeezing
      for I in Positive range 1 .. Repeat loop
         Start_Measurement (Start_Time);

         ParallelHash.Extract (Ctx, Data_Chunk_1.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);
   end ParallelHash_Benchmark;

   ----------------------------------------------------------------------------
   --  Ketje_Benchmark
   --
   --  Generic procedure to run a benchmark for Ketje (instances of MonkeyWrap)
   ----------------------------------------------------------------------------
   generic
      Name : String;
      with package MonkeyWrap is new Keccak.Generic_MonkeyWrap (<>);
   procedure Ketje_Benchmark;

   procedure Ketje_Benchmark
   is
      Ctx    : MonkeyWrap.Context;

      Start_Time : Timing.Time;
      Cycles     : Cycles_Count;
      Min_Cycles : Cycles_Count := Cycles_Count'Last;

      Empty : constant Keccak.Types.Byte_Array (1 .. 0) := (others => 0);

   begin
      Ada.Text_IO.Put (Name & " (AAD): ");

      Timing.Calibrate;

      --  Benchmark AAD
      for I in Positive range 1 .. Repeat loop
         MonkeyWrap.Init (Ctx, Empty, Empty);

         Start_Measurement (Start_Time);

         MonkeyWrap.Update_Auth_Data (Ctx, Data_Chunk_1.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);

      Min_Cycles := Cycles_Count'Last;
      Ada.Text_IO.Put (Name & " (Encrypt): ");

      Timing.Calibrate;

      --  Benchmark Encrypt
      for I in Positive range 1 .. Repeat loop
         MonkeyWrap.Init (Ctx, Empty, Empty);

         Start_Measurement (Start_Time);

         MonkeyWrap.Update_Encrypt (Ctx, Data_Chunk_1.all, Data_Chunk_2.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);
      Ada.Text_IO.Put (Name & " (Decrypt): ");

      --  Benchmark Decrypt
      for I in Positive range 1 .. Repeat loop
         MonkeyWrap.Init (Ctx, Empty, Empty);

         Start_Measurement (Start_Time);

         MonkeyWrap.Update_Decrypt (Ctx, Data_Chunk_1.all, Data_Chunk_2.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);
      Ada.Text_IO.Put (Name & " (Tag): ");

      --  Benchmark Tag
      for I in Positive range 1 .. Repeat loop
         MonkeyWrap.Init (Ctx, Empty, Empty);

         Start_Measurement (Start_Time);

         MonkeyWrap.Extract_Tag (Ctx, Data_Chunk_1.all);

         Cycles := End_Measurement (Start_Time);

         if Cycles < Min_Cycles then
            Min_Cycles := Cycles;
         end if;
      end loop;

      Print_Cycles_Per_Byte (Data_Chunk_1.all'Length, Min_Cycles);

   end Ketje_Benchmark;

   ----------------------------------------------------------------------------
   --  Benchmark procedure instantiations.
   ----------------------------------------------------------------------------

   procedure Benchmark_SHA_224 is new Hash_Benchmark
      ("SHA3-224", SHA3.SHA3_224);
   procedure Benchmark_SHA_256 is new Hash_Benchmark
      ("SHA3-256", SHA3.SHA3_256);
   procedure Benchmark_SHA_384 is new Hash_Benchmark
      ("SHA3-384", SHA3.SHA3_384);
   procedure Benchmark_SHA_512 is new Hash_Benchmark
      ("SHA3-512", SHA3.SHA3_512);

   procedure Benchmark_Keccak_224 is new Hash_Benchmark
      ("Keccak-224", SHA3.Keccak_224);
   procedure Benchmark_Keccak_256 is new Hash_Benchmark
      ("Keccak-256", SHA3.Keccak_256);
   procedure Benchmark_Keccak_384 is new Hash_Benchmark
      ("Keccak-384", SHA3.Keccak_384);
   procedure Benchmark_Keccak_512 is new Hash_Benchmark
      ("Keccak-512", SHA3.Keccak_512);

   procedure Benchmark_SHAKE128 is new XOF_Benchmark
      ("SHAKE128", SHAKE.SHAKE128);
   procedure Benchmark_SHAKE256 is new XOF_Benchmark
      ("SHAKE256", SHAKE.SHAKE256);

   procedure Benchmark_RawSHAKE128 is new XOF_Benchmark
      ("RawSHAKE128", RawSHAKE.RawSHAKE128);
   procedure Benchmark_RawSHAKE256 is new XOF_Benchmark
      ("RawSHAKE256", RawSHAKE.RawSHAKE256);

   procedure Benchmark_Duplex_r1152c448 is new Duplex_Benchmark
      ("Duplex r1152c448", 448, Keccak.Keccak_1600.Rounds_24.Duplex);
   procedure Benchmark_Duplex_r1088c512 is new Duplex_Benchmark
      ("Duplex r1088c512", 512, Keccak.Keccak_1600.Rounds_24.Duplex);
   procedure Benchmark_Duplex_r832c768 is new Duplex_Benchmark
      ("Duplex r832c768", 768, Keccak.Keccak_1600.Rounds_24.Duplex);
   procedure Benchmark_Duplex_r576c1024 is new Duplex_Benchmark
     ("Duplex r576c1024", 1024, Keccak.Keccak_1600.Rounds_24.Duplex);

   procedure Benchmark_KeccakF_25 is new KeccakF_Benchmark
     ("Keccak-p[25,12]",
      Keccak.Keccak_25.State,
      Keccak.Keccak_25.KeccakF_25.Init,
      Keccak.Keccak_25.Rounds_12.Permute);
   procedure Benchmark_KeccakF_50 is new KeccakF_Benchmark
     ("Keccak-p[50,14]",
      Keccak.Keccak_50.State,
      Keccak.Keccak_50.KeccakF_50.Init,
      Keccak.Keccak_50.Rounds_14.Permute);
   procedure Benchmark_KeccakF_100 is new KeccakF_Benchmark
     ("Keccak-p[100,16]",
      Keccak.Keccak_100.State,
      Keccak.Keccak_100.KeccakF_100.Init,
      Keccak.Keccak_100.Rounds_16.Permute);
   procedure Benchmark_KeccakF_200 is new KeccakF_Benchmark
     ("Keccak-p[200,18]",
      Keccak.Keccak_200.State,
      Keccak.Keccak_200.KeccakF_200.Init,
      Keccak.Keccak_200.Rounds_18.Permute);
   procedure Benchmark_KeccakF_400 is new KeccakF_Benchmark
     ("Keccak-p[400,20]",
      Keccak.Keccak_400.State,
      Keccak.Keccak_400.KeccakF_400.Init,
      Keccak.Keccak_400.Rounds_20.Permute);
   procedure Benchmark_KeccakF_800 is new KeccakF_Benchmark
     ("Keccak-p[800,22]",
      Keccak.Keccak_800.State,
      Keccak.Keccak_800.KeccakF_800.Init,
      Keccak.Keccak_800.Rounds_22.Permute);
   procedure Benchmark_KeccakF_1600_R24 is new KeccakF_Benchmark
     ("Keccak-p[1600,24]",
      Keccak.Keccak_1600.State,
      Keccak.Keccak_1600.KeccakF_1600.Init,
      Keccak.Keccak_1600.Rounds_24.Permute);
   procedure Benchmark_KeccakF_1600_P2_R24 is new KeccakF_Benchmark
     ("Keccak-p[1600,24]×2",
      Keccak.Parallel_Keccak_1600.Parallel_State_P2,
      Keccak.Parallel_Keccak_1600.Init_P2,
      Keccak.Parallel_Keccak_1600.Rounds_24.Permute_All_P2);
   procedure Benchmark_KeccakF_1600_P4_R24 is new KeccakF_Benchmark
     ("Keccak-p[1600,24]×4",
      Keccak.Parallel_Keccak_1600.Parallel_State_P4,
      Keccak.Parallel_Keccak_1600.Init_P4,
      Keccak.Parallel_Keccak_1600.Rounds_24.Permute_All_P4);
   procedure Benchmark_KeccakF_1600_P8_R24 is new KeccakF_Benchmark
     ("Keccak-p[1600,24]×8",
      Keccak.Parallel_Keccak_1600.Parallel_State_P8,
      Keccak.Parallel_Keccak_1600.Init_P8,
      Keccak.Parallel_Keccak_1600.Rounds_24.Permute_All_P8);
   procedure Benchmark_KeccakF_1600_R12 is new KeccakF_Benchmark
     ("Keccak-p[1600,12]",
      Keccak.Keccak_1600.State,
      Keccak.Keccak_1600.KeccakF_1600.Init,
      Keccak.Keccak_1600.Rounds_12.Permute);
   procedure Benchmark_KeccakF_1600_P2_R12 is new KeccakF_Benchmark
     ("Keccak-p[1600,12]×2",
      Keccak.Parallel_Keccak_1600.Parallel_State_P2,
      Keccak.Parallel_Keccak_1600.Init_P2,
      Keccak.Parallel_Keccak_1600.Rounds_12.Permute_All_P2);
   procedure Benchmark_KeccakF_1600_P4_R12 is new KeccakF_Benchmark
     ("Keccak-p[1600,12]×4",
      Keccak.Parallel_Keccak_1600.Parallel_State_P4,
      Keccak.Parallel_Keccak_1600.Init_P4,
      Keccak.Parallel_Keccak_1600.Rounds_12.Permute_All_P4);
   procedure Benchmark_KeccakF_1600_P8_R12 is new KeccakF_Benchmark
     ("Keccak-p[1600,12]×8",
      Keccak.Parallel_Keccak_1600.Parallel_State_P8,
      Keccak.Parallel_Keccak_1600.Init_P8,
      Keccak.Parallel_Keccak_1600.Rounds_12.Permute_All_P8);

   procedure Benchmark_K12 is new K12_Benchmark
     ("KangarooTwelve",
      KangarooTwelve.K12);

   procedure Benchmark_M14 is new K12_Benchmark
     ("MarsupilamiFourteen",
      MarsupilamiFourteen.M14);

   procedure Benchmark_ParallelHash128 is new ParallelHash_Benchmark
     ("ParallelHash128",
      Parallel_Hash.ParallelHash128);
   procedure Benchmark_ParallelHash256 is new ParallelHash_Benchmark
     ("ParallelHash256",
      Parallel_Hash.ParallelHash256);

   procedure Benchmark_Ketje_Jr is new Ketje_Benchmark
     ("Ketje Jr", Ketje.Jr);
   procedure Benchmark_Ketje_Sr is new Ketje_Benchmark
     ("Ketje Sr", Ketje.Sr);
   procedure Benchmark_Ketje_Minor is new Ketje_Benchmark
     ("Ketje Minor", Ketje.Minor);
   procedure Benchmark_Ketje_Major is new Ketje_Benchmark
     ("Ketje Major", Ketje.Major);

   procedure Benchmark_Gimli is new KeccakF_Benchmark
     ("Gimli",
      Gimli.State,
      Gimli.Init,
      Gimli.Permute);

   procedure Benchmark_Gimli_Hash is new Hash_Benchmark
     ("Gimli Hash",
      Gimli.Hash);

   procedure Benchmark_Ascon12 is new KeccakF_Benchmark
     ("Ascon (12 rounds)",
      Ascon.State,
      Ascon.Init,
      Ascon.Permutations.Permute_12);

   procedure Benchmark_Ascon8 is new KeccakF_Benchmark
     ("Ascon (8 rounds)",
      Ascon.State,
      Ascon.Init,
      Ascon.Permutations.Permute_8);

   procedure Benchmark_Ascon6 is new KeccakF_Benchmark
     ("Ascon (6 rounds)",
      Ascon.State,
      Ascon.Init,
      Ascon.Permutations.Permute_6);

   procedure Benchmark_Ascon_Hash is new Hash_Benchmark
     ("Ascon-Hash",
      Ascon.Hash);

begin
   Data_Chunk_1.all := (others => 16#A7#);
   Data_Chunk_2.all := (others => 16#A7#);

   Put ("Message size: ");
   Ada.Integer_Text_IO.Put (Data_Chunk_1.all'Length, Width => 0);
   Put (" bytes");
   New_Line;
   Put ("Performing ");
   Ada.Integer_Text_IO.Put (Repeat, Width => 0);
   Put (" measurements for each test");
   New_Line;
   New_Line;

   Benchmark_Gimli;
   Benchmark_Gimli_Hash;

   Benchmark_Ascon12;
   Benchmark_Ascon8;
   Benchmark_Ascon6;
   Benchmark_Ascon_Hash;

   Benchmark_K12;
   Benchmark_M14;
   Benchmark_ParallelHash128;
   Benchmark_ParallelHash256;
   Benchmark_SHA_224;
   Benchmark_SHA_256;
   Benchmark_SHA_384;
   Benchmark_SHA_512;
   Benchmark_Keccak_224;
   Benchmark_Keccak_256;
   Benchmark_Keccak_384;
   Benchmark_Keccak_512;
   Benchmark_SHAKE128;
   Benchmark_SHAKE256;
   Benchmark_RawSHAKE128;
   Benchmark_RawSHAKE256;
   Benchmark_Duplex_r1152c448;
   Benchmark_Duplex_r1088c512;
   Benchmark_Duplex_r832c768;
   Benchmark_Duplex_r576c1024;
   Benchmark_KeccakF_1600_R24;
   Benchmark_KeccakF_1600_P2_R24;
   Benchmark_KeccakF_1600_P4_R24;
   Benchmark_KeccakF_1600_P8_R24;
   Benchmark_KeccakF_1600_R12;
   Benchmark_KeccakF_1600_P2_R12;
   Benchmark_KeccakF_1600_P4_R12;
   Benchmark_KeccakF_1600_P8_R12;
   Benchmark_KeccakF_800;
   Benchmark_KeccakF_400;
   Benchmark_KeccakF_200;
   Benchmark_KeccakF_100;
   Benchmark_KeccakF_50;
   Benchmark_KeccakF_25;
   Benchmark_Ketje_Jr;
   Benchmark_Ketje_Sr;
   Benchmark_Ketje_Minor;
   Benchmark_Ketje_Major;

end Benchmark;
