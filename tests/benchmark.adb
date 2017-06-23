-------------------------------------------------------------------------------
-- Copyright (c) 2016, Daniel King
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * The name of the copyright holder may not be used to endorse or promote
--       Products derived from this software without specific prior written
--       permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
-- THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Execution_Time;
with Ada.Text_IO;
with KangarooTwelve;
with Keccak.Parallel_Keccak_1600;
with Keccak.Generic_KangarooTwelve;
with Keccak.Generic_KeccakF;
with Keccak.Keccak_25;
with Keccak.Keccak_50;
with Keccak.Keccak_100;
with Keccak.Keccak_200;
with Keccak.Keccak_400;
with Keccak.Keccak_800;
with Keccak.Keccak_1600;
with Keccak.Types;
with Keccak.Generic_XOF;
with Keccak.Generic_Hash;
with Keccak.Generic_Duplex;
with Keccak.Keccak_1600;
with SHA3;
with SHAKE;
with RawSHAKE;

procedure Benchmark
is
   Benchmark_Data_Size_MiB : constant := 128; -- size of the benchmark data in MiB
   Repeat                  : constant := 10;  -- number of benchmark iterations
   
   -- A 1 MiB data chunk to use as an input to the algorithms.
   type Byte_Array_Access is access Keccak.Types.Byte_Array;
   Data_Chunk : Byte_Array_Access := new Keccak.Types.Byte_Array (1 .. Benchmark_Data_Size_MiB*1024*1024);
   
   ----------------------------------------------------------------------------
   -- Print_Number
   --
   -- Prints a number with the unit B/kiB/MiB/GiB depending on the magnitude
   -- of the value. E.g. 1_048_576 would be displayed as 1.0 MiB.
   ----------------------------------------------------------------------------
   procedure Print_Number(Value  : in Float;
                          Suffix : in String;
                          Aft    : in Natural)
   is
      package Float_IO  is new Ada.Text_IO.Float_IO(Float);
      
   begin
      if Value >= 1024.0*1024.0*1024.0 then
         Float_IO.Put(Value / (1024.0*1024.0*1024.0),
                      Fore => 0, 
                      Aft => Aft, 
                      Exp => 0);
         Ada.Text_IO.Put(" GiB" & Suffix);
      elsif Value >= 1024.0*1024.0 then
         Float_IO.Put(Value / (1024.0*1024.0),
                      Fore => 0, 
                      Aft => Aft, 
                      Exp => 0);
         Ada.Text_IO.Put(" MiB" & Suffix);
      elsif Value >= 1024.0 then
         Float_IO.Put(Value / 1024.0,
                      Fore => 0, 
                      Aft => Aft, 
                      Exp => 0);
         Ada.Text_IO.Put(" kiB" & Suffix);
      else
         Float_IO.Put(Value,
                      Fore => 0, 
                      Aft => Aft, 
                      Exp => 0);
         Ada.Text_IO.Put(" B" & Suffix);
      end if;
      
   end Print_Number;

   ----------------------------------------------------------------------------
   -- Print_Time
   --
   -- Prints a line displaying: the measurement time, the data size, and
   -- the performance (bytes per second).
   --
   -- E.g. Print_Time(1_048_576, Elapsed) where Elapsed represents a Time_Span
   -- of 0.123 seconds would be displayed as:
   -- "0.123000s, 1.0 MiB, 8.130 MiB/s"
   ----------------------------------------------------------------------------
   procedure Print_Time(Data_Size : in Natural;
                        Time      : in Ada.Real_Time.Time_Span)
   is
      package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
      
   begin
      Duration_IO.Put(Ada.Real_Time.To_Duration(Time), Fore => 0, Aft => 6);
      Ada.Text_IO.Put("s, ");
      
      Print_Number(Float(Data_Size), "", 0);
      
      Ada.Text_IO.Put(", ");
      
      Print_Number(Float(Data_Size) / Float(Ada.Real_Time.To_Duration(Time)), "/s", 3);
      
      Ada.Text_IO.New_Line;
   end Print_Time;
   
   ----------------------------------------------------------------------------
   -- Hash_Benchmark
   --
   -- Generic procedure to run a benchmark for any hash algorithm (e.g. SHA3-224,
   -- Keccak-256, etc...).
   ----------------------------------------------------------------------------
   generic
       Name : String;
       with package Hash_Package is new Keccak.Generic_Hash(<>);
   procedure Hash_Benchmark;
   
   procedure Hash_Benchmark
   is
      use type Ada.Execution_Time.CPU_Time;
   
      Ctx   : Hash_Package.Context;
      Digest : Hash_Package.Digest_Type;
      
      Start_Time : Ada.Execution_Time.CPU_Time;
      End_Time   : Ada.Execution_Time.CPU_Time;
      
      Total_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      
   begin
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
      
         Hash_Package.Init(Ctx);
         
         Hash_Package.Update(Ctx, Data_Chunk.all, Data_Chunk.all'Length*8);
         
         Hash_Package.Final(Ctx, Digest);
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & ", ");
         Print_Time(Data_Chunk.all'Length, End_Time - Start_Time);
      end loop;
      
      Ada.Text_IO.Put(Name & " Average for" & Natural'Image (Repeat) & " runs: ");
      Print_Time (Data_Chunk.all'Length, Total_Time / Repeat);
   end Hash_Benchmark;
   
   
   ----------------------------------------------------------------------------
   -- XOF_Benchmark
   --
   -- Generic procedure to run a benchmark for any XOF algorithm (e.g. SHAKE128,
   -- RawSHAKE256, etc...).
   ----------------------------------------------------------------------------
   generic
       Name : String;
       with package XOF_Package is new Keccak.Generic_XOF(<>);
   procedure XOF_Benchmark;
   
   procedure XOF_Benchmark
   is
      use type Ada.Execution_Time.CPU_Time;
   
      Ctx    : XOF_Package.Context;
      
      Start_Time : Ada.Execution_Time.CPU_Time;
      End_Time   : Ada.Execution_Time.CPU_Time;
      
      Total_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      
   begin
      
      -- Benchmark Absorbing
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
      
         XOF_Package.Init(Ctx);
         
         XOF_Package.Update(Ctx, Data_Chunk.all, Data_Chunk.all'Length*8);
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & " (Absorbing), ");
         Print_Time(Data_Chunk.all'Length, End_Time - Start_Time);
      end loop;
      
      Ada.Text_IO.Put(Name & " (Absorbing) Average for" & Natural'Image (Repeat) & " runs: ");
      Print_Time (Data_Chunk.all'Length, Total_Time / Repeat);
      Ada.Text_IO.New_Line;
      
      Total_Time := Ada.Real_Time.Time_Span_Zero;
      
      -- Benchmark squeezing
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
      
         XOF_Package.Extract(Ctx, Data_Chunk.all);
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & " (Squeezing), ");
         Print_Time(Data_Chunk.all'Length, End_Time - Start_Time);
      end loop;
      
      Ada.Text_IO.Put(Name & " (Squeezing) Average for" & Natural'Image (Repeat) & " runs: ");
      Print_Time (Data_Chunk.all'Length, Total_Time / Repeat);
   end XOF_Benchmark;
   
   ----------------------------------------------------------------------------
   -- Duplex_Benchmark
   --
   -- Generic procedure to run a benchmark for any Duplex algorithm.
   ----------------------------------------------------------------------------
   generic
      Name : String;
      Capacity : Positive;
      with package Duplex is new Keccak.Generic_Duplex(<>);
   procedure Duplex_Benchmark;
   
   procedure Duplex_Benchmark
   is
      use type Ada.Execution_Time.CPU_Time;
      
      Ctx : Duplex.Context;
      
      Out_Data : Keccak.Types.Byte_Array(1 .. 1600/8);
      
      Start_Time : Ada.Execution_Time.CPU_Time;
      End_Time   : Ada.Execution_Time.CPU_Time;
      
      Total_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      
      Num_Iterations : Natural := (Benchmark_Data_Size_MiB*1024*1024) / ((1600-Capacity)/8);
      
   begin
      
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
         
         Duplex.Init(Ctx, Capacity);
         
         for J in Positive range 1 .. Num_Iterations loop
            Duplex.Duplex(Ctx,
                          Data_Chunk.all(1 .. Duplex.Rate_Of(Ctx)/8),
                          Duplex.Rate_Of(Ctx) - Duplex.Min_Padding_Bits,
                          Out_Data(1 .. Duplex.Rate_Of(Ctx)/8),
                          Duplex.Rate_Of(Ctx) - Duplex.Min_Padding_Bits);
         end loop;
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & ", ");
         Print_Time((Duplex.Rate_Of(Ctx)/8) * Num_Iterations,
                    End_Time - Start_Time);
                       
      end loop;
      
      Ada.Text_IO.Put(Name & " Average for" & Natural'Image (Repeat) & " runs: ");
      Print_Time (Data_Chunk.all'Length, Total_Time / Repeat);
   
   end Duplex_Benchmark;
   
   ----------------------------------------------------------------------------
   -- KeccakF_Benchmark
   --
   -- Generic procedure to run a benchmark for a KeccakF permutation.
   ----------------------------------------------------------------------------
   generic
      Name : String;
      type State_Type is private;
      with procedure Init (A : out State_Type);
      with procedure Permute(A : in out State_Type);
   procedure KeccakF_Benchmark;
   
   procedure KeccakF_Benchmark
   is
      use type Ada.Execution_Time.CPU_Time;
      
      package Duration_IO is new Ada.Text_IO.Fixed_IO(Duration);
      package Integer_IO is new Ada.Text_IO.Integer_IO(Integer);
            
      State : State_Type;
      
      Start_Time : Ada.Execution_Time.CPU_Time;
      End_Time   : Ada.Execution_Time.CPU_Time;
      
      Total_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      
      Num_Iterations : Natural := 1_000_000;
      
   begin
      Init(State);
      
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
         
         for J in Positive range 1 .. Num_Iterations loop
            Permute(State);
         end loop;
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & ", ");
         
         Integer_IO.Put(Item => Num_Iterations, Width => 0);
         Ada.Text_IO.Put(" calls, ");
         
         Duration_IO.Put((Ada.Real_Time.To_Duration(End_Time - Start_Time) * 1_000_000) / Num_Iterations, Fore => 0);
         Ada.Text_IO.Put_Line(" us/call");
      end loop;
      
      Ada.Text_IO.Put(Name & " Average for" & Natural'Image (Repeat) & " runs: ");

      Integer_IO.Put(Item => Num_Iterations, Width => 0);
      Ada.Text_IO.Put(" calls, ");

      Duration_IO.Put((Ada.Real_Time.To_Duration(Total_Time / Repeat) * 1_000_000) / Num_Iterations, Fore => 0);
      Ada.Text_IO.Put_Line(" us/call");
      
   end KeccakF_Benchmark;
   
   ----------------------------------------------------------------------------
   -- K12_Benchmark
   --
   -- Generic procedure to run a benchmark for a KangarooTwelve 
   ----------------------------------------------------------------------------
   generic
      Name : String;
      with package K12 is new Keccak.Generic_KangarooTwelve(<>);
   procedure K12_Benchmark;
   
   procedure K12_Benchmark
   is
      use type Ada.Execution_Time.CPU_Time;
   
      Ctx    : K12.Context;
      
      Start_Time : Ada.Execution_Time.CPU_Time;
      End_Time   : Ada.Execution_Time.CPU_Time;
      
      Total_Time : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Zero;
      
   begin
      
      -- Benchmark Absorbing
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
      
         K12.Init(Ctx);
         
         K12.Update(Ctx, Data_Chunk.all);
      
         K12.Finish (Ctx, "Benchmark");
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & " (Updating), ");
         Print_Time(Data_Chunk.all'Length, End_Time - Start_Time);
      end loop;
      
      Ada.Text_IO.Put(Name & " (Absorbing) Average for" & Natural'Image (Repeat) & " runs: ");
      Print_Time (Data_Chunk.all'Length, Total_Time / Repeat);
      Ada.Text_IO.New_Line;
      
      Total_Time := Ada.Real_Time.Time_Span_Zero;
      
      -- Benchmark squeezing
      for I in Positive range 1 .. Repeat loop
         Start_Time := Ada.Execution_Time.Clock;
      
         K12.Extract(Ctx, Data_Chunk.all);
         
         End_Time := Ada.Execution_Time.Clock;
         
         Total_Time := Total_Time + (End_Time - Start_Time);
         
         Ada.Text_IO.Put(Name & " (Squeezing), ");
         Print_Time(Data_Chunk.all'Length, End_Time - Start_Time);
      end loop;
      
      Ada.Text_IO.Put(Name & " (Squeezing) Average for" & Natural'Image (Repeat) & " runs: ");
      Print_Time (Data_Chunk.all'Length, Total_Time / Repeat);
   end K12_Benchmark;
   
   ----------------------------------------------------------------------------
   -- Benchmark procedure instantiations.
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
      ("Duplex r1152c448", 448, Keccak.Keccak_1600.Duplex);
   procedure Benchmark_Duplex_r1088c512 is new Duplex_Benchmark
      ("Duplex r1088c512", 512, Keccak.Keccak_1600.Duplex);
   procedure Benchmark_Duplex_r832c768 is new Duplex_Benchmark
      ("Duplex r832c768", 768, Keccak.Keccak_1600.Duplex);
   procedure Benchmark_Duplex_r576c1024 is new Duplex_Benchmark
     ("Duplex r576c1024", 1024, Keccak.Keccak_1600.Duplex);
   
   procedure Benchmark_KeccakF_25 is new KeccakF_Benchmark
     ("Keccak-p[25,12]", 
      Keccak.Keccak_25.KeccakF_25.State, 
      Keccak.Keccak_25.KeccakF_25.Init, 
      Keccak.Keccak_25.Permute);
   procedure Benchmark_KeccakF_50 is new KeccakF_Benchmark
     ("Keccak-p[50,14]", 
      Keccak.Keccak_50.KeccakF_50.State, 
      Keccak.Keccak_50.KeccakF_50.Init, 
      Keccak.Keccak_50.Permute);
   procedure Benchmark_KeccakF_100 is new KeccakF_Benchmark
     ("Keccak-p[100,16]", 
      Keccak.Keccak_100.KeccakF_100.State, 
      Keccak.Keccak_100.KeccakF_100.Init, 
      Keccak.Keccak_100.Permute);
   procedure Benchmark_KeccakF_200 is new KeccakF_Benchmark
     ("Keccak-p[200,18]", 
      Keccak.Keccak_200.KeccakF_200.State, 
      Keccak.Keccak_200.KeccakF_200.Init,  
      Keccak.Keccak_200.Permute);
   procedure Benchmark_KeccakF_400 is new KeccakF_Benchmark
     ("Keccak-p[400,20]", 
      Keccak.Keccak_400.KeccakF_400.State, 
      Keccak.Keccak_400.KeccakF_400.Init, 
      Keccak.Keccak_400.Permute);
   procedure Benchmark_KeccakF_800 is new KeccakF_Benchmark
     ("Keccak-p[800,22]",
      Keccak.Keccak_800.KeccakF_800.State, 
      Keccak.Keccak_800.KeccakF_800.Init, 
      Keccak.Keccak_800.Permute);
   procedure Benchmark_KeccakF_1600 is new KeccakF_Benchmark
     ("Keccak-p[1600,24]", 
      Keccak.Keccak_1600.KeccakF_1600.State,
      Keccak.Keccak_1600.KeccakF_1600.Init, 
      Keccak.Keccak_1600.Permute_R24);
   procedure Benchmark_KeccakF_1600_P2_R12 is new KeccakF_Benchmark
     ("Keccak-p[1600,12]×2", 
      Keccak.Parallel_Keccak_1600.Parallel_State_P2,
      Keccak.Parallel_Keccak_1600.Init_P2, 
      Keccak.Parallel_Keccak_1600.Permute_All_P2_R12);
   procedure Benchmark_KeccakF_1600_P2_R24 is new KeccakF_Benchmark
     ("Keccak-p[1600,24]×2", 
      Keccak.Parallel_Keccak_1600.Parallel_State_P2,
      Keccak.Parallel_Keccak_1600.Init_P2, 
      Keccak.Parallel_Keccak_1600.Permute_All_P2_R24);
   
   procedure Benchmark_K12 is new K12_Benchmark
     ("KangarooTwelve",
      KangarooTwelve.K12);
      
   type Algorithm_Name is 
      (K12,
      SHA3_224,
      SHA3_256,
      SHA3_384,
      SHA3_512,
      Keccak_224,
      Keccak_256,
      Keccak_384,
      Keccak_512,
      SHAKE128,
      SHAKE256,
      RawSHAKE128,
      RawSHAKE256,
      Duplex_r1152c448,
      Duplex_r1088c512,
      Duplex_r832c768,
      Duplex_r576c1024,
      KeccakF_1600,
      KeccakF_1600_P2_R24,
      KeccakF_1600_P2_R12,
      KeccakF_800,
      KeccakF_400,
      KeccakF_200,
      KeccakF_100,
      KeccakF_50,
      KeccakF_25);
   
   Benchmarks_Enabled : array (Algorithm_Name) of Boolean := (others => False);

begin
   Data_Chunk.all := (others => 16#A7#);
   
   for I in 1 .. Ada.Command_Line.Argument_Count loop
      declare
         Arg : constant String := Ada.Command_Line.Argument (I);
      begin
         if Arg = "--all" then
            Benchmarks_Enabled := (others => True);
         elsif Arg = "--k12" then
            Benchmarks_Enabled (K12) := True;
         elsif Arg = "--sha3-224" then
            Benchmarks_Enabled (SHA3_224) := True;
         elsif Arg = "--sha3-256" then
            Benchmarks_Enabled (SHA3_256) := True;
         elsif Arg = "--sha3-384" then
            Benchmarks_Enabled (SHA3_384) := True;
         elsif Arg = "--sha3-512" then
            Benchmarks_Enabled (SHA3_512) := True;
         elsif Arg = "--keccak-224" then
            Benchmarks_Enabled (Keccak_224) := True;
         elsif Arg = "--keccak-256" then
            Benchmarks_Enabled (Keccak_256) := True;
         elsif Arg = "--keccak-384" then
            Benchmarks_Enabled (Keccak_384) := True;
         elsif Arg = "--keccak-512" then
            Benchmarks_Enabled (Keccak_512) := True;
         elsif Arg = "--shake128" then
            Benchmarks_Enabled (SHAKE128) := True;
         elsif Arg = "--shake256" then
            Benchmarks_Enabled (SHAKE256) := True;
         elsif Arg = "--rawshake128" then
            Benchmarks_Enabled (RawSHAKE128) := True;
         elsif Arg = "--rawshake256" then
            Benchmarks_Enabled (RawSHAKE256) := True;
         elsif Arg = "--duplex-r1152c448" then
            Benchmarks_Enabled (Duplex_r1152c448) := True;
         elsif Arg = "--duplex-r1088c512" then
            Benchmarks_Enabled (Duplex_r1088c512) := True;
         elsif Arg = "--duplex-r832c768" then
            Benchmarks_Enabled (Duplex_r832c768) := True;
         elsif Arg = "--duplex-r576c1024" then
            Benchmarks_Enabled (Duplex_r576c1024) := True;
         elsif Arg = "--keccak-f[1600,24]" then
            Benchmarks_Enabled (KeccakF_1600) := True;
         elsif Arg = "--keccak-f[1600,24]x2" then
            Benchmarks_Enabled (KeccakF_1600_P2_R24) := True;
         elsif Arg = "--keccak-f[1600,12]x2" then
            Benchmarks_Enabled (KeccakF_1600_P2_R12) := True;
         elsif Arg = "--keccak-f[800,22]" then
            Benchmarks_Enabled (KeccakF_800) := True;
         elsif Arg = "--keccak-f[400,20]" then
            Benchmarks_Enabled (KeccakF_400) := True;
         elsif Arg = "--keccak-f[200,18]" then
            Benchmarks_Enabled (KeccakF_200) := True;
         elsif Arg = "--keccak-f[100,16]" then
            Benchmarks_Enabled (KeccakF_100) := True;
         elsif Arg = "--keccak-f[50,14]" then
            Benchmarks_Enabled (KeccakF_50) := True;
         elsif Arg = "--keccak-f[25,12]" then
            Benchmarks_Enabled (KeccakF_25) := True;
         else
            Ada.Text_IO.Put_Line ("Unrecognized argument: " & Arg);
            Ada.Command_Line.Set_Exit_Status (1);
            return;
         end if;
      end;
   end loop;

   if Benchmarks_Enabled (K12) then
      Benchmark_K12;
      Ada.Text_IO.New_Line;
   end if;

   if Benchmarks_Enabled (SHA3_224) then
      Benchmark_SHA_224;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (SHA3_256) then
      Benchmark_SHA_256;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (SHA3_384) then
      Benchmark_SHA_384;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (SHA3_512) then
      Benchmark_SHA_512;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Keccak_224) then
      Benchmark_Keccak_224;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Keccak_256) then
      Benchmark_Keccak_256;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Keccak_384) then
      Benchmark_Keccak_384;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Keccak_512) then
      Benchmark_Keccak_512;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (SHAKE128) then
      Benchmark_SHAKE128;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (SHAKE256) then
      Benchmark_SHAKE256;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (RawSHAKE128) then
      Benchmark_RawSHAKE128;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (RawSHAKE256) then
      Benchmark_RawSHAKE256;
     Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Duplex_r1152c448) then
      Benchmark_Duplex_r1152c448;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Duplex_r1088c512) then
      Benchmark_Duplex_r1088c512;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Duplex_r832c768) then
      Benchmark_Duplex_r832c768;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (Duplex_r576c1024) then
      Benchmark_Duplex_r576c1024;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_1600) then
      Benchmark_KeccakF_1600;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_1600_P2_R24) then
      Benchmark_KeccakF_1600_P2_R24;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_1600_P2_R12) then
      Benchmark_KeccakF_1600_P2_R12;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_800) then
      Benchmark_KeccakF_800;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_400) then
      Benchmark_KeccakF_400;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_200) then
      Benchmark_KeccakF_200;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_100) then
      Benchmark_KeccakF_100;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_50) then
      Benchmark_KeccakF_50;
      Ada.Text_IO.New_Line;
   end if;
   
   if Benchmarks_Enabled (KeccakF_25) then
      Benchmark_KeccakF_25;
      Ada.Text_IO.New_Line;
   end if;
end Benchmark;
