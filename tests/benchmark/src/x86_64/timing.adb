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

with System.Machine_Code; use System.Machine_Code;

package body Timing
is

   Measurement_Overhead : Cycles_Count;

   function RDTSC return Cycles_Count
   is
      L, H : Unsigned_32;
   begin
      Asm ("rdtsc",
           Outputs => (Unsigned_32'Asm_Output ("=a", L),
                       Unsigned_32'Asm_Output ("=d", H)),
           Volatile => True);

      return Shift_Left (Unsigned_64 (H), 32) or Unsigned_64 (L);
   end RDTSC;

   procedure Calibrate
   is
      T    : Time;
      Diff : Cycles_Count;
      Min  : Cycles_Count;

   begin
      Measurement_Overhead := 0;

      Start_Measurement (T);
      Min := End_Measurement (T);

      for N in 1 .. 100 loop
         Start_Measurement (T);
         Diff := End_Measurement (T);

         --  Keep the minimum
         if Diff < Min then
            Min := Diff;
         end if;
      end loop;

      Measurement_Overhead := Min;
   end Calibrate;

   procedure Start_Measurement (T : out Time)
   is
   begin
      T := Time (RDTSC);
   end Start_Measurement;

   function End_Measurement (T : in Time) return Cycles_Count
   is
      End_Time : Cycles_Count;

   begin
      End_Time := RDTSC;

      return (End_Time - Cycles_Count (T)) - Measurement_Overhead;
   end End_Measurement;

begin
   Calibrate;
end Timing;
