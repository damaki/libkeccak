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
with Keccak.Generic_XOF;
with Keccak.Generic_Parallel_XOF;
with Keccak.Types;

generic

   with package XOF_Serial is new Keccak.Generic_XOF (<>);
   --  This XOF must be configured with NO SUFFIX BITS.
   --  The Generic_KangarooTwelve implementation takes care of the appropriate
   --  suffix bits when using this XOF_Serial.

   with package XOF_Parallel_2 is new Keccak.Generic_Parallel_XOF (<>);
   --  This XOF must be configured to add the 3 suffix bits 2#011#.

   with package XOF_Parallel_4 is new Keccak.Generic_Parallel_XOF (<>);
   --  This XOF must be configured to add the 3 suffix bits 2#011#.

   with package XOF_Parallel_8 is new Keccak.Generic_Parallel_XOF (<>);
   --  This XOF must be configured to add the 3 suffix bits 2#011#.

package Keccak.Generic_KangarooTwelve
is

   --  Assertions to check that the correct parallel instances have
   --  been provided.
   pragma Assert (XOF_Parallel_2.Num_Parallel_Instances = 2);
   pragma Assert (XOF_Parallel_4.Num_Parallel_Instances = 4);
   pragma Assert (XOF_Parallel_8.Num_Parallel_Instances = 8);


   Block_Size_Bytes : constant := 8192;
   --  Size of each parallel block.
   --  This is set to 8 kiB in the K12 documentation.

   type Context is private;


   type States is (Updating, Ready_To_Extract, Extracting);


   type Byte_Count is new Long_Long_Integer range 0 .. Long_Long_Integer'Last;


   procedure Init (Ctx : out Context)
     with Global => null,
     Post => State_Of (Ctx) = Updating;


   procedure Update (Ctx  : in out Context;
                     Data : in     Types.Byte_Array)
     with Global => null,
     Pre => (State_Of (Ctx) = Updating
             and Byte_Count (Data'Length) <= Max_Input_Length (Ctx)),
     Post => (State_Of (Ctx) = Updating
              and Num_Bytes_Processed (Ctx) =
                Num_Bytes_Processed (Ctx'Old) + Byte_Count (Data'Length));


   procedure Finish (Ctx           : in out Context;
                     Customization : in     String)
     with Global => null,
     Pre => (State_Of (Ctx) = Updating
             and then
              (Byte_Count (Customization'Length) + Byte_Count (Long_Long_Integer'Size / 8) + 2
              <= Max_Input_Length (Ctx))),
     Post => State_Of (Ctx) = Ready_To_Extract;


   procedure Extract (Ctx  : in out Context;
                      Data :    out Types.Byte_Array)
     with Global => null,
     Pre => State_Of (Ctx) in Ready_To_Extract | Extracting,
     Post => State_Of (Ctx) = Extracting;


   function State_Of (Ctx : in Context) return States
     with Global => null;


   function Num_Bytes_Processed (Ctx : in Context) return Byte_Count
     with Global => null;


   function Max_Input_Length (Ctx : in Context) return Byte_Count;

private

   CV_Size_Bytes    : constant := 256 / 8;
   --  The size of each chaining value (CV) in bytes.
   --  This is set to 256 bits (32 bytes) in the documentation.

   use type XOF_Serial.States;

   subtype Partial_Block_Length_Number is Natural range 0 .. Block_Size_Bytes - 1;


   type Context is record
      Outer_XOF            : XOF_Serial.Context;
      Partial_Block_XOF    : XOF_Serial.Context;
      Input_Len            : Byte_Count;
      Partial_Block_Length : Partial_Block_Length_Number;
      Finished             : Boolean;
   end record;


   function State_Of (Ctx : in Context) return States
   is (if (XOF_Serial.State_Of (Ctx.Outer_XOF) = XOF_Serial.Updating
           and XOF_Serial.State_Of (Ctx.Partial_Block_XOF) = XOF_Serial.Updating)
       then (if Ctx.Finished
             then Ready_To_Extract
             else Updating)

       elsif XOF_Serial.State_Of (Ctx.Outer_XOF) = XOF_Serial.Ready_To_Extract
       then Ready_To_Extract

       else Extracting);


   function Num_Bytes_Processed (Ctx : in Context) return Byte_Count
   is (Ctx.Input_Len);


   function Max_Input_Length (Ctx : in Context) return Byte_Count
   is (Byte_Count'Last - Num_Bytes_Processed (Ctx));


end Keccak.Generic_KangarooTwelve;
