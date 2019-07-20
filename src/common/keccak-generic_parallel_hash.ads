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
with Keccak.Generic_CSHAKE;
with Keccak.Generic_XOF;
with Keccak.Generic_Parallel_XOF;
with Keccak.Types;

generic

   CV_Size_Bytes : Positive;

   with package CSHAKE_Serial is new Keccak.Generic_CSHAKE (<>);
   --  This CSHAKE must be configured with NO SUFFIX BITS.
   --  The Generic_KangarooTwelve implementation takes care of the appropriate
   --  suffix bits when using this CSHAKE_Serial.

   with package SHAKE_Serial is new Keccak.Generic_XOF (<>);

   with package SHAKE_Parallel_2 is new Keccak.Generic_Parallel_XOF (<>);
   --  This CSHAKE must be configured to add the 3 suffix bits 2#011#.

   with package SHAKE_Parallel_4 is new Keccak.Generic_Parallel_XOF (<>);
   --  This CSHAKE must be configured to add the 3 suffix bits 2#011#.

   with package SHAKE_Parallel_8 is new Keccak.Generic_Parallel_XOF (<>);
   --  This CSHAKE must be configured to add the 3 suffix bits 2#011#.

package Keccak.Generic_Parallel_Hash
is

   --  Assertions to check that the correct parallel instances have
   --  been provided.
   pragma Assert (SHAKE_Parallel_2.Num_Parallel_Instances = 2);
   pragma Assert (SHAKE_Parallel_4.Num_Parallel_Instances = 4);
   pragma Assert (SHAKE_Parallel_8.Num_Parallel_Instances = 8);


   type Context is private;


   type States is (Updating, Extracting, Finished);


   type Byte_Count is new Long_Long_Integer
   range 0 .. Long_Long_Integer'Last;


   subtype Block_Size_Number is Positive range 1 .. Positive'Last / 8;


   procedure Init (Ctx           :    out Context;
                   Block_Size    : in     Block_Size_Number;
                   Customization : in     String)
     with Global => null,
     Post => State_Of (Ctx) = Updating;


   procedure Update (Ctx  : in out Context;
                     Data : in     Types.Byte_Array)
     with Global => null,
     Pre => (State_Of (Ctx) = Updating
             and Byte_Count (Data'Length) <= Max_Input_Length (Ctx)),
     Post => (State_Of (Ctx) = Updating
              and Num_Bytes_Processed (Ctx) = Num_Bytes_Processed (Ctx'Old) + Byte_Count (Data'Length));


   procedure Finish (Ctx  : in out Context;
                     Data :    out Types.Byte_Array)
     with Global => null,
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Finished;
   --  Extract a fixed number of output bytes.
   --
   --  This procedure finalizes the ParallelHash and outputs a fixed number
   --  of output bytes. The ParallelHash parameter L is the requested output
   --  length, and is determined by the length of the @Data@ array.
   --  I.e. Data'Length is used as the ParallelHash parameter L.
   --
   --  After calling this procedure the ParallelHash instance cannot be used
   --  further.


   procedure Extract (Ctx  : in out Context;
                      Data :    out Types.Byte_Array)
     with Global => null,
     Pre => State_Of (Ctx) in Updating | Extracting,
     Post => State_Of (Ctx) = Extracting;
   --  Extract an arbitrary number of output bytes.
   --
   --  This procedure finalizes the ParallelHash and puts it into XOF mode
   --  where an arbitary number of bytes can be output. When this parameter
   --  is called for the first time after inputting data, the value 0 is used
   --  as the ParallelHash parameter L.
   --
   --  This procedure can be called multiple times to produce any output length.


   function State_Of (Ctx : in Context) return States
     with Global => null;


   function Num_Bytes_Processed (Ctx : in Context) return Byte_Count
     with Global => null;


   function Max_Input_Length (Ctx : in Context) return Byte_Count
     with Global => null;


   function Block_Size (Ctx : in Context) return Block_Size_Number
     with Global => null;

private

   use type CSHAKE_Serial.States;
   use type SHAKE_Serial.States;


   type Context is record
      Outer_CSHAKE         : CSHAKE_Serial.Context;
      Partial_Block_CSHAKE : SHAKE_Serial.Context;
      Input_Len            : Byte_Count;
      Block_Size           : Block_Size_Number;
      Partial_Block_Length : Natural;
      Finished             : Boolean;
   end record
     with Predicate => Context.Partial_Block_Length < Context.Block_Size;


   function State_Of (Ctx : in Context) return States
   is (if (Ctx.Finished
           or SHAKE_Serial.State_Of (Ctx.Partial_Block_CSHAKE) /= SHAKE_Serial.Updating
           or CSHAKE_Serial.State_Of (Ctx.Outer_CSHAKE)          = CSHAKE_Serial.Ready_To_Extract)
       then Finished

       elsif CSHAKE_Serial.State_Of (Ctx.Outer_CSHAKE) = CSHAKE_Serial.Updating
       then Updating

       else Extracting);


   function Num_Bytes_Processed (Ctx : in Context) return Byte_Count
   is (Ctx.Input_Len);


   function Max_Input_Length (Ctx : in Context) return Byte_Count
   is (Byte_Count'Last - Num_Bytes_Processed (Ctx));


   function Block_Size (Ctx : in Context) return Block_Size_Number
   is (Ctx.Block_Size);


end Keccak.Generic_Parallel_Hash;
