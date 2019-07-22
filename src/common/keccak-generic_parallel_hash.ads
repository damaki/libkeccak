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

--  @summary
--  Generic implementation of the ParallelHash algorithm as defined in NIST SP 800-185.
--
--  @description
--  The purpose of ParallelHash10 is to support the efficient hashing of very long
--  strings, by taking advantage of the parallelism available in modern processors.
--  ParallelHash provides variable-length output. Changing any input parameter to
--  ParallelHash, even the requested output length, will result in unrelated output.
--  Like the other functions defined in this document, ParallelHash also supports
--  user-selected customization strings.
--
--  The block size defines the size of each input block that is processed in parallel.
--  It can be any positive value up to Block_Size_Number'Last / 8. Since Block_Size_Number
--  is defined as a subtype of Positive, this guaranteeds the maximum block size to
--  be at least 4095 bytes when Integer is at least a 16-bit signed type. On most systems
--  is likely to be at least 268_435_455 bytes (256 MiB - 1 B) when Integer is a
--  32-bit signed type.
--
--  This API is used as follows:
--
--  1 Call Init to initialise a new context. The block size and an optional
--    customisation string (for domain separation) are provided here.
--
--  2 Call Update one or more times to input an arbitrary amount of data into
--    the ParallelHash context.
--
--  3 Call either Finish or Extract to produce the desired type of output
--    (ParallelHash or ParallelHashXOF):
--
--  * Finish is used to produce a single output of arbitrary length (ParallelHash).
--    The requested output length affects the output. For example, requesting
--    a 10-byte output will produce an unrelated hash to requesting a 20-byte
--    output.
--
--  * Extract can be called one or more times to produce an arbitrary number
--    of output bytes (ParallelHashXOF). In this case, the total output length is
--    unknown in advance so the output does not change based on the overall length.
--    For example, a 10-byte output is the truncated version of a 20-byte output.
--
--  @group ParallelHash
generic

   CV_Size_Bytes : Positive;
   --  Length of the Chaining Values, in bytes.

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
   --  @value Updating When in this state additional data can be input into the
   --    ParallelHash context.
   --
   --  @value Extracting When in this state, the ParallelHash context can generate
   --    output bytes by calling the Extract procedure.
   --
   --  @value Finished When in this state the context is finished and no more data
   --    can be input or output.

   type Byte_Count is new Long_Long_Integer
   range 0 .. Long_Long_Integer'Last;

   subtype Block_Size_Number is Positive range 1 .. Positive'Last / 8;

   procedure Init (Ctx           :    out Context;
                   Block_Size    : in     Block_Size_Number;
                   Customization : in     String)
     with Global => null,
     Post => State_Of (Ctx) = Updating;
   --  Initialise the ParallelHash instance.
   --
   --  @param Ctx The context to initialise.
   --
   --  @param Block_Size The block size in bytes for parallel hashing.

   procedure Update (Ctx  : in out Context;
                     Data : in     Types.Byte_Array)
     with Global => null,
     Pre => (State_Of (Ctx) = Updating
             and Byte_Count (Data'Length) <= Max_Input_Length (Ctx)),
     Post => (State_Of (Ctx) = Updating
              and Num_Bytes_Processed (Ctx) =
                Num_Bytes_Processed (Ctx'Old) + Byte_Count (Data'Length));
   --  Process input bytes with ParallelHash.
   --
   --  This procedure is most efficient when the input Data buffer is
   --  at least 8 times larger than the ParallelHash block size.
   --
   --  This may be called multiple times to process a large amount of data.
   --
   --  Note that there is an overall limit to the maximum amount of data that
   --  can be processed with ParallelHash. The maximum input size is returned
   --  by calling the Max_Input_Length function.
   --
   --  @param Ctx The context to update with new input data.
   --
   --  @param Data The data to process with ParallelHash.

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
   --  Get the total number of bytes that have been input to the ParllelHash
   --  instance so far.
   --
   --  This restricts the maximum permitted input length to a maximum of
   --  Byte_Count'Last bytes.

   function Max_Input_Length (Ctx : in Context) return Byte_Count
     with Global => null;
   --  Gets the maximum number of bytes that may be input into the ParallelHash context.
   --
   --  This value decreases as the context is updated with additional input data.

   function Block_Size_Of (Ctx : in Context) return Block_Size_Number
     with Global => null;
   --  Get the configured block size parameter of the ParallelHash instance.

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

   function Block_Size_Of (Ctx : in Context) return Block_Size_Number
   is (Ctx.Block_Size);

end Keccak.Generic_Parallel_Hash;
