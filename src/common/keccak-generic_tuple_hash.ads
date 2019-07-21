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
with Keccak.Types;          use Keccak.Types;

--  @group TupleHash
generic
   with package CSHAKE is new Generic_CSHAKE (<>);
package Keccak.Generic_Tuple_Hash
is

   type Context is private;

   type States is (Updating, Extracting, Finished);

   procedure Init (Ctx           :    out Context;
                   Customization : in     String := "")
     with Global => null,
     Depends => (Ctx => Customization),
     Post => State_Of (Ctx) = Updating;

   procedure Update_Tuple_Item (Ctx  : in out Context;
                                Item : in     Byte_Array)
     with Global => null,
     Depends => (Ctx =>+ Item),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Updating;

   procedure Finish (Ctx     : in out Context;
                     Digest  :    out Byte_Array)
     with Global => null,
     Depends => ((Ctx, Digest) => (Ctx, Digest)),
     Pre => State_Of (Ctx) = Updating,
     Post => State_Of (Ctx) = Finished;

   procedure Extract (Ctx    : in out Context;
                      Digest :    out Byte_Array)
     with Global => null,
     Depends => ((Ctx, Digest) => (Ctx, Digest)),
     Pre => State_Of (Ctx) in Updating | Extracting,
     Post => State_Of (Ctx) = Extracting;

   function State_Of (Ctx : in Context) return States
     with Global => null;

private
   use type CSHAKE.States;

   type Context is record
      Ctx      : CSHAKE.Context;
      Finished : Boolean;
   end record;

   function State_Of (Ctx : in Context) return States
   is (if Ctx.Finished then Finished
       elsif CSHAKE.State_Of (Ctx.Ctx) = CSHAKE.Updating then Updating
       else Extracting);

end Keccak.Generic_Tuple_Hash;
