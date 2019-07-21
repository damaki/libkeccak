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

--  @summary
--  An optimised implementation of the Keccak-f permutation.
--
--  @description
--  This implementation of Keccak-f is based on the optimised implementation
--  from the Keccak code package.
--
--  @group Keccak-f
generic
package Keccak.Generic_KeccakF.Optimized_Permutation
is

   generic
      --  Number of rounds.
      --
      --  By default, the definition from The Keccak Reference is used.
      First_Round : Round_Index := 0;
      Num_Rounds  : Round_Count := 12 + (2 * L);
   procedure Permute (S : in out State)
     with Global => null;

end Keccak.Generic_KeccakF.Optimized_Permutation;
