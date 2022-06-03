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

with Ketje_Tests;
with Ketje;
with AUnit.Test_Caller;

package body Ketje_Suite
is

   package Ketje_Jr_Tests is new Ketje_Tests (Ketje.Jr);
   package Ketje_Sr_Tests is new Ketje_Tests (Ketje.Jr);
   package Ketje_Minor_Tests is new Ketje_Tests (Ketje.Minor);
   package Ketje_Major_Tests is new Ketje_Tests (Ketje.Major);

   package Caller_Jr is new AUnit.Test_Caller (Ketje_Jr_Tests.Test);
   package Caller_Sr is new AUnit.Test_Caller (Ketje_Sr_Tests.Test);
   package Caller_Minor is new AUnit.Test_Caller (Ketje_Minor_Tests.Test);
   package Caller_Major is new AUnit.Test_Caller (Ketje_Major_Tests.Test);

   function Suite return Access_Test_Suite
   is

      Ret : constant Access_Test_Suite := new Test_Suite;
   begin
      --  KetjeJr
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test encrypt -> decrypt loopback",
             Ketje_Jr_Tests.Test_Encrypt_Decrypt'Access));
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test streaming AAD",
             Ketje_Jr_Tests.Test_Streaming_AAD'Access));
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test streaming encryption",
             Ketje_Jr_Tests.Test_Streaming_Encryption'Access));
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test streaming decryption",
             Ketje_Jr_Tests.Test_Streaming_Decryption'Access));
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test streaming tag",
             Ketje_Jr_Tests.Test_Streaming_Tag'Access));
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test Verify_Tag",
             Ketje_Jr_Tests.Test_Verify_Tag'Access));
      Ret.Add_Test
         (Caller_Jr.Create
            ("KetjeJr: Test streaming Verify_Tag",
             Ketje_Jr_Tests.Test_Streaming_Verify_Tag'Access));

      --  KetjeSr
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test encrypt -> decrypt loopback",
             Ketje_Sr_Tests.Test_Encrypt_Decrypt'Access));
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test streaming AAD",
             Ketje_Sr_Tests.Test_Streaming_AAD'Access));
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test streaming encryption",
             Ketje_Sr_Tests.Test_Streaming_Encryption'Access));
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test streaming decryption",
             Ketje_Sr_Tests.Test_Streaming_Decryption'Access));
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test streaming tag",
             Ketje_Sr_Tests.Test_Streaming_Tag'Access));
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test Verify_Tag",
             Ketje_Sr_Tests.Test_Verify_Tag'Access));
      Ret.Add_Test
         (Caller_Sr.Create
            ("KetjeSr: Test streaming Verify_Tag",
             Ketje_Sr_Tests.Test_Streaming_Verify_Tag'Access));

      --  KetjeMinor
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test encrypt -> decrypt loopback",
             Ketje_Minor_Tests.Test_Encrypt_Decrypt'Access));
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test streaming AAD",
             Ketje_Minor_Tests.Test_Streaming_AAD'Access));
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test streaming encryption",
             Ketje_Minor_Tests.Test_Streaming_Encryption'Access));
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test streaming decryption",
             Ketje_Minor_Tests.Test_Streaming_Decryption'Access));
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test streaming tag",
             Ketje_Minor_Tests.Test_Streaming_Tag'Access));
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test Verify_Tag",
             Ketje_Minor_Tests.Test_Verify_Tag'Access));
      Ret.Add_Test
         (Caller_Minor.Create
            ("KetjeMinor: Test streaming Verify_Tag",
             Ketje_Minor_Tests.Test_Streaming_Verify_Tag'Access));

      --  KetjeMajor
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test encrypt -> decrypt loopback",
             Ketje_Major_Tests.Test_Encrypt_Decrypt'Access));
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test streaming AAD",
             Ketje_Major_Tests.Test_Streaming_AAD'Access));
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test streaming encryption",
             Ketje_Major_Tests.Test_Streaming_Encryption'Access));
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test streaming decryption",
             Ketje_Major_Tests.Test_Streaming_Decryption'Access));
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test streaming tag",
             Ketje_Major_Tests.Test_Streaming_Tag'Access));
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test Verify_Tag",
             Ketje_Major_Tests.Test_Verify_Tag'Access));
      Ret.Add_Test
         (Caller_Major.Create
            ("KetjeMajor: Test streaming Verify_Tag",
             Ketje_Major_Tests.Test_Streaming_Verify_Tag'Access));

      return Ret;
   end Suite;

end Ketje_Suite;
