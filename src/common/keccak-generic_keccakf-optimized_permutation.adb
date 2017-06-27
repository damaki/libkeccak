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
with Interfaces; use Interfaces;

package body Keccak.Generic_KeccakF.Optimized_Permutation
is
   --  This implementation is ported from the Optimized implementation by
   --  the Keccak, Keyak, and Ketje teams provided in the Keccak Code Package.

   procedure Permute (S : in out State)
   is
      type Round_Constants is array(Round_Index) of Interfaces.Unsigned_64;

      RC : constant Round_Constants :=
        (
         16#0000_0000_0000_0001#,
         16#0000_0000_0000_8082#,
         16#8000_0000_0000_808A#,
         16#8000_0000_8000_8000#,
         16#0000_0000_0000_808B#,
         16#0000_0000_8000_0001#,
         16#8000_0000_8000_8081#,
         16#8000_0000_0000_8009#,
         16#0000_0000_0000_008A#,
         16#0000_0000_0000_0088#,
         16#0000_0000_8000_8009#,
         16#0000_0000_8000_000A#,
         16#0000_0000_8000_808B#,
         16#8000_0000_0000_008B#,
         16#8000_0000_0000_8089#,
         16#8000_0000_0000_8003#,
         16#8000_0000_0000_8002#,
         16#8000_0000_0000_0080#,
         16#0000_0000_0000_800A#,
         16#8000_0000_8000_000A#,
         16#8000_0000_8000_8081#,
         16#8000_0000_0000_8080#,
         16#0000_0000_8000_0001#,
         16#8000_0000_8000_8008#
        );

      Aba, Abe, Abi, Abo, Abu : Lane_Type;
      Aga, Age, Agi, Ago, Agu : Lane_Type;
      Aka, Ake, Aki, Ako, Aku : Lane_Type;
      Ama, Ame, Ami, Amo, Amu : Lane_Type;
      Asa, Ase, Asi, Aso, Asu : Lane_Type;
      Ca, Ce, Ci, Co, Cu      : Lane_Type;
      Eba, Ebe, Ebi, Ebo, Ebu : Lane_Type;
      Ega, Ege, Egi, Ego, Egu : Lane_Type;
      Eka, Eke, Eki, Eko, Eku : Lane_Type;
      Ema, Eme, Emi, Emo, Emu : Lane_Type;
      Esa, Ese, Esi, Eso, Esu : Lane_Type;

      procedure Copy_From_State
        with Global => (Input  => S,
                        Output => (Aba, Abe, Abi, Abo, Abu,
                                   Aga, Age, Agi, Ago, Agu,
                                   Aka, Ake, Aki, Ako, Aku,
                                   Ama, Ame, Ami, Amo, Amu,
                                   Asa, Ase, Asi, Aso, Asu)),
        Inline
      is
      begin
         Aba := S (0,0);
         Abe := S (1,0);
         Abi := S (2,0);
         Abo := S (3,0);
         Abu := S (4,0);
         Aga := S (0,1);
         Age := S (1,1);
         Agi := S (2,1);
         Ago := S (3,1);
         Agu := S (4,1);
         Aka := S (0,2);
         Ake := S (1,2);
         Aki := S (2,2);
         Ako := S (3,2);
         Aku := S (4,2);
         Ama := S (0,3);
         Ame := S (1,3);
         Ami := S (2,3);
         Amo := S (3,3);
         Amu := S (4,3);
         Asa := S (0,4);
         Ase := S (1,4);
         Asi := S (2,4);
         Aso := S (3,4);
         Asu := S (4,4);
      end Copy_From_State;


      procedure Copy_To_State_From_A
        with Global => (Input  => (Aba, Abe, Abi, Abo, Abu,
                                   Aga, Age, Agi, Ago, Agu,
                                   Aka, Ake, Aki, Ako, Aku,
                                   Ama, Ame, Ami, Amo, Amu,
                                   Asa, Ase, Asi, Aso, Asu),
                        Output => S),
        Inline
      is
      begin
         S := (0 => (0 => Aba,
                     1 => Aga,
                     2 => Aka,
                     3 => Ama,
                     4 => Asa),
               1 => (0 => Abe,
                     1 => Age,
                     2 => Ake,
                     3 => Ame,
                     4 => Ase),
               2 => (0 => Abi,
                     1 => Agi,
                     2 => Aki,
                     3 => Ami,
                     4 => Asi),
               3 => (0 => Abo,
                     1 => Ago,
                     2 => Ako,
                     3 => Amo,
                     4 => Aso),
               4 => (0 => Abu,
                     1 => Agu,
                     2 => Aku,
                     3 => Amu,
                     4 => Asu)
              );
      end Copy_To_State_From_A;


      procedure Copy_To_State_From_E
        with Global => (Input  => (Eba, Ebe, Ebi, Ebo, Ebu,
                                   Ega, Ege, Egi, Ego, Egu,
                                   Eka, Eke, Eki, Eko, Eku,
                                   Ema, Eme, Emi, Emo, Emu,
                                   Esa, Ese, Esi, Eso, Esu),
                        Output => S),
        Inline
      is
      begin
         S := (0 => (0 => Eba,
                     1 => Ega,
                     2 => Eka,
                     3 => Ema,
                     4 => Esa),
               1 => (0 => Ebe,
                     1 => Ege,
                     2 => Eke,
                     3 => Eme,
                     4 => Ese),
               2 => (0 => Ebi,
                     1 => Egi,
                     2 => Eki,
                     3 => Emi,
                     4 => Esi),
               3 => (0 => Ebo,
                     1 => Ego,
                     2 => Eko,
                     3 => Emo,
                     4 => Eso),
               4 => (0 => Ebu,
                     1 => Egu,
                     2 => Eku,
                     3 => Emu,
                     4 => Esu)
              );
      end Copy_To_State_From_E;

      procedure Prepare_Theta
        with Global => (Input  => (Aba, Abe, Abi, Abo, Abu,
                                   Aga, Age, Agi, Ago, Agu,
                                   Aka, Ake, Aki, Ako, Aku,
                                   Ama, Ame, Ami, Amo, Amu,
                                   Asa, Ase, Asi, Aso, Asu),
                        Output => (Ca, Ce, Ci, Co, Cu)),
          Inline
      is
      begin
         Ca := Aba xor Aga xor Aka xor Ama xor Asa;
         Ce := Abe xor Age xor Ake xor Ame xor Ase;
         Ci := Abi xor Agi xor Aki xor Ami xor Asi;
         Co := Abo xor Ago xor Ako xor Amo xor Aso;
         Cu := Abu xor Agu xor Aku xor Amu xor Asu;
      end Prepare_Theta;

      procedure Theta_Rho_Pi_Chi_Iota_Prepare_Theta_AtoE (RI : in Round_Index)
        with Global => (In_Out => (Aba, Abe, Abi, Abo, Abu,
                                   Aga, Age, Agi, Ago, Agu,
                                   Aka, Ake, Aki, Ako, Aku,
                                   Ama, Ame, Ami, Amo, Amu,
                                   Asa, Ase, Asi, Aso, Asu,
                                   Ca, Ce, Ci, Co, Cu),
                        Output => (Eba, Ebe, Ebi, Ebo, Ebu,
                                   Ega, Ege, Egi, Ego, Egu,
                                   Eka, Eke, Eki, Eko, Eku,
                                   Ema, Eme, Emi, Emo, Emu,
                                   Esa, Ese, Esi, Eso, Esu)),
        Inline
      is
         Da, De, Di, D0, Du : Lane_Type;

         Bba, Bbe, Bbi, Bbo, Bbu : Lane_Type;
         Bga, Bge, Bgi, Bgo, Bgu : Lane_Type;
         Bka, Bke, Bki, Bko, Bku : Lane_Type;
         Bma, Bme, Bmi, Bmo, Bmu : Lane_Type;
         Bsa, Bse, Bsi, Bso, Bsu : Lane_Type;

      begin
         Da := Cu xor Rotate_Left(Ce, 1);
         De := Ca xor Rotate_Left(Ci, 1);
         Di := Ce xor Rotate_Left(Co, 1);
         D0 := Ci xor Rotate_Left(Cu, 1);
         Du := Co xor Rotate_Left(Ca, 1);

         Aba := Aba xor Da;
         Bba := Aba;
         Age := Age xor De;
         Bbe := Rotate_Left(Age, 300 mod W);
         Aki := Aki xor Di;
         Bbi := Rotate_Left(Aki, 171 mod W);
         Amo := Amo xor D0;
         Bbo := Rotate_Left(Amo, 21 mod W);
         Asu := Asu xor Du;
         Bbu := Rotate_Left(Asu, 78 mod W);
         Eba := Bba  xor ((not Bbe) and   Bbi );
         Eba := Eba xor Lane_Type(RC(RI) and (2**W - 1));
         Ca  := Eba;
         Ebe := Bbe xor ((not Bbi) and Bbo );
         Ce  := Ebe;
         Ebi := Bbi xor ((not Bbo) and Bbu );
         Ci  := Ebi;
         Ebo := Bbo xor ((not Bbu) and Bba );
         Co  := Ebo;
         Ebu := Bbu xor ((not Bba) and Bbe );
         Cu  := Ebu;

         Abo := Abo xor D0;
         Bga := Rotate_Left(Abo, 28 mod W);
         Agu := Agu xor Du;
         Bge := Rotate_Left(Agu, 276 mod W);
         Aka := Aka xor Da;
         Bgi := Rotate_Left(Aka, 3 mod W);
         Ame := Ame xor De;
         Bgo := Rotate_Left(Ame, 45 mod W);
         Asi := Asi xor Di;
         Bgu := Rotate_Left(Asi, 253 mod W);
         Ega := Bga xor ((not Bge) and Bgi );
         Ca  := Ca  xor Ega;
         Ege := Bge xor ((not Bgi) and Bgo );
         Ce  := Ce  xor Ege;
         Egi := Bgi xor ((not Bgo) and Bgu );
         Ci  := Ci  xor Egi;
         Ego := Bgo xor ((not Bgu) and Bga );
         Co  := Co  xor Ego;
         Egu := Bgu xor ((not Bga) and Bge );
         Cu  := Cu  xor Egu;

         Abe := Abe xor De;
         Bka := Rotate_Left(Abe, 1 mod W);
         Agi := Agi xor Di;
         Bke := Rotate_Left(Agi, 6 mod W);
         Ako := Ako xor D0;
         Bki := Rotate_Left(Ako, 153 mod W);
         Amu := Amu xor Du;
         Bko := Rotate_Left(Amu, 136 mod W);
         Asa := Asa xor Da;
         Bku := Rotate_Left(Asa, 210 mod W);
         Eka := Bka xor ((not Bke) and Bki );
         Ca  := Ca  xor Eka;
         Eke := Bke xor ((not Bki) and Bko );
         Ce  := Ce  xor Eke;
         Eki := Bki xor ((not Bko) and Bku );
         Ci  := Ci  xor Eki;
         Eko := Bko xor ((not Bku) and Bka );
         Co  := Co  xor Eko;
         Eku := Bku xor ((not Bka) and Bke );
         Cu  := Cu  xor Eku;

         Abu := Abu xor Du;
         Bma := Rotate_Left(Abu, 91 mod W);
         Aga := Aga xor Da;
         Bme := Rotate_Left(Aga, 36 mod W);
         Ake := Ake xor De;
         Bmi := Rotate_Left(Ake, 10 mod W);
         Ami := Ami xor Di;
         Bmo := Rotate_Left(Ami, 15 mod W);
         Aso := Aso xor D0;
         Bmu := Rotate_Left(Aso, 120 mod W);
         Ema := Bma xor ((not Bme) and Bmi );
         Ca  := Ca  xor Ema;
         Eme := Bme xor ((not Bmi) and Bmo );
         Ce  := Ce  xor Eme;
         Emi := Bmi xor ((not Bmo) and Bmu );
         Ci  := Ci  xor Emi;
         Emo := Bmo xor ((not Bmu) and Bma );
         Co  := Co  xor Emo;
         Emu := Bmu xor ((not Bma) and Bme );
         Cu  := Cu  xor Emu;

         Abi := Abi xor Di;
         Bsa := Rotate_Left(Abi, 190 mod W);
         Ago := Ago xor D0;
         Bse := Rotate_Left(Ago, 55 mod W);
         Aku := Aku xor Du;
         Bsi := Rotate_Left(Aku, 231 mod W);
         Ama := Ama xor Da;
         Bso := Rotate_Left(Ama, 105 mod W);
         Ase := Ase xor De;
         Bsu := Rotate_Left(Ase, 66 mod W);
         Esa := Bsa xor ((not Bse) and Bsi );
         Ca  := Ca  xor Esa;
         Ese := Bse xor ((not Bsi) and Bso );
         Ce  := Ce  xor Ese;
         Esi := Bsi xor ((not Bso) and Bsu );
         Ci  := Ci  xor Esi;
         Eso := Bso xor ((not Bsu) and Bsa );
         Co  := Co  xor Eso;
         Esu := Bsu xor ((not Bsa) and Bse );
         Cu  := Cu  xor Esu;

      end Theta_Rho_Pi_Chi_Iota_Prepare_Theta_AtoE;

      procedure Theta_Rho_Pi_Chi_Iota_Prepare_Theta_EtoA (RI : in Round_Index)
        with Global => (In_Out => (Eba, Ebe, Ebi, Ebo, Ebu,
                                   Ega, Ege, Egi, Ego, Egu,
                                   Eka, Eke, Eki, Eko, Eku,
                                   Ema, Eme, Emi, Emo, Emu,
                                   Esa, Ese, Esi, Eso, Esu,
                                   Ca, Ce, Ci, Co, Cu),
                        Output => (Aba, Abe, Abi, Abo, Abu,
                                   Aga, Age, Agi, Ago, Agu,
                                   Aka, Ake, Aki, Ako, Aku,
                                   Ama, Ame, Ami, Amo, Amu,
                                   Asa, Ase, Asi, Aso, Asu)),
        Inline
      is
         Da, De, Di, D0, Du : Lane_Type;

         Bba, Bbe, Bbi, Bbo, Bbu : Lane_Type;
         Bga, Bge, Bgi, Bgo, Bgu : Lane_Type;
         Bka, Bke, Bki, Bko, Bku : Lane_Type;
         Bma, Bme, Bmi, Bmo, Bmu : Lane_Type;
         Bsa, Bse, Bsi, Bso, Bsu : Lane_Type;

      begin
         Da := Cu xor Rotate_Left(Ce, 1);
         De := Ca xor Rotate_Left(Ci, 1);
         Di := Ce xor Rotate_Left(Co, 1);
         D0 := Ci xor Rotate_Left(Cu, 1);
         Du := Co xor Rotate_Left(Ca, 1);

         Eba := Eba xor Da;
         Bba := Eba;
         Ege := Ege xor De;
         Bbe := Rotate_Left(Ege, 300 mod W);
         Eki := Eki xor Di;
         Bbi := Rotate_Left(Eki, 171 mod W);
         Emo := Emo xor D0;
         Bbo := Rotate_Left(Emo, 21 mod W);
         Esu := Esu xor Du;
         Bbu := Rotate_Left(Esu, 78 mod W);
         Aba := Bba xor ((not Bbe) and Bbi );
         Aba := Aba xor Lane_Type(RC(RI) and (2**W - 1));
         Ca  := Aba;
         Abe := Bbe xor ((not Bbi) and Bbo );
         Ce  := Abe;
         Abi := Bbi xor ((not Bbo) and Bbu );
         Ci  := Abi;
         Abo := Bbo xor ((not Bbu) and Bba );
         Co  := Abo;
         Abu := Bbu xor ((not Bba) and Bbe );
         Cu  := Abu;

         Ebo := Ebo xor D0;
         Bga := Rotate_Left(Ebo, 28 mod W);
         Egu := Egu xor Du;
         Bge := Rotate_Left(Egu, 276 mod W);
         Eka := Eka xor Da;
         Bgi := Rotate_Left(Eka, 3 mod W);
         Eme := Eme xor De;
         Bgo := Rotate_Left(Eme, 45 mod W);
         Esi := Esi xor Di;
         Bgu := Rotate_Left(Esi, 253 mod W);
         Aga := Bga xor ((not Bge) and Bgi );
         Ca  := Ca  xor Aga;
         Age := Bge xor ((not Bgi) and Bgo );
         Ce  := Ce  xor Age;
         Agi := Bgi xor ((not Bgo) and Bgu );
         Ci  := Ci  xor Agi;
         Ago := Bgo xor ((not Bgu) and Bga );
         Co  := Co  xor Ago;
         Agu := Bgu xor ((not Bga) and Bge );
         Cu  := Cu  xor Agu;

         Ebe := Ebe xor De;
         Bka := Rotate_Left(Ebe, 1 mod W);
         Egi := Egi xor Di;
         Bke := Rotate_Left(Egi, 6 mod W);
         Eko := Eko xor D0;
         Bki := Rotate_Left(Eko, 153 mod W);
         Emu := Emu xor Du;
         Bko := Rotate_Left(Emu, 136 mod W);
         Esa := Esa xor Da;
         Bku := Rotate_Left(Esa, 210 mod W);
         Aka := Bka xor ((not Bke) and Bki );
         Ca  := Ca  xor Aka;
         Ake := Bke xor ((not Bki) and Bko );
         Ce  := Ce  xor Ake;
         Aki := Bki xor ((not Bko) and Bku );
         Ci  := Ci  xor Aki;
         Ako := Bko xor ((not Bku) and Bka );
         Co  := Co  xor Ako;
         Aku := Bku xor ((not Bka) and Bke );
         Cu  := Cu  xor Aku;

         Ebu := Ebu xor Du;
         Bma := Rotate_Left(Ebu, 91 mod W);
         Ega := Ega xor Da;
         Bme := Rotate_Left(Ega, 36 mod W);
         Eke := Eke xor De;
         Bmi := Rotate_Left(Eke, 10 mod W);
         Emi := Emi xor Di;
         Bmo := Rotate_Left(Emi, 15 mod W);
         Eso := Eso xor D0;
         Bmu := Rotate_Left(Eso, 120 mod W);
         Ama := Bma xor ((not Bme) and Bmi );
         Ca  := Ca  xor Ama;
         Ame := Bme xor ((not Bmi) and Bmo );
         Ce  := Ce  xor Ame;
         Ami := Bmi xor ((not Bmo) and Bmu );
         Ci  := Ci  xor Ami;
         Amo := Bmo xor ((not Bmu) and Bma );
         Co  := Co  xor Amo;
         Amu := Bmu xor ((not Bma) and Bme );
         Cu  := Cu  xor Amu;

         Ebi := Ebi xor Di;
         Bsa := Rotate_Left(Ebi, 190 mod W);
         Ego := Ego xor D0;
         Bse := Rotate_Left(Ego, 55 mod W);
         Eku := Eku xor Du;
         Bsi := Rotate_Left(Eku, 231 mod W);
         Ema := Ema xor Da;
         Bso := Rotate_Left(Ema, 105 mod W);
         Ese := Ese xor De;
         Bsu := Rotate_Left(Ese, 66 mod W);
         Asa := Bsa xor ((not Bse) and Bsi );
         Ca  := Ca  xor Asa;
         Ase := Bse xor ((not Bsi) and Bso );
         Ce  := Ce  xor Ase;
         Asi := Bsi xor ((not Bso) and Bsu );
         Ci  := Ci  xor Asi;
         Aso := Bso xor ((not Bsu) and Bsa );
         Co  := Co  xor Aso;
         Asu := Bsu xor ((not Bsa) and Bse );
         Cu  := Cu  xor Asu;

      end Theta_Rho_Pi_Chi_Iota_Prepare_Theta_EtoA;

   begin

      Copy_From_State;

      Prepare_Theta;

      for RI in 0 .. (Num_Rounds / 2) - 1 loop
         pragma Warnings
           (GNATprove, Off,
            "unused assignment to ""A",
            Reason => "Axx variables are also re-used as temporaries");

         Theta_Rho_Pi_Chi_Iota_Prepare_Theta_AtoE (First_Round + Round_Index (RI * 2));

         pragma Warnings (GNATprove, On);

         pragma Warnings
           (GNATprove, Off,
            "unused assignment to ""E",
            Reason => "Exx variables are also re-used as temporaries");

         Theta_Rho_Pi_Chi_Iota_Prepare_Theta_EtoA (First_Round + Round_Index (RI * 2) + 1);

         pragma Warnings (GNATprove, On);
      end loop;

      if Num_Rounds mod 2 /= 0 then
         --  Number of rounds is an odd number, so we need to do the final step.

         pragma Warnings
           (GNATprove, Off,
            "unused assignment to ""C",
            Reason => "Cx variables are no longer needed");

         Theta_Rho_Pi_Chi_Iota_Prepare_Theta_AtoE (First_Round + Round_Index (Num_Rounds - 1));

         pragma Warnings (GNATprove, On);

         Copy_To_State_From_E;

      else
         Copy_To_State_From_A;

      end if;

   end Permute;

end Keccak.Generic_KeccakF.Optimized_Permutation;
