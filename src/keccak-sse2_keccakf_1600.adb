with Interfaces; use Interfaces;

package body Keccak.SSE2_KeccakF_1600
is
   W : constant := 64;

   function Bytes_To_Lane (Data   : in Types.Byte_Array;
                           Offset : in Natural) return Unsigned_64
   is (Unsigned_64 (Data (Data'First + Offset))
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 1)), 8)
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 2)), 16)
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 3)), 24)
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 4)), 32)
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 5)), 40)
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 6)), 48)
       or Shift_Left (Unsigned_64 (Data (Data'First + Offset + 7)), 56))
   with Inline;


   procedure Init (S : out Parallel_State)
   is
   begin
      S := (others => (others => (others => 0)));
   end Init;


   procedure Permute_All (S : in out Parallel_State)
   is
      type Round_Constants is array(Round_Index) of V2DI_View;

      RC : constant Round_Constants :=
        (
           (16#0000_0000_0000_0001#,16#0000_0000_0000_0001#),
           (16#0000_0000_0000_8082#,16#0000_0000_0000_8082#),
           (16#8000_0000_0000_808A#,16#8000_0000_0000_808A#),
           (16#8000_0000_8000_8000#,16#8000_0000_8000_8000#),
           (16#0000_0000_0000_808B#,16#0000_0000_0000_808B#),
           (16#0000_0000_8000_0001#,16#0000_0000_8000_0001#),
           (16#8000_0000_8000_8081#,16#8000_0000_8000_8081#),
           (16#8000_0000_0000_8009#,16#8000_0000_0000_8009#),
           (16#0000_0000_0000_008A#,16#0000_0000_0000_008A#),
           (16#0000_0000_0000_0088#,16#0000_0000_0000_0088#),
           (16#0000_0000_8000_8009#,16#0000_0000_8000_8009#),
           (16#0000_0000_8000_000A#,16#0000_0000_8000_000A#),
           (16#0000_0000_8000_808B#,16#0000_0000_8000_808B#),
           (16#8000_0000_0000_008B#,16#8000_0000_0000_008B#),
           (16#8000_0000_0000_8089#,16#8000_0000_0000_8089#),
           (16#8000_0000_0000_8003#,16#8000_0000_0000_8003#),
           (16#8000_0000_0000_8002#,16#8000_0000_0000_8002#),
           (16#8000_0000_0000_0080#,16#8000_0000_0000_0080#),
           (16#0000_0000_0000_800A#,16#0000_0000_0000_800A#),
           (16#8000_0000_8000_000A#,16#8000_0000_8000_000A#),
           (16#8000_0000_8000_8081#,16#8000_0000_8000_8081#),
           (16#8000_0000_0000_8080#,16#8000_0000_0000_8080#),
           (16#0000_0000_8000_0001#,16#0000_0000_8000_0001#),
           (16#8000_0000_8000_8008#,16#8000_0000_8000_8008#)
        );

      Aba, Abe, Abi, Abo, Abu : V2DI;
      Aga, Age, Agi, Ago, Agu : V2DI;
      Aka, Ake, Aki, Ako, Aku : V2DI;
      Ama, Ame, Ami, Amo, Amu : V2DI;
      Asa, Ase, Asi, Aso, Asu : V2DI;
      Ca, Ce, Ci, Co, Cu      : V2DI;
      Eba, Ebe, Ebi, Ebo, Ebu : V2DI;
      Ega, Ege, Egi, Ego, Egu : V2DI;
      Eka, Eke, Eki, Eko, Eku : V2DI;
      Ema, Eme, Emi, Emo, Emu : V2DI;
      Esa, Ese, Esi, Eso, Esu : V2DI;

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
         Aba := Load (S (0,0));
         Abe := Load (S (1,0));
         Abi := Load (S (2,0));
         Abo := Load (S (3,0));
         Abu := Load (S (4,0));
         Aga := Load (S (0,1));
         Age := Load (S (1,1));
         Agi := Load (S (2,1));
         Ago := Load (S (3,1));
         Agu := Load (S (4,1));
         Aka := Load (S (0,2));
         Ake := Load (S (1,2));
         Aki := Load (S (2,2));
         Ako := Load (S (3,2));
         Aku := Load (S (4,2));
         Ama := Load (S (0,3));
         Ame := Load (S (1,3));
         Ami := Load (S (2,3));
         Amo := Load (S (3,3));
         Amu := Load (S (4,3));
         Asa := Load (S (0,4));
         Ase := Load (S (1,4));
         Asi := Load (S (2,4));
         Aso := Load (S (3,4));
         Asu := Load (S (4,4));
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
         S := (0 => (0 => Store (Aba),
                     1 => Store (Aga),
                     2 => Store (Aka),
                     3 => Store (Ama),
                     4 => Store (Asa)),
               1 => (0 => Store (Abe),
                     1 => Store (Age),
                     2 => Store (Ake),
                     3 => Store (Ame),
                     4 => Store (Ase)),
               2 => (0 => Store (Abi),
                     1 => Store (Agi),
                     2 => Store (Aki),
                     3 => Store (Ami),
                     4 => Store (Asi)),
               3 => (0 => Store (Abo),
                     1 => Store (Ago),
                     2 => Store (Ako),
                     3 => Store (Amo),
                     4 => Store (Aso)),
               4 => (0 => Store (Abu),
                     1 => Store (Agu),
                     2 => Store (Aku),
                     3 => Store (Amu),
                     4 => Store (Asu))
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
         S := (0 => (0 => Store (Eba),
                     1 => Store (Ega),
                     2 => Store (Eka),
                     3 => Store (Ema),
                     4 => Store (Esa)),
               1 => (0 => Store (Ebe),
                     1 => Store (Ege),
                     2 => Store (Eke),
                     3 => Store (Eme),
                     4 => Store (Ese)),
               2 => (0 => Store (Ebi),
                     1 => Store (Egi),
                     2 => Store (Eki),
                     3 => Store (Emi),
                     4 => Store (Esi)),
               3 => (0 => Store (Ebo),
                     1 => Store (Ego),
                     2 => Store (Eko),
                     3 => Store (Emo),
                     4 => Store (Eso)),
               4 => (0 => Store (Ebu),
                     1 => Store (Egu),
                     2 => Store (Eku),
                     3 => Store (Emu),
                     4 => Store (Esu))
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
                                   Esa, Ese, Esi, Eso, Esu))
      is
         Da, De, Di, D0, Du : V2DI;

         Bba, Bbe, Bbi, Bbo, Bbu : V2DI;
         Bga, Bge, Bgi, Bgo, Bgu : V2DI;
         Bka, Bke, Bki, Bko, Bku : V2DI;
         Bma, Bme, Bmi, Bmo, Bmu : V2DI;
         Bsa, Bse, Bsi, Bso, Bsu : V2DI;

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
         Eba := Bba  xor And_Not(Bbe, Bbi);
         Eba := Eba xor Load (RC(RI));
         Ca  := Eba;
         Ebe := Bbe xor And_Not(Bbi, Bbo);
         Ce  := Ebe;
         Ebi := Bbi xor And_Not(Bbo, Bbu);
         Ci  := Ebi;
         Ebo := Bbo xor And_Not(Bbu, Bba);
         Co  := Ebo;
         Ebu := Bbu xor And_Not(Bba, Bbe);
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
         Ega := Bga xor And_Not(Bge, Bgi);
         Ca  := Ca  xor Ega;
         Ege := Bge xor And_Not(Bgi, Bgo);
         Ce  := Ce  xor Ege;
         Egi := Bgi xor And_Not(Bgo, Bgu);
         Ci  := Ci  xor Egi;
         Ego := Bgo xor And_Not(Bgu, Bga);
         Co  := Co  xor Ego;
         Egu := Bgu xor And_Not(Bga, Bge);
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
         Eka := Bka xor And_Not(Bke, Bki);
         Ca  := Ca  xor Eka;
         Eke := Bke xor And_Not(Bki, Bko);
         Ce  := Ce  xor Eke;
         Eki := Bki xor And_Not(Bko, Bku);
         Ci  := Ci  xor Eki;
         Eko := Bko xor And_Not(Bku, Bka);
         Co  := Co  xor Eko;
         Eku := Bku xor And_Not(Bka, Bke);
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
         Ema := Bma xor And_Not(Bme, Bmi);
         Ca  := Ca  xor Ema;
         Eme := Bme xor And_Not(Bmi, Bmo);
         Ce  := Ce  xor Eme;
         Emi := Bmi xor And_Not(Bmo, Bmu);
         Ci  := Ci  xor Emi;
         Emo := Bmo xor And_Not(Bmu, Bma);
         Co  := Co  xor Emo;
         Emu := Bmu xor And_Not(Bma, Bme);
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
         Esa := Bsa xor And_Not(Bse, Bsi);
         Ca  := Ca  xor Esa;
         Ese := Bse xor And_Not(Bsi, Bso);
         Ce  := Ce  xor Ese;
         Esi := Bsi xor And_Not(Bso, Bsu);
         Ci  := Ci  xor Esi;
         Eso := Bso xor And_Not(Bsu, Bsa);
         Co  := Co  xor Eso;
         Esu := Bsu xor And_Not(Bsa, Bse);
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
         Da, De, Di, D0, Du : V2DI;

         Bba, Bbe, Bbi, Bbo, Bbu : V2DI;
         Bga, Bge, Bgi, Bgo, Bgu : V2DI;
         Bka, Bke, Bki, Bko, Bku : V2DI;
         Bma, Bme, Bmi, Bmo, Bmu : V2DI;
         Bsa, Bse, Bsi, Bso, Bsu : V2DI;

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
         Aba := Bba  xor And_Not(Bbe, Bbi);
         Aba := Aba xor Load (RC(RI));
         Ca  := Aba;
         Abe := Bbe xor And_Not(Bbi, Bbo);
         Ce  := Abe;
         Abi := Bbi xor And_Not(Bbo, Bbu);
         Ci  := Abi;
         Abo := Bbo xor And_Not(Bbu, Bba);
         Co  := Abo;
         Abu := Bbu xor And_Not(Bba, Bbe);
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
         Aga := Bga xor And_Not(Bge, Bgi);
         Ca  := Ca  xor Aga;
         Age := Bge xor And_Not(Bgi, Bgo);
         Ce  := Ce  xor Age;
         Agi := Bgi xor And_Not(Bgo, Bgu);
         Ci  := Ci  xor Agi;
         Ago := Bgo xor And_Not(Bgu, Bga);
         Co  := Co  xor Ago;
         Agu := Bgu xor And_Not(Bga, Bge);
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
         Aka := Bka xor And_Not(Bke, Bki);
         Ca  := Ca  xor Aka;
         Ake := Bke xor And_Not(Bki, Bko);
         Ce  := Ce  xor Ake;
         Aki := Bki xor And_Not(Bko, Bku);
         Ci  := Ci  xor Aki;
         Ako := Bko xor And_Not(Bku, Bka);
         Co  := Co  xor Ako;
         Aku := Bku xor And_Not(Bka, Bke);
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
         Ama := Bma xor And_Not(Bme, Bmi);
         Ca  := Ca  xor Ama;
         Ame := Bme xor And_Not(Bmi, Bmo);
         Ce  := Ce  xor Ame;
         Ami := Bmi xor And_Not(Bmo, Bmu);
         Ci  := Ci  xor Ami;
         Amo := Bmo xor And_Not(Bmu, Bma);
         Co  := Co  xor Amo;
         Amu := Bmu xor And_Not(Bma, Bme);
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
         Asa := Bsa xor And_Not(Bse, Bsi);
         Ca  := Ca  xor Asa;
         Ase := Bse xor And_Not(Bsi, Bso);
         Ce  := Ce  xor Ase;
         Asi := Bsi xor And_Not(Bso, Bsu);
         Ci  := Ci  xor Asi;
         Aso := Bso xor And_Not(Bsu, Bsa);
         Co  := Co  xor Aso;
         Asu := Bsu xor And_Not(Bsa, Bse);
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

   end Permute_All;


   procedure XOR_Bits_Into_State(S           : in out Parallel_State;
                                 Data        : in     Keccak.Types.Byte_Array;
                                 Data_Offset : in     Natural;
                                 Bit_Len     : in     Natural)
   is
      use type Keccak.Types.Byte;

      Stride           : constant Natural := Data'Length / 2;

      Remaining_Bits   : Natural := Bit_Len;
      Offset           : Natural := 0;

   begin
      -- Process whole lanes (64 bits).
      Outer_Loop:
      for Y in Y_Coord loop
         pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
         pragma Loop_Invariant (Offset mod (W/8) = 0);
         pragma Loop_Invariant (Offset = Natural (Y) * (W/8) * 5);

         for X in X_Coord loop
            pragma Loop_Invariant ((Offset * 8) + Remaining_Bits = Bit_Len);
            pragma Loop_Invariant (Offset mod (W/8) = 0);
            pragma Loop_Invariant (Offset = (Natural (Y) * (W/8) * 5) + (Natural (X) * (W/8)));

            exit Outer_Loop when Remaining_Bits < W;

            S(X, Y)(0) := S (X,Y)(0) xor Bytes_To_Lane (Data, Data_Offset + Offset);
            S(X, Y)(1) := S (X,Y)(1) xor Bytes_To_Lane (Data, Data_Offset + Offset + Stride);

            Offset         := Offset          + W/8;
            Remaining_Bits := Remaining_Bits  - W;

         end loop;
      end loop Outer_Loop;

      -- Process any remaining data (smaller than 1 lane - 64 bits)
      if Remaining_Bits > 0 then
         declare
            X                : X_Coord   := X_Coord ((Bit_Len / W) mod 5);
            Y                : Y_Coord   := Y_Coord ((Bit_Len / W)  /  5);
            Word0            : Unsigned_64 := 0;
            Word1            : Unsigned_64 := 0;
            Remaining_Bytes  : Natural   := (Remaining_Bits + 7) / 8;

         begin
            for I in Natural range 0 .. Remaining_Bytes - 1 loop
               Word0 := Word0 or Shift_Left(Unsigned_64(Data(Data'First + Data_Offset + Offset + I)), I*8);
               Word1 := Word1 or Shift_Left(Unsigned_64(Data(Data'First + Data_Offset + Offset + I + Stride)), I*8);
            end loop;

            S(X, Y)(0) := S(X, Y)(0) xor (Word0 and (2**Remaining_Bits) - 1);
            S(X, Y)(1) := S(X, Y)(1) xor (Word0 and (2**Remaining_Bits) - 1);
         end;
      end if;
   end XOR_Bits_Into_State;



   procedure Extract_Bytes(S           : in     Parallel_State;
                           Data        :    out Keccak.Types.Byte_Array;
                           Data_Offset : in     Natural;
                           Byte_Len    : in     Natural)
   is
      use type Keccak.Types.Byte;

      Stride          : constant Natural := Data'Length / 2;

      X               : X_Coord := 0;
      Y               : Y_Coord := 0;

      Remaining_Bytes : Natural := Byte_Len;
      Offset          : Natural := 0;

      Lane0           : Unsigned_64;
      Lane1           : Unsigned_64;
   begin
      -- Case when each lane is at least 1 byte (i.e. 8, 16, 32, or 64 bits)

      -- Process whole lanes
      while Remaining_Bytes >= W/8 loop
         pragma Loop_Variant(Increases => Offset,
                             Decreases => Remaining_Bytes);
         pragma Loop_Invariant(Offset mod (W/8) = 0
                               and Offset + Remaining_Bytes = Byte_Len);

         Lane0 := S(X, Y)(0);

         for I in Natural range 0 .. (W/8) - 1 loop
            Data(Data'First + Data_Offset + Offset + I)
              := Keccak.Types.Byte(Shift_Right(Lane0, I*8) and 16#FF#);

            pragma Annotate (GNATprove, False_Positive,
                             """Data"" might not be initialized",
                             "Data is initialized at end of procedure");
         end loop;

         Lane1 := S(X, Y)(1);

         for I in Natural range 0 .. (W/8) - 1 loop
            Data(Data'First + Data_Offset + Offset + I + Stride)
              := Keccak.Types.Byte(Shift_Right(Lane1, I*8) and 16#FF#);
         end loop;

         X := X + 1;
         if X = 0 then
            Y := Y + 1;
         end if;

         Remaining_Bytes := Remaining_Bytes - W/8;
         Offset          := Offset + W/8;
      end loop;

      -- Process any remaining data (smaller than 1 lane)
      if Remaining_Bytes > 0 then
         Lane0 := S(X, Y)(0);
         Lane1 := S(X, Y)(1);

         declare
            Shift          : Natural := 0;
            Initial_Offset : Natural := Offset with Ghost;
         begin
            while Remaining_Bytes > 0 loop
               pragma Loop_Variant(Increases => Offset,
                                   Increases => Shift,
                                   Decreases => Remaining_Bytes);
               pragma Loop_Invariant(Offset + Remaining_Bytes = Data'Length
                                     and Shift mod 8 = 0
                                     and Shift = (Offset - Initial_Offset) * 8);

               Data(Data'First + Data_Offset + Offset)
                 := Keccak.Types.Byte(Shift_Right(Lane0, Shift) and 16#FF#);
               Data(Data'First + Data_Offset + Offset + Stride)
                 := Keccak.Types.Byte(Shift_Right(Lane1, Shift) and 16#FF#);

               pragma Annotate (GNATprove, False_Positive,
                                """Data"" might not be initialized",
                                "Data is initialized at end of procedure");

               Shift           := Shift + 8;
               Offset          := Offset + 1;
               Remaining_Bytes := Remaining_Bytes - 1;
            end loop;
         end;
      end if;

   end Extract_Bytes;


end Keccak.SSE2_KeccakF_1600;
