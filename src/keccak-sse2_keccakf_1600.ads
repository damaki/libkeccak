with Keccak.Arch.SSE2; use Keccak.Arch.SSE2;
with Keccak.Types;

package Keccak.SSE2_KeccakF_1600
is

   type Parallel_State is private;


   subtype State_Index is Natural range 0 .. 1;


   type Round_Index is range 0 .. 23;


   subtype Round_Count is Positive range 1 .. 24;


   procedure Init (S : out Parallel_State);


   generic
      First_Round : Round_Index := 0;
      Num_Rounds  : Round_Count := 24;
   procedure Permute_All (S : in out Parallel_State);


   procedure XOR_Bits_Into_State (S           : in out Parallel_State;
                                  Data        : in     Types.Byte_Array;
                                  Data_Offset : in     Natural;
                                  Bit_Len     : in     Natural)
     with Pre => (Data'Length / 2 <= Natural'Last / 8
                  and then Data'Length mod 2 = 0
                  and then Bit_Len <= (Data'Length / 2) * 8
                  and then Bit_Len <= 1600);
   --    DO    BL             DO    BL
   --  |--->|<---->|        |--->|<---->|
   --  +-----------------------------------------+
   --  |                    |                    | Data
   --  +-----------------------------------------+
   --       |      |             |      |
   --       | XOR  |             | XOR  |
   --       |  v   |             |  v   |
   --       +-----------+        +-----------+
   --       |  state 0  |        |  state 1  |
   --       +-----------+        +-----------+
   --
   --  Where DO = Data_Offset and BL = Bit_Len


   procedure Extract_Bytes (S           : in     Parallel_State;
                            Data        :    out Types.Byte_Array;
                            Data_Offset : in     Natural;
                            Byte_Len    : in     Natural)
     with Pre => (Byte_Len <= 1600 / 8
                  and Byte_Len <= Data'Length / 2
                  and Data'Length mod 2 = 0);
   --
   --  |<--byte-len-->|     |<--byte-len-->|
   --  +-----------------------------------------+
   --  |    instance 0      |    instance 1      |
   --  +-----------------------------------------+

private

   type X_Coord is mod 5;
   type Y_Coord is mod 5;

   type Parallel_State is array (X_Coord, Y_Coord) of Keccak.Arch.SSE2.V2DI_View;

end Keccak.SSE2_KeccakF_1600;
