--  Advent of Code 2021
--
--  John Perry
--
--  Day 16: Packet Decoder
--
--  part 1: compute the sum of the packet versions in a BITS message
--
--  part 2: compute the result of the performing in the BITS message operations

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day16 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   --  we convert the input to a binary vector

   subtype Binary is Natural range 0 .. 1;

   package Binary_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Binary
      );

   Binary_Input : Binary_Vectors.Vector;

   --  Part 1 wants versions

   package Version_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Natural
      );

   Version_Numbers : Version_Vectors.Vector;

   --  part 2 wants operations

   type Operator is (Sum, Product, Minimum, Maximum, Literal,
                     Greater_Than, Less_Than, Equal_To
                    );

   --  SECTION
   --  I/O

   procedure De_Hex (S : String) is
   --  converts the hex string to a vector of binary numbers,
   --  stored in Binary_Inputs

      Hex_Value : Natural;

   begin

      Binary_Input.Clear;

      for C of S loop

         Hex_Value := (if C in '0' .. '9' then
                          Character'Pos (C) - Character'Pos ('0')
                       else
                          Character'Pos (C) - Character'Pos ('A') + 10
                      );

         if Hex_Value >= 8 then
            Binary_Input.Append (1);
            Hex_Value := Hex_Value - 8;
         else
            Binary_Input.Append (0);
         end if;

         if Hex_Value >= 4 then
            Binary_Input.Append (1);
            Hex_Value := Hex_Value - 4;
         else
            Binary_Input.Append (0);
         end if;

         if Hex_Value >= 2 then
            Binary_Input.Append (1);
            Hex_Value := Hex_Value - 2;
         else
            Binary_Input.Append (0);
         end if;

         if Hex_Value = 1 then
            Binary_Input.Append (1);
         else
            Binary_Input.Append (0);
         end if;

      end loop;

   end De_Hex;

   procedure Read_Input is
   --  reads the input string and converts it to binary by calling De_Hex

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      declare
         Input_String : constant String := Text_IO.Get_Line (Input_File);
      begin
         De_Hex (Input_String);
      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   procedure Read_Bits (Pos : in out Positive;
                        Num   : Positive;
                        Value : out Natural)
   is
   --  reads Num bits from Binary_Input starting at Pos, storing in Value
   begin

      Value := 0;

      for I in 0 .. Num - 1 loop
         Value := 2 * Value + Binary_Input (Pos + I);
      end loop;

      Pos := Pos + Num;

   end Read_Bits;

   procedure Read_Literal (Pos   : in out Positive;
                           Value : out Long_Long_Integer)
   is
   --  reads a literal from Binary_Input starting at Pos, storing in Value

      Tmp : Natural;

   begin

      Value := 0;

      --  non-final groups start with 1
      while Binary_Input (Pos) = 1 loop
         Pos := Pos + 1;
         Read_Bits (Pos, 4, Tmp);
         Value := Value * 16 + Long_Long_Integer (Tmp);
      end loop;

      --  final group
      Pos := Pos + 1;
      Read_Bits (Pos, 4, Tmp);
      Value := Value * 16 + Long_Long_Integer (Tmp);

   end Read_Literal;

   Bad_Operator : exception;
   --  shouldn't ever be called

   procedure Perform_Op (Op    : Operator;
                         Value : in out Long_Long_Integer;
                         Tmp   : Long_Long_Integer)
   is
   --  performs Op on Value and Tmp
   --
   --  The first four behave as you'd expect, but
   --  the behavior of the last three depends on whether Value is -1;
   --  if so, then we assume it is uninitialized and simply assign Tmp to it;
   --  otherwise, we perform the desired comparison
   --  and assign 0/1 if False/True
   begin

      Value := (case Op is
                --  behave as you'd expect
                   when Sum          => Value + Tmp,
                   when Product      => Value * Tmp,
                   when Minimum      => Long_Long_Integer'Min (Value, Tmp),
                   when Maximum      => Long_Long_Integer'Max (Value, Tmp),

                --  should never occur
                   when Literal      => raise Bad_Operator,

                --  behave as described above
                   when Greater_Than =>
                      (if Value = -1 then Tmp elsif Value > Tmp then 1 else 0),
                   when Less_Than    =>
                      (if Value = -1 then Tmp elsif Value < Tmp then 1 else 0),
                   when Equal_To     =>
                      (if Value = -1 then Tmp elsif Value = Tmp then 1 else 0)
               );

   end Perform_Op;

   procedure Parse (Pos : in out Positive; Value : out Long_Long_Integer);
   --  parses Binary_Input, starting at position Pos,
   --  returning the result in Value

   procedure Read_Operator (Pos   : in out Positive;
                            Op    : Operator;
                            Value : out Long_Long_Integer)
   is
   --  parses an operator from Binary_Input, starting at Pos,
   --  and performs Op, storing the result in Value

      Length, Num_Sub_Packets : Natural;
      Tmp                     : Long_Long_Integer;

   begin

      --  initialize Value appropriately
      --  see Perform_Op above for details on these values
      Value := (case Op is
                   when Sum          => 0,
                   when Product      => 1,
                   when Minimum      => Long_Long_Integer'Last,
                   when Maximum      => Long_Long_Integer'First,
                   when Greater_Than => -1,
                   when Less_Than    => -1,
                   when Equal_To     => -1,
                   when others       => raise Bad_Operator
               );

      --  type ID is "length"
      if Binary_Input (Pos) = 0 then

         --  total length in bits
         Pos := Pos + 1;
         Read_Bits (Pos, 15, Length);

         declare
            Old_Pos : constant Natural := Pos;
         begin

            while Pos < Old_Pos + Length loop
               Parse (Pos, Tmp);
               Perform_Op (Op, Value, Tmp);
            end loop;

         end;

      else

         Pos := Pos + 1;
         Read_Bits (Pos, 11, Num_Sub_Packets);

         for I in 1 .. Num_Sub_Packets loop
            Parse (Pos, Tmp);
            Perform_Op (Op, Value, Tmp);
         end loop;

      end if;

   end Read_Operator;

   procedure Parse (Pos : in out Positive; Value : out Long_Long_Integer) is
   --  parses Binary_Input starting at Pos, storing the result in Value

      Packet_Ver, Packet_Type : Natural;

   begin

      --  packet version
      Read_Bits (Pos, 3, Packet_Ver);
      Version_Numbers.Append (Packet_Ver);

      --  packet type
      Read_Bits (Pos, 3, Packet_Type);

      --  what we do next depends on header's type ID
      case Packet_Type is
         when 4 =>
            Read_Literal (Pos, Value);
         when others =>
            Read_Operator (Pos, Operator'Val (Packet_Type), Value);
      end case;

   end Parse;

   function Version_Sum return Natural is
   --  part 1 wants the sum of the version numbers

      Result : Natural := 0;

   begin

      for Version of Version_Numbers loop
         Result := Result + Version;
      end loop;

      return Result;

   end Version_Sum;

begin

   if Doing_Example then

      --  tests from examples
      declare
         Pos : Positive;
         Value : Long_Long_Integer;
      begin

         De_Hex ("38006F45291200");
         Pos := 1;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum of 38006F45291200 is"
                           & Version_Sum'Image);
         Text_IO.New_Line;

         De_Hex ("EE00D40C823060");
         Pos := 1;
         Version_Numbers.Clear;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum of EE00D40C823060 is"
                           & Version_Sum'Image);
         Text_IO.New_Line;

         De_Hex ("8A004A801A8002F478");
         Pos := 1;
         Version_Numbers.Clear;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum of 8A004A801A8002F478 is"
                           & Version_Sum'Image);
         Text_IO.New_Line;

         De_Hex ("620080001611562C8802118E34");
         Pos := 1;
         Version_Numbers.Clear;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum of 620080001611562C8802118E34 is"
                           & Version_Sum'Image);
         Text_IO.New_Line;

         De_Hex ("C0015000016115A2E0802F182340");
         Pos := 1;
         Version_Numbers.Clear;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum of C0015000016115A2E0802F182340 is"
                           & Version_Sum'Image);
         Text_IO.New_Line;

         De_Hex ("A0016C880162017C3686B18A3D4780");
         Pos := 1;
         Version_Numbers.Clear;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum of A0016C880162017C3686B18A3D4780 is"
                           & Version_Sum'Image);
         Text_IO.New_Line;

      end;

   else

      declare
         Value : Long_Long_Integer;
         Pos   : Positive := 1;
      begin

         Read_Input;
         Parse (Pos, Value);
         Text_IO.Put_Line ("version sum is" & Version_Sum'Image);
         Text_IO.Put_Line ("value is" & Value'Image);

      end;

   end if;

end Day16;
