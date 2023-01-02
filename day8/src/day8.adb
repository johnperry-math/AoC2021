--  Advent of Code 2021
--
--  John Perry
--
--  Day 8: Seven Segment Search
--
--  part 1: identify the lcd outputs that correspond to the easy cases
--     of 1, 4, 7, 8, and report how many there are
--
--  part 2: determine the output values and report their sum
--
--  comments often refer to A, ... G; these come from a diagram of an LCD:
--   AAA
--  B   C
--  B   C
--   DDD
--  E   F
--  E   F
--   GGG

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day8 is

   package Text_IO renames Ada.Text_IO;
   --  package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   --  type Part_Number is (First, Second);

   --  SECTION
   --  global types and variables

   subtype Display_Characters is Character range 'a' .. 'g';

   type Display is array (Display_Characters) of Boolean;

   type Display_Array is array (Natural range <>) of Display;

   type Note_Entry is record
      Signals : Display_Array (1 .. 10) := (others => (others => False));
      Output  : Display_Array (1 .. 4) := (others => (others => False));
   end record;

   package Display_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Note_Entry
      );

   Displays : Display_Vectors.Vector;

   --  SECTION
   --  I/O

   procedure Get_Display (Input : String;
                          Value : out Display;
                          Pos   : out Positive)
   is
   --  reads Value from Input, starting at its first position,
   --  and reports both Value *and* the position immediately *after* it

   begin

      Pos := Input'First;
      Value := (others => False);

      while Pos <= Input'Last and then Input (Pos) in Display_Characters loop
         Value (Input (Pos)) := True;
         Pos := Pos + 1;
      end loop;

   end Get_Display;

   pragma Warnings (Off, "procedure ""Put_Display"" is not referenced");
   procedure Put_Display (D : Display) is
   --  useful for debugging

   begin

      for I in Display_Characters loop
         if D (I) then
            Text_IO.Put (I);
         end if;
      end loop;

   end Put_Display;
   pragma Warnings (On, "procedure ""Put_Display"" is not referenced");

   procedure Read_Input is
   --  read entries from notes, one at a time

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Input_String : constant String := Text_IO.Get_Line (Input_File);
            N            : constant Positive := Input_String'Length;
            Pos          : Positive := 1;
            Val          : Note_Entry;
         begin

            --  signals
            Get_Display (Input_String, Val.Signals (1), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (2), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (3), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (4), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (5), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (6), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (7), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (8), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (9), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Signals (10), Pos);
            Pos := Pos + 3;

            --  outputs
            Get_Display (Input_String (Pos .. N), Val.Output (1), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Output (2), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Output (3), Pos);
            Pos := Pos + 1;
            Get_Display (Input_String (Pos .. N), Val.Output (4), Pos);
            Displays.Append (Val);
         end;

      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Count_Bars (D : Display) return Natural is
   --  return the number of bars active in D
      ((if D ('a') then 1 else 0) +
       (if D ('b') then 1 else 0) +
       (if D ('c') then 1 else 0) +
       (if D ('d') then 1 else 0) +
       (if D ('e') then 1 else 0) +
       (if D ('f') then 1 else 0) +
       (if D ('g') then 1 else 0)
      );

   --  SECTION
   --  part 1

   function Number_Of_Easy_Digits return Natural is
   --  return the number of output displays that are an easy digit (1, 4, 7, 8)

      Result : Natural := 0;

   begin

      for E of Displays loop
         for D of E.Output loop

            declare
               Num_Digits : constant Natural := Count_Bars (D);
            begin

               if Num_Digits = 2         -- 1
                  or else Num_Digits = 3 -- 7
                  or else Num_Digits = 4 -- 4
                  or else Num_Digits = 7 -- 8
               then
                  Result := Result + 1;
               end if;

            end;

         end loop;
      end loop;

      return Result;

   end Number_Of_Easy_Digits;

   --  SECTION
   --  part 2

   function Determine_A (One, Seven : Display) return Display is
   --  determines which bar has A's signal using the rule
   --  A = Seven \ One

      Result : Display;

   begin

      for C in Display_Characters loop
         Result (C) := Seven (C) and then not One (C);
      end loop;

      return Result;

   end Determine_A;

   function Determine_EG (Four, Seven, Eight : Display) return Display is
   --  determines which bars have the E and G signals using the rule
   --  EG = 8 \ (4 u 7)

      Result : Display;

   begin

      for C in Display_Characters loop
         Result (C) := Eight (C) and then not (Four (C) or else Seven (C));
      end loop;

      return Result;

   end Determine_EG;

   function Determine_D (Fivers : Display_Array; EG, A, One : Display)
                         return Display
   is
   --  determines which bar has D's signal using the rule
   --  D = 2 \ (EG u A u 1)
   --    or
   --  D = 3 \ (EG u A u 1)
   --
   --  (5 ALWAYS gives two bars instead of one)

      Result : Display;

   begin

      for Fiver of Fivers loop

         Result := (others => False);

         for C in Display_Characters loop
            Result (C) := Fiver (C)
               and then not (EG (C) or else A (C) or else One (C));
         end loop;

         if Count_Bars (Result) = 1 then
            exit;
         end if;

      end loop;

      return Result;

   end Determine_D;

   function Determine_C (Fivers : Display_Array; EG, A, D : Display)
                           return Display
   is
   --  determines which bar has C's signal using the rule
   --  C = 2 \ (EG u A u D)
   --
   --  trouble is, we don't yet know 2; fortunately, we do know that
   --  of the fivers, only 2 gives one bar from this formula

      Result : Display;

   begin

      for Fiver of Fivers loop

         Result := (others => False);

         for C in Display_Characters loop
            Result (C) := Fiver (C)
               and then not (EG (C) or else A (C) or else D (C));
         end loop;

         if Count_Bars (Result) = 1 then
            exit;
         end if;

      end loop;

      return Result;

   end Determine_C;

   function Determine_F (Two, Three : Display) return Display is
   --  determine which bar has F's signal using the rule
   --  F = 3 \ 2

      Result : Display;

   begin

      for C in Display_Characters loop
         Result (C) := Three (C) and then not Two (C);
      end loop;

      return Result;

   end Determine_F;

   function Determine_B (Two, Five, F : Display) return Display is
   --  determine which bar has B's signal using the rule
   --  B = 5 \ (2 u F)

      Result : Display;

   begin

      for C in Display_Characters loop
         Result (C) := Five (C) and then not (Two (C) or else F (C));
      end loop;

      return Result;

   end Determine_B;

   function Determine_E (Three, Five : Display) return Display is
   --  determines which bar has E's signal using the rule
   --  that E is the only bar that neither 3 nor 5 has

      Result : Display;

   begin

      for C in Display_Characters loop
         Result (C) := not (Three (C) or else Five (C));
      end loop;

      return Result;

   end Determine_E;

   function Determine_G (A, B, C, D, E, F : Display) return Display is
   --  determines which bar has G's signal using the rule
   --  that if we know A through F's bars, then G is the only one remaining

      Result : Display := (others => False);

   begin

      for Char in Display_Characters loop
         Result (Char)
            := not (A (Char) or else B (Char) or else C (Char)
                      or else D (Char) or else E (Char) or else F (Char));
      end loop;

      return Result;

   end Determine_G;

   overriding function "or" (Left, Right : Display) return Display is
   --  returns the display whose bars appear in Left's or Right's or both

      Result : Display;

   begin

      for C in Display_Characters loop
         Result (C) := Left (C) or else Right (C);
      end loop;

      return Result;

   end "or";

   function Union (Displays : Display_Array) return Display is
   --  repeatedly applies "or" to the displays to obtain the bars
   --  that appear in at least one of them

      Result : Display := (others => False);

   begin

      for D of Displays loop
         Result := Result or D;
      end loop;

      return Result;

   end Union;

   function Determine_One (Note : Note_Entry) return Display is
   --  determines which output correspond to 1's signal using the rule
   --  that only 1 has two bars active

      Result : Display;

   begin

      for Signal of Note.Signals loop
         if Count_Bars (Signal) = 2 then
            Result := Signal;
            exit;
         end if;
      end loop;

      return Result;

   end Determine_One;

   function Determine_Four (Note : Note_Entry) return Display is
   --  determines which output correspond to 4's signal using the rule
   --  that only 4 has four bars active

      Result : Display;

   begin

      for Signal of Note.Signals loop
         if Count_Bars (Signal) = 4 then
            Result := Signal;
            exit;
         end if;
      end loop;

      return Result;

   end Determine_Four;

   function Determine_Seven (Note : Note_Entry) return Display is
   --  determines which output correspond to 7's signal using the rule
   --  that only 7 has three bars active

      Result : Display;

   begin

      for Signal of Note.Signals loop
         if Count_Bars (Signal) = 3 then
            Result := Signal;
            exit;
         end if;
      end loop;

      return Result;

   end Determine_Seven;

   function Determine_Eight (Note : Note_Entry) return Display is
   --  determines which output correspond to 8's signal using the rule
   --  that only 8 has all bars active

      Result : Display;

   begin

      for Signal of Note.Signals loop
         if Count_Bars (Signal) = 7 then
            Result := Signal;
         end if;
      end loop;

      return Result;

   end Determine_Eight;

   function Determine_Fivers (Note : Note_Entry) return Display_Array is
   --  determines which outputs can correspond to 2's, 3's, or 5's signal
   --  using the rule that only they has five bars active (hence, "fiver")

      Result : Display_Array (1 .. 3);
      I      : Positive := 1;

   begin

      for D of Note.Signals loop
         if Count_Bars (D) = 5 then
            Result (I) := D;
            I := I + 1;
            if I > 3 then
               exit;
            end if;
         end if;
      end loop;

      return Result;

   end Determine_Fivers;

   function Determine_Two (Fivers : Display_Array; EG, A, D, C : Display)
                           return Display
   is
   --  determines which output correspond to 2's signal using the rule
   --  that 2 is the only fiver satisfying the formula
   --  C = Fiver \ (EG u A u D)

      Result : Display;

   begin

      for Fiver of Fivers loop

         if (for all X in Display_Characters =>
                  C (X) = (Fiver (X)
               and then not (EG (X) or else A (X) or else D (X))))
         then
            Result := Fiver;
            exit;
         end if;

      end loop;

      return Result;

   end Determine_Two;

   function Determine_Three (Fivers : Display_Array; Two : Display)
                             return Display
   is
   --  determines which output correspond to 3's signal using the rule
   --  that 3 is the only fiver such that 3 \ 2 leaves one bar
   --  (5 \ 2 leaves two bars and 2 \ 2 leaves none)

      Result : Display;

   begin

      for Fiver of Fivers loop

         if Fiver /= Two then

            Result := (others => False);

            for C in Display_Characters loop
               Result (C) := Fiver (C) and then not Two (C);
            end loop;

            if Count_Bars (Result) = 1 then
               Result := Fiver;
               exit;
            end if;

         end if;

      end loop;

      return Result;

   end Determine_Three;

   function Determine_Five (Fivers : Display_Array; Two, Three : Display)
                            return Display
   is
   --  determines which output correspond to 5's signal using the rule
   --  that if we know 2 and 3 then we know 5

      Result : Display;

   begin

      for Fiver of Fivers loop

         if Fiver /= Two and then Fiver /= Three then
            Result := Fiver;
            exit;
         end if;

      end loop;

      return Result;

   end Determine_Five;

   Bad_Combination : exception;
   --  shouldn't be raised if things go well --
   --  the fact that my first implementations raised it was helpful

   function Digit (Output, A, B, C, D, E, F, G : Display) return Natural is
   --  returns the digit corresponding to the `Output`,
   --  given the signals for each bar as indicated by the parameters

   begin

      if Output = Union ((A, B, C, E, F, G)) then
         return 0;

      elsif Output = Union ((C, F)) then
         return 1;

      elsif Output = Union ((A, C, D, E, G)) then
         return 2;

      elsif Output = Union ((A, C, D, F, G)) then
         return 3;

      elsif Output = Union ((B, C, D, F)) then
         return 4;

      elsif Output = Union ((A, B, D, F, G)) then
         return 5;

      elsif Output = Union ((A, B, D, E, F, G)) then
         return 6;

      elsif Output = Union ((A, C, F)) then
         return 7;

      elsif Output = Union ((A, B, C, D, E, F, G)) then
         return 8;

      elsif Output = Union ((A, B, C, D, F, G)) then
         return 9;

      end if;

      Text_IO.Put_Line ("bad combination!"
                        & A'Image & B'Image & C'Image & D'Image
                        & E'Image & F'Image & G'Image
                       );
      raise Bad_Combination;

   end Digit;

   function Output_Value (Note : Note_Entry) return Natural is
   --  returns the output value of Note; that is,
   --  the value of the four digits in its signal output
   --
   --  conducts a full analysis of the note to determine their values

      One, Two, Three, Four, Five, Seven, Eight,
         A, B, C, D, E, F, G,
         EG : Display;
      --  outputs corresponding to the given names
      Fivers : Display_Array (1 .. 3);
      --  outputs with five bars active

   begin

      --  determine the easy ones
      One := Determine_One (Note);
      Four := Determine_Four (Note);
      Seven := Determine_Seven (Note);
      Eight := Determine_Eight (Note);

      --  determine A, EG
      A := Determine_A (One, Seven);
      EG := Determine_EG (Four, Seven, Eight);

      --  determine the signals with five bars
      Fivers := Determine_Fivers (Note);

      --  determine C, D
      D := Determine_D (Fivers, EG, A, One);
      C := Determine_C (Fivers, EG, A, D);

      --  isolate 2, 3, 5
      Two := Determine_Two (Fivers, EG, A, D, C);
      Three := Determine_Three (Fivers, Two);
      Five := Determine_Five (Fivers, Two, Three);

      --  determine the remaining bars
      F := Determine_F (Two, Three);
      B := Determine_B (Two, Five, F);
      E := Determine_E (Three, Five);
      G := Determine_G (A, B, C, D, E, F);

      --  Text_IO.Put_Line
      --     ("signal value" & Natural'Image
      --         (Digit (Note.Output (1), A, B, C, D, E, F, G) * 1_000
      --          + Digit (Note.Output (2), A, B, C, D, E, F, G) * 100
      --          + Digit (Note.Output (3), A, B, C, D, E, F, G) * 10
      --          + Digit (Note.Output (4), A, B, C, D, E, F, G)));
      return Digit (Note.Output (1), A, B, C, D, E, F, G) * 1_000
         + Digit (Note.Output (2), A, B, C, D, E, F, G) * 100
         + Digit (Note.Output (3), A, B, C, D, E, F, G) * 10
         + Digit (Note.Output (4), A, B, C, D, E, F, G);

   end Output_Value;

   function Sum_Outputs return Natural is
   --  returns the sum of all the output values

      Result : Natural := 0;

   begin

      for Note of Displays loop
         Result := Result + Output_Value (Note);
      end loop;

      return Result;

   end Sum_Outputs;

begin

   Read_Input;

   Text_IO.Put_Line ("there are" & Number_Of_Easy_Digits'Image
                     & " 1's, 4's, 7's, and 8's");

   Text_IO.Put_Line ("the sum of the outputs is" & Sum_Outputs'Image);

end Day8;
