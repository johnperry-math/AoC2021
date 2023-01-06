--  Advent of Code 2021
--
--  John Perry
--
--  Day 14: Extended Polymerization
--
--  part 1: report the difference in highest and lowest frequencies of elements
--     after 10 iterations
--
--  part 2: same, but after 40 iterations... wherein any sort of explicit
--     enumeration of the polymer is practically infeasible,
--     and possibly impossible

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

procedure Day14 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   type Part_Number is (First, Second);

   --  SECTION
   --  global types and variables

   package Polymer_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Character
      );

   Polymer : Polymer_Vectors.Vector;
   --  used in part 1 only; this explicitly lists the polymer's elements
   --  in sequence

   type Pair is array (1 .. 2) of Character;
   --  used for both counting and rewriting

   function Pair_Hash (P : Pair) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type
          ((Character'Pos (P (1)) - Character'Pos ('A')) * 26
           + (Character'Pos (P (2)) - Character'Pos ('A')))
      );

   package Rewrite_Rule_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => Pair,
       Element_Type    => Character,
       Hash            => Pair_Hash,
       Equivalent_Keys => "="
      );

   Rewrite_Rules : Rewrite_Rule_Maps.Map;
   --  rules to rewrite a pair of elements

   package Pair_Count_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => Pair,
       Element_Type    => Long_Long_Integer,
       Hash            => Pair_Hash,
       Equivalent_Keys => "="
      );

   Pair_Counts : Pair_Count_Maps.Map;
   --  how many of each pair of elements

   --  SECTION
   --  I/O

   procedure Read_Input is
   --  reads in the dots, then the folds

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      --  first read the starting polymer template
      declare
         Input_String : constant String := Text_IO.Get_Line (Input_File);
      begin
         for C of Input_String loop
            Polymer.Append (C);
         end loop;
      end;

      --  blank line
      Text_IO.Skip_Line (Input_File);

      --  now read the rules
      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
         begin

            Rewrite_Rules.Include ((Input_String (1), Input_String (2)),
                                   Input_String (7)
                                  );

         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   procedure Rewrite (Part : Part_Number := First) is
   --  if Part = First, then this rewrites Polymer according to Rewrite_Rules;
   --  otherwise, it uses Rewrite_Rules to re-count the Pair_Counts

   begin

      if Part = First then

         declare
            New_Polymer : Polymer_Vectors.Vector;
         begin

            for I in Polymer.First_Index .. Polymer.Last_Index - 1 loop
               New_Polymer.Append (Polymer (I));
               --  WOW did I get lucky: what if the rules don't contain them?
               --  my original version didn't consider that...
               if Rewrite_Rules.Contains ((Polymer (I), Polymer (I + 1))) then
                  New_Polymer.Append
                     (Rewrite_Rules ((Polymer (I), Polymer (I + 1))));
               else
                  New_Polymer.Append (Polymer (I));
               end if;
            end loop;

            New_Polymer.Append (Polymer.Last_Element);
            Polymer := Polymer_Vectors.Copy (New_Polymer);

         end;

      else

         declare
            New_Counts           : Pair_Count_Maps.Map;
            P                    : Pair;
            C                    : Character;
            Old_Value, New_Value : Long_Long_Integer;
         begin

            for Cursor in Pair_Counts.Iterate loop

               --  get pair and old value
               P := Pair_Count_Maps.Key (Cursor);
               Old_Value := Pair_Count_Maps.Element (Cursor);

               --  it's possible that the rules don't consider P,
               --  so we have to consider that
               --  (but based on my experience of part 1 that isn't the case;
               --  see above)
               if Rewrite_Rules.Contains (P) then

                  C := Rewrite_Rules (P);

                  --  to avoid triggering Ada's check on tampering with cursors
                  --  we'll have to use New_Value and Old_Value
                  --  I really should look into a better way
                  --  of getting around this

                  New_Value := Old_Value;
                  if New_Counts.Contains ((P (1), C)) then
                     New_Value := New_Value + New_Counts ((P (1), C));
                  end if;
                  New_Counts.Include ((P (1), C), New_Value);

                  New_Value := Old_Value;
                  if New_Counts.Contains ((C, P (2))) then
                     New_Value := New_Value + New_Counts ((C, P (2)));
                  end if;
                  New_Counts.Include ((C, P (2)), New_Value);

               else

                  New_Value := Old_Value;
                  if New_Counts.Contains (P) then
                     New_Value := New_Value + New_Counts (P);
                  end if;
                  New_Counts.Include (P, New_Value);

               end if;

            end loop;

            Pair_Counts := New_Counts;

         end;

      end if;

   end Rewrite;

   function Score_1 return Natural is
   --  return the difference between the highest and lowest frequencies
   --  of elements as recorded in Polymer (which we use for Part 1)

      function Character_Hash (C : Character) return Ada.Containers.Hash_Type
      is (Character'Pos (C));

      package Character_Count_Maps is new Ada.Containers.Hashed_Maps
         (Key_Type => Character,
          Element_Type    => Positive,
          Hash            => Character_Hash,
          Equivalent_Keys => "="
         );

      Character_Counts : Character_Count_Maps.Map;
      --  the number of times each character appears

      Value            : Positive;
      --  the number of times a particular character appears

      Highest_Freq              : Positive := 1;
      Lowest_Freq               : Positive := Positive'Last;
      --  Most_Common, Least_Common : Character;
      --  Highest_Freq and Lowest_Freq are what they say
      --  Most_Common and Least_Common are the characters that correspond
      --  to those counts

   begin

      for C of Polymer loop
         if not Character_Counts.Contains (C) then
            Character_Counts.Include (C, 1);
         else
            Value := Character_Counts (C) + 1;
            Character_Counts.Include (C, Value);
         end if;
      end loop;

      for Cursor in Character_Counts.Iterate loop

         if Character_Count_Maps.Element (Cursor) > Highest_Freq then
            Highest_Freq := Character_Count_Maps.Element (Cursor);
            --  Most_Common := Character_Count_Maps.Key (Cursor);
         end if;

         if Character_Count_Maps.Element (Cursor) < Lowest_Freq then
            Lowest_Freq := Character_Count_Maps.Element (Cursor);
            --  Least_Common := Character_Count_Maps.Key (Cursor);
         end if;

      end loop;

      --  useful for debugging
      --  Text_IO.Put (Most_Common); Text_IO.Put_Line (Highest_Freq'Image);
      --  Text_IO.Put (Least_Common); Text_IO.Put_Line (Lowest_Freq'Image);
      --  Text_IO.New_Line;

      return Highest_Freq - Lowest_Freq;

   end Score_1;

   function Score_2 return Long_Long_Integer is
   --  return the differences between the highest and lowest frequencies
   --  of elements as recorded in Pair_Counts (which we use for Part 2)

      function Character_Hash (C : Character) return Ada.Containers.Hash_Type
      is (Character'Pos (C));

      package Character_Count_Maps is new Ada.Containers.Hashed_Maps
         (Key_Type => Character,
          Element_Type    => Long_Long_Integer,
          Hash            => Character_Hash,
          Equivalent_Keys => "="
         );

      Character_Counts : Character_Count_Maps.Map;
      --  the number of times each character appears

      Value            : Long_Long_Integer;
      --  the number of times a particular character appears

      Highest_Freq              : Long_Long_Integer := 1;
      Lowest_Freq               : Long_Long_Integer := Long_Long_Integer'Last;
      --  Most_Common, Least_Common : Character;
      --  Highest_Freq and Lowest_Freq are what they say
      --  Most_Common and Least_Common are the characters that correspond
      --  to those counts

   begin

      for Cursor in Pair_Counts.Iterate loop

         declare
            P : constant Character := Pair_Count_Maps.Key (Cursor) (1);
            E : constant Long_Long_Integer := Pair_Count_Maps.Element (Cursor);
         begin

            if not Character_Counts.Contains (P) then
               Character_Counts.Include (P, E);
            else
               Value := Character_Counts (P) + E;
               Character_Counts.Include (P, Value);
            end if;

         end;

      end loop;

      --  the last character isn't counted in the loop above,
      --  so we handle it here
      Value := Character_Counts (Polymer.Last_Element);
      Character_Counts.Include (Polymer.Last_Element, Value + 1);

      for Cursor in Character_Counts.Iterate loop

         if Character_Count_Maps.Element (Cursor) > Highest_Freq then
            Highest_Freq := Character_Count_Maps.Element (Cursor);
            --  Most_Common := Character_Count_Maps.Key (Cursor);
         end if;

         if Character_Count_Maps.Element (Cursor) < Lowest_Freq then
            Lowest_Freq := Character_Count_Maps.Element (Cursor);
            --  Least_Common := Character_Count_Maps.Key (Cursor);
         end if;

      end loop;

      --  Text_IO.Put (Most_Common); Text_IO.Put_Line (Highest_Freq'Image);
      --  Text_IO.Put (Least_Common); Text_IO.Put_Line (Lowest_Freq'Image);
      --  Text_IO.New_Line;

      return Highest_Freq - Lowest_Freq;

   end Score_2;

   procedure Initialize_Counts is
   --  initializes Pair_Counts using Polymer, so do this before Part 1!

      Value : Long_Long_Integer;

   begin

      for I in Polymer.First_Index .. Polymer.Last_Index - 1 loop

         if Pair_Counts.Contains ((Polymer (I), Polymer (I + 1))) then
            Value := Pair_Counts ((Polymer (I), Polymer (I + 1)));
            Pair_Counts.Include ((Polymer (I), Polymer (I + 1)),
                                 Value + 1);

         else
            Pair_Counts.Include ((Polymer (I), Polymer (I + 1)), 1);

         end if;

      end loop;

   end Initialize_Counts;

begin

   Read_Input;
   Text_IO.Put_Line ("template length:" & Polymer.Length'Image);
   Text_IO.Put_Line ("# of rewrite rules:" & Rewrite_Rules.Length'Image);

   Initialize_Counts;

   --  part 1
   for I in 1 .. 10 loop
      Rewrite;
   end loop;
   Text_IO.Put_Line ("after 10 iterations the score is" & Score_1'Image);

   --  part 2
   for I in 1 .. 40 loop
      Rewrite (Second);
   end loop;
   Text_IO.Put_Line ("after 40 iterations the score is" & Score_2'Image);

end Day14;
