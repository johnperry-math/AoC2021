--  Advent of Code 2021
--
--  John Perry
--
--  Day 10: Syntax Scoring
--
--  part 1: determine which lines of chunks are corrupt,
--     add up the syntax error scores
--
--  part 2: complete the remaining lines of chunks,
--     report middle completion score

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day10 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   type Delimiter is (Parens_Open, Parens_Close,
                      Bracket_Open, Bracket_Close,
                      Brace_Open, Brace_Close,
                      Angle_Open, Angle_Close
                     );
   --  this turned out to be overkill; could just as well have used strings

   package Chunk_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Delimiter
      );

   use type Ada.Containers.Count_Type;

   function "=" (Left, Right : Chunk_Vectors.Vector) return Boolean is
      (Left.Length = Right.Length and then
          (for all I in Left.First_Index .. Left.Last_Index =>
                 Left (I) = Right (I)
      ));

   package Syntax_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Chunk_Vectors.Vector
      );

   Syntax : Syntax_Vectors.Vector;
   --  stores each line of chunks

   package Positive_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Positive
      );

   package Natural_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Long_Long_Integer
      );

   --  SECTION
   --  I/O

   Invalid_Character : exception;

   procedure Read_Input is
   --  read syntax chunks

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
            New_Chunk_Vec : Chunk_Vectors.Vector;
         begin
            for C of Input_String loop
               New_Chunk_Vec.Append
                  ((case C is
                      when '(' => Parens_Open,
                      when ')' => Parens_Close,
                      when '[' => Bracket_Open,
                      when ']' => Bracket_Close,
                      when '{' => Brace_Open,
                      when '}' => Brace_Close,
                      when '<' => Angle_Open,
                      when '>' => Angle_Close,
                      when others => raise Invalid_Character
                  ));
            end loop;
            Syntax.Append (New_Chunk_Vec);
         end;

      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PART 1

   procedure Parse_Inner (Line          : Chunk_Vectors.Vector;
                          Pos           : in out Positive;
                          Match         : Delimiter;
                          Valid_Through : in out Natural)
      with Pre => Pos = Valid_Through + 1
   is
   --  parses Line, starting at Pos,
   --  stopping when it finds the requested Match,
   --  recording the last valid character at Valid_Through
   --
   --  upon entry, expects Pos = Valid_Through + 1

   begin

      while Valid_Through = Pos - 1 and then Pos <= Line.Last_Index loop

         case Line (Pos) is

         when Parens_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Parens_Close, Valid_Through);

         when Parens_Close =>

            if Match = Parens_Close then
               Valid_Through := Pos;
               Pos := Pos + 1;
               return;

            else
               Pos := Line.Last_Index + 1;
            end if;

         when Brace_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Brace_Close, Valid_Through);

         when Brace_Close =>

            if Match = Brace_Close then
               Valid_Through := Pos;
               Pos := Pos + 1;
               return;

            else
               Pos := Line.Last_Index + 1;

            end if;

         when Bracket_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Bracket_Close, Valid_Through);

         when Bracket_Close =>

            if Match = Bracket_Close then
               Valid_Through := Pos;
               Pos := Pos + 1;
               return;

            else
               Pos := Line.Last_Index + 1;

            end if;

         when Angle_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Angle_Close, Valid_Through);

         when Angle_Close =>

            if Match = Angle_Close then
               Valid_Through := Pos;
               Pos := Pos + 1;
               return;

            else
               Pos := Line.Last_Index + 1;

            end if;

         end case;
      end loop;

   end Parse_Inner;

   function Parse_Syntax (Line : Chunk_Vectors.Vector) return Natural is
   --  parses a line of chunks; depends on Parse_Inner

      Pos           : Positive := Line.First_Index;
      Valid_Through : Natural  := 0;

   begin

      while Pos <= Line.Last_Index loop

         case Line (Pos) is

         when Parens_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Parens_Close, Valid_Through);

         when Bracket_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Bracket_Close, Valid_Through);

         when Brace_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Brace_Close, Valid_Through);

         when Angle_Open =>
            Valid_Through := Pos;
            Pos := Pos + 1;
            Parse_Inner (Line, Pos, Angle_Close, Valid_Through);

         when others =>
            Pos := Line.Last_Index;

         end case;

      end loop;

      return (if Valid_Through = Line.Last_Index then 0
              elsif Line (Valid_Through + 1) = Parens_Close then 3
              elsif Line (Valid_Through + 1) = Bracket_Close then 57
              elsif Line (Valid_Through + 1) = Brace_Close then 1197
              elsif Line (Valid_Through + 1) = Angle_Close then 25137
              else raise Invalid_Character with "189");

   end Parse_Syntax;

   --  SECTION
   --  PART 2

   Bad_Pairing : exception;
   --  should never be called...

   function Completion_Score (Line : Chunk_Vectors.Vector)
                              return Long_Long_Integer
   is
   --  returns the completion score of the given line

      Pos      : Natural              := 1;
      Result   : Long_Long_Integer    := 0;
      New_Line : Chunk_Vectors.Vector := Chunk_Vectors.Copy (Line);

      Score : constant array (Delimiter) of Long_Long_Integer
         := (Parens_Open  => 1,
             Bracket_Open => 2,
             Brace_Open   => 3,
             Angle_Open   => 4,
             others       => 0
            );

      Paired : constant array (Delimiter) of Delimiter
         := (Parens_Open => Parens_Close,
             Parens_Close => Parens_Open,
             Bracket_Open => Bracket_Close,
             Bracket_Close => Bracket_Open,
             Brace_Open    => Brace_Close,
             Brace_Close   => Brace_Open,
             Angle_Open    => Angle_Close,
             Angle_Close   => Angle_Open
            );

   begin

      while Pos <= New_Line.Last_Index loop

         case New_Line (Pos) is

            when Parens_Open | Brace_Open | Bracket_Open | Angle_Open =>
               Pos := Pos + 1;

            when others =>
               if New_Line (Pos - 1) /= Paired (New_Line (Pos)) then
                  raise Bad_Pairing with New_Line'Image;
               end if;
               Pos := Pos - 2;
               New_Line.Delete (Pos + 1, 2);

         end case;

         Pos := Natural'Max (Pos, 1);

      end loop;

      if Pos > 0 then
         Pos := Pos - 1;
      end if;

      while Pos > 0 loop

         case New_Line (Pos) is

            when Parens_Open | Bracket_Open | Brace_Open | Angle_Open =>
               Result := Result * 5 + Score (New_Line (Pos));

            when others =>
               raise Bad_Pairing with "when completing" & New_Line'Image;

         end case;

         Pos := Pos - 1;

      end loop;

      return Result;

   end Completion_Score;

begin

   Read_Input;

   --  PART 1

   declare
      Total_Syntax_Error_Score : Natural := 0;
      Lines_To_Delete          : Positive_Vectors.Vector;
      Syntax_Error_Score       : Natural;
   begin
      for I in Syntax.First_Index .. Syntax.Last_Index loop
         Syntax_Error_Score := Parse_Syntax (Syntax (I));
         if Syntax_Error_Score /= 0 then
            Lines_To_Delete.Append (I);
            Total_Syntax_Error_Score := @ + Syntax_Error_Score;
         end if;
      end loop;
      Text_IO.Put_Line ("total syntax error score is"
                        & Total_Syntax_Error_Score'Image);
      Lines_To_Delete.Reverse_Elements;
      for I of Lines_To_Delete loop
         Syntax.Delete (I);
      end loop;
   end;

   --  PART 2

   declare
      Scores : Natural_Vectors.Vector;
      package Score_Sorter is new Natural_Vectors.Generic_Sorting;
   begin
      for Line of Syntax loop
         --  Text_IO.Put_Line ("completing line" & Line'Image);
         Scores.Append (Completion_Score (Line));
      end loop;
      Score_Sorter.Sort (Scores);
      Text_IO.Put_Line ("the middle autocomplete score is"
                        & Long_Long_Integer'Image
                           (Scores ((Positive (Scores.Length) + 1) / 2))
                       );
   end;

end Day10;
