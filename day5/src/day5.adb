--  Advent of Code 2021
--
--  John Perry
--
--  Day 5: Hydrothermal Venture
--
--  part 1: count the number of hydrothermal vents that appear in more than one
--     vertical or horizontal column of vents
--
--  part 2: repeat, but with diagonal vents

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

procedure Day5 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   type Position is record
      Row, Col : Natural;
   end record;

   function "<" (Left, Right : Position) return Boolean is
      (Left.Row < Right.Row
       or else
          (Left.Row = Right.Row and then Left.Col < Right.Col)
      );

   package Position_Heat_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type     => Position,
       Element_Type => Positive
      );

   Position_Heat_Map : Position_Heat_Maps.Map;

   type Line is record
      First, Last : Position;
   end record;

   package Line_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Line
      );

   Lines : Line_Vectors.Vector;

   --  SECTION
   --  I/O

   procedure Read_Input is
   --  reads the bingo numbers, then the bingo cards

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
            Pos           : Positive := 1;
            First, Second : Position;
            Tmp           : Natural;
         begin
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), First.Row, Pos);
            Pos := Pos + 2;
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), First.Col, Pos);
            Pos := Pos + 5;
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), Second.Row, Pos);
            Pos := Pos + 2;
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), Second.Col, Pos);
            Lines.Append (Line'(First, Second));
            if First.Row = Second.Row then
               if First.Col > Second.Col then
                  Tmp := First.Col;
                  First.Col := Second.Col;
                  Second.Col := Tmp;
               end if;
               for Col in First.Col .. Second.Col loop
                  if Position_Heat_Map.Contains ((First.Row, Col)) then
                     Position_Heat_Map.Include
                        ((First.Row, Col),
                         Position_Heat_Map ((First.Row, Col)) + 1
                        );
                  else
                     Position_Heat_Map.Include ((First.Row, Col), 1);
                  end if;
               end loop;
            elsif First.Col = Second.Col then
               if First.Row > Second.Row then
                  Tmp := First.Row;
                  First.Row := Second.Row;
                  Second.Row := Tmp;
               end if;
               for Row in First.Row .. Second.Row loop
                  if Position_Heat_Map.Contains ((Row, First.Col)) then
                     Position_Heat_Map.Include
                        ((Row, First.Col),
                         Position_Heat_Map ((Row, First.Col)) + 1
                        );
                  else
                     Position_Heat_Map.Include ((Row, First.Col), 1);
                  end if;
               end loop;
            end if;
         end;

      end loop;

      Text_IO.Close (Input_File);

      Text_IO.Put_Line ("there are" & Position_Heat_Map.Length'Image
                        & " positions with vents");

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Number_Of_Overlapping_Points return Natural is
      Result : Natural := 0;
   begin
      for Point in Position_Heat_Map.Iterate loop
         if Position_Heat_Maps.Element (Point) > 1 then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Number_Of_Overlapping_Points;

   --  SECTION
   --  PART 2

   procedure Add_Diagonal_Lines is
   begin
      for Line of Lines loop
         declare
            First renames Line.First;
            Second renames Line.Last;

            Current      : Position := First;
            Row_Delta,
               Col_Delta    : Integer;
         begin
            if First.Row /= Second.Row and then First.Col /= Second.Col then
               Row_Delta := (if First.Row < Second.Row then 1 else -1);
               Col_Delta := (if First.Col < Second.Col then 1 else -1);
               if Position_Heat_Map.Contains (Current) then
                  Position_Heat_Map.Include
                     (Current, Position_Heat_Map (Current) + 1);
               else
                  Position_Heat_Map.Include (Current, 1);
               end if;
               while Current /= Second loop
                  Current.Row := Current.Row + Row_Delta;
                  Current.Col := Current.Col + Col_Delta;
                  if Position_Heat_Map.Contains (Current) then
                     Position_Heat_Map.Include
                        (Current, Position_Heat_Map (Current) + 1);
                  else
                     Position_Heat_Map.Include (Current, 1);
                  end if;
               end loop;
            end if;
         end;
      end loop;
   end Add_Diagonal_Lines;

begin

   Read_Input;

   Text_IO.Put_Line (Number_Of_Overlapping_Points'Image
                     & " overlapping points from horizontal or vertical lines"
                    );

   Add_Diagonal_Lines;

   Text_IO.Put_Line (Number_Of_Overlapping_Points'Image
                     & " overlapping points from all lines"
                     );

end Day5;
