--  Advent of Code 2021
--
--  John Perry
--
--  Day 1: Sonar Sweep
--
--  part 1: find how quickly the sea floor is descending
--
--  part 2:

with Ada.Text_IO;

procedure Day1 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   procedure Count_Increases is
      Filename : constant String := (if Doing_Example then "example.txt"
                                     else "input.txt"
                                    );
      Input_File : Text_IO.File_Type;
      Number_Of_Increases : Natural := 0;
      Depth, Prev_Depth   : Natural;

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);
      Natural_IO.Get (Input_File, Prev_Depth);
      while not Text_IO.End_Of_File (Input_File) loop
         Natural_IO.Get (Input_File, Depth);
         if Depth > Prev_Depth then
            Number_Of_Increases := Number_Of_Increases + 1;
         end if;
         Prev_Depth := Depth;
      end loop;
      Text_IO.Close (Input_File);

      Text_IO.Put_Line ("depth increased" & Number_Of_Increases'Image
                        & " times");

   end Count_Increases;

   procedure Count_Sum_Increases is
      Filename : constant String := (if Doing_Example then "example.txt"
                                     else "input.txt"
                                    );
      Input_File : Text_IO.File_Type;
      Number_Of_Increases : Natural := 0;
      D1, D2, D3          : Natural;
      Sum, Prev_Sum       : Natural;

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);
      Natural_IO.Get (Input_File, D1);
      Natural_IO.Get (Input_File, D2);
      Natural_IO.Get (Input_File, D3);
      Sum := D1 + D2 + D3;
      while not Text_IO.End_Of_File (Input_File) loop
         D1 := D2;
         D2 := D3;
         Natural_IO.Get (Input_File, D3);
         Prev_Sum := Sum;
         Sum := D1 + D2 + D3;
         if Sum > Prev_Sum then
            Number_Of_Increases := Number_Of_Increases + 1;
         end if;
      end loop;
      Text_IO.Close (Input_File);

      Text_IO.Put_Line ("depth sums increased" & Number_Of_Increases'Image
                        & " times");

   end Count_Sum_Increases;

begin
   Count_Increases;
   Count_Sum_Increases;
end Day1;
