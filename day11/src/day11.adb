--  Advent of Code 2021
--
--  John Perry
--
--  Day 11: Dumbo Octopus
--
--  part 1: how many dumbo octopus flashes occur over 100 iterations?
--
--  part 2: after how many iterations do all the octopus flash?

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Components.Queue;

procedure Day11 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Make_Imagery : constant Boolean := False;

   --  SECTION
   --  global types and variables

   subtype Row_Range is Natural range 1 .. 10;
   subtype Col_Range is Natural range 1 .. 10;

   Energy_Levels : array (Row_Range, Col_Range) of Natural;

   type Position is record
      Row : Row_Range;
      Col : Col_Range;
   end record;

   package Position_Queues is new Components.Queue
      (Element_Type => Position);

   To_Do : Position_Queues.Queue;

   --  SECTION
   --  I/O

   procedure Read_Input is
   --  read octopus' energy levels

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      for Row in 1 .. 10 loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
         begin
            for Col in 1 .. 10 loop
               Energy_Levels (Row, Col)
                  := Character'Pos (Input_String (Col)) - Character'Pos ('0');
            end loop;
         end;

      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   Iteration : Natural := 0;

   procedure Write_Energy_As_Ppm is
      Output_File  : Text_IO.File_Type;
      Suffix       : array (1 .. 4) of Character := (others => '0');
   begin
      declare Tmp_Suffix : constant String := Iteration'Image;
      begin
         if Iteration < 10 then
            Suffix (4) := Tmp_Suffix (2);
         elsif Iteration < 100 then
            Suffix (3) := Tmp_Suffix (2);
            Suffix (4) := Tmp_Suffix (3);
         else
            for I in 2 .. 4 loop
               Suffix (I) := Tmp_Suffix (I);
            end loop;
         end if;
      end;

      Text_IO.Create (Output_File,
                      Name =>
                         "energy_" & String (Suffix) & ".ppm"
                     );
      Text_IO.Put (Output_File, "P3");
      Text_IO.Put (Output_File,
                   Natural'Image (Col_Range'Last - Col_Range'First + 1));
      Text_IO.Put (Output_File,
                   Natural'Image (Row_Range'Last - Row_Range'First + 1));
      Text_IO.Put (Output_File, " 255"); -- max color
      Text_IO.New_Line (Output_File);

      for Row in Row_Range loop
         for Col in Col_Range loop
            declare
               Blue_Value : constant Natural := Energy_Levels (Row, Col) * 25;
               Red_Value  : constant Natural := Energy_Levels (Row, Col) * 12;
            begin
               Text_IO.Put_Line
                  (Output_File, Red_Value'Image & " 0" & Blue_Value'Image);
            end;
         end loop;
         Text_IO.New_Line (Output_File);
      end loop;

      Text_IO.Close (Output_File);

   end Write_Energy_As_Ppm;

   --  SECTION
   --  PARTS 1 AND 2

   procedure Raise_Energy is
   --  raises the energy of each octopus and, if it's higher than 9,
   --  adds it to To_Do for flashing

   begin

      for Row in Row_Range loop
         for Col in Col_Range loop

            Energy_Levels (Row, Col) := @ + 1;
            if Energy_Levels (Row, Col) > 9 then
               To_Do.Push ((Row, Col));
            end if;

         end loop;
      end loop;

   end Raise_Energy;

   procedure Apply_Flash_Spillover is
   --  for each octopus in To_Do, adds 1 to its neighbors' energy and,
   --  if the new energy is more than 9, adds them to to_do
   --  each octopus is considered at most once

      function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
         (Ada.Containers.Hash_Type (P.Row * 11 + P.Col));

      package Position_Sets is new Ada.Containers.Hashed_Sets
         (Element_Type        => Position,
          Hash                => Position_Hash,
          Equivalent_Elements => "="
         );

      Curr : Position;
      Done : Position_Sets.Set;

   begin

      --  add initial flashes to Done
      for P of To_Do loop
         Done.Include (P);
      end loop;

      --  spill over
      Through_Queue : while not To_Do.Is_Empty loop

         To_Do.Pop (Curr);

         Rows : for I in -1 .. 1 loop
            Cols : for J in -1 .. 1 loop

               if (I /= 0 or else J /= 0)
                  and then Curr.Row + I in Row_Range
                  and then Curr.Col + J in Col_Range
               then

                  Energy_Levels (Curr.Row + I, Curr.Col + J) := @ + 1;

                  if Energy_Levels (Curr.Row + I, Curr.Col + J) > 9
                     and then
                        not Done.Contains ((Curr.Row + I, Curr.Col + J))
                  then
                     To_Do.Push ((Curr.Row + I, Curr.Col + J));
                     Done.Include ((Curr.Row + I, Curr.Col + J));
                  end if;

               end if;

            end loop Cols;
         end loop Rows;

      end loop Through_Queue;

   end Apply_Flash_Spillover;

   function Flashes return Natural is
   --  returns the number of octopus that flash

      Result : Natural := 0;

   begin

      for Row in Row_Range loop
         for Col in Col_Range loop

            if Energy_Levels (Row, Col) > 9 then
               Result := Result + 1;
            end if;

         end loop;
      end loop;

      return Result;

   end Flashes;

   procedure Clear_Flashes is
   --  resets all octopus who have flashed to 0

   begin

      for Row in Row_Range loop
         for Col in Col_Range loop

            if Energy_Levels (Row, Col) > 9 then
               Energy_Levels (Row, Col) := 0;
            end if;

         end loop;
      end loop;

   end Clear_Flashes;

   First_Round_Where_All_Flash : Natural := 0;

   function Iterate return Natural is
   --  one iteration of octopus behavior

      Result : Natural;

   begin

      Raise_Energy;
      Apply_Flash_Spillover;
      Result := Flashes;
      Iteration := Iteration + 1;
      if Make_Imagery then
         Write_Energy_As_Ppm;
      end if;
      Clear_Flashes;

      return Result;

   end Iterate;

   function Count_All_Flashes return Natural is
   --  counts flashes over the first 100 steps

      Tmp, Result : Natural := 0;

   begin

      for Step in 1 .. 100 loop

         Tmp := Iterate;
         Result := Result + Tmp;
         if Tmp = 100 and then First_Round_Where_All_Flash = 0 then
            First_Round_Where_All_Flash := Step;
         end if;

      end loop;

      return Result;

   end Count_All_Flashes;

begin

   Read_Input;

   if Make_Imagery then
      Write_Energy_As_Ppm;
   end if;

   --  PART 1
   Text_IO.Put_Line ("there were" & Count_All_Flashes'Image
                     & " flashes after 100 steps");

   --  PART 2
   --  I don't expect them all to have flashed in the first 100 rounds,
   --  but it probably won't hurt to be sure

   if First_Round_Where_All_Flash = 0 then

      declare
         Num_Flashes : Natural;
         Step        : Natural := 101;
      begin

         while First_Round_Where_All_Flash = 0 loop

            Num_Flashes := Iterate;
            if Num_Flashes = 100 then
               First_Round_Where_All_Flash := Step;
            end if;
            Step := Step + 1;

         end loop;

      end;

   end if;

   Text_IO.Put_Line ("all flashed on round"
                     & First_Round_Where_All_Flash'Image);

end Day11;
