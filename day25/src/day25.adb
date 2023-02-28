-- Advent of Code 2021
--
-- John Perry
--
-- Day 24: Sea Cucumbers
--
-- part 1: determine how many turns it takes the sea cucumbers to stop moving
--
-- part 2: freebie, as usual

with Ada.Text_IO;

procedure Day25 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Visualizing : constant Boolean := True;

   -- SECTION
   -- global types and variables

   type Status is ( Empty, Down, Right );
   -- sea cucumbers move either down or right

   Num_Rows : constant Positive := ( if Doing_Example then 9 else 137 );
   Num_Cols : constant Positive := ( if Doing_Example then 10 else 139 );

   type Map_Array is array ( 1 .. Num_Rows, 1 .. Num_Cols ) of Status;
   Map : Map_Array;

   -- SECTION
   -- I/O

   Invalid_Input : exception;

   procedure Read_Input is
   -- reads the input and fills the initial map

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := ( if Doing_Example then "example.txt"
                                         else "input.txt" );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      for Row in Map'Range ( 1 ) loop

         declare
            S : String := Text_IO.Get_Line ( Input_File );
         begin

            for Col in Map'Range ( 2 ) loop

               Map ( Row, Col ) := ( if S ( Col ) = '.' then Empty
                                     elsif S ( Col ) = '>' then Right
                                     elsif S ( Col ) = 'v' then Down
                                     else raise Invalid_Input
                                        with Row'Image & Col'Image );

            end loop;
         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   procedure Put_Map ( Map : Map_Array ) is
   -- useful for debugging
   begin

      for Row in Map'Range ( 1 ) loop

         for Col in Map'Range ( 2 ) loop
            Text_IO.Put ( ( case Map ( Row, Col ) is
                             when Empty => '.',
                             when Right => '>',
                             when Down => 'v'
                         ) );
         end loop;

         Text_IO.New_Line;

      end loop;

   end Put_Map;

   procedure Write_To_Ppm ( Map : Map_Array; Step : Natural; Substep : Natural )
   is

      Output_File : Text_IO.File_Type;

      Scale       : constant Positive := 1;

      subtype Rows is Integer range 1 .. Scale * Num_Rows;
      subtype Cols is Integer range 1 .. Scale * Num_Cols;

      Width       : constant Positive := Cols'Last - Cols'First + 1;
      Height      : constant Positive := Rows'Last - Rows'First + 1;

      Step_String : String := Step'Image;
      Substep_String : String := Substep'Image;
      Suffix_String : String := ( if Step < 1 then "000"
                         elsif Step < 10 then "00"
                         elsif Step < 100 then "0"
                         else ""
                       ) & Step_String ( 2 .. Step_String'Last );

   begin

      Text_IO.Create (Output_File,
                      Name => "iteration_"
                      & Suffix_String
                      & "_" & Substep_String ( 2 .. Substep_String'Last )
                      & ".ppm" );
      Text_IO.Put (Output_File, "P3");
      Text_IO.Put (Output_File, Width'Image);
      Text_IO.Put (Output_File, Height'Image);
      Text_IO.Put (Output_File, " 255"); -- max color
      Text_IO.New_Line (Output_File);

      for Row in 1 .. Num_Rows loop
         for Row_Repeat in 1 .. Scale loop
            for Col in 1 .. Num_Cols loop

               for Col_Repeat in 1 .. Scale loop
                  case Map ( Row, Col ) is
                  when Empty => Text_IO.Put_Line ( Output_File, "194 178 128" );
                  when Right => Text_IO.Put_Line ( Output_File, "224 64 64" );
                  when Down => Text_IO.Put_Line ( Output_File, "64 64 224" );
                  end case;
               end loop;

            end loop;
            Text_IO.New_Line ( Output_File );
         end loop;
         Text_IO.New_Line ( Output_File );
      end loop;

      Text_IO.Close (Output_File);

   end Write_To_Ppm;



   -- SECTION
   -- Part 1

   procedure Iterate ( Changed : out Boolean; Step : Positive ) is
   -- move the cucumbers per puzzle rules
      Tmp : Map_Array := ( others => ( others => Empty ) );
      Next_Row, Next_Col : Positive;
   begin

      Changed := False; -- innocent until proven guilty

      -- first the east-facing sea cucumbers move
      for Row in Map'Range ( 1 ) loop
         for Col in Map'Range ( 2 ) loop

            case Map ( Row, Col ) is

               when Empty => null;

               when Right =>
                  Next_Col := ( if Col = Map'Last ( 2 ) then 1 else Col + 1 );
                  if Map ( Row, Next_Col ) = Empty then
                     Tmp ( Row, Next_Col ) := Right;
                     Changed := True;
                  else
                     Tmp ( Row, Col ) := Right;
                  end if;

               when Down =>
                  Tmp ( Row, Col ) := Down;

            end case;

         end loop;
      end loop;

      -- update map and reset tmp
      if Visualizing then
         Write_To_Ppm ( Tmp, Step, 1 );
      end if;
      Map := Tmp;
      Tmp := ( others => ( others => Empty ) );

       -- next the right-facing sea cucumbers move
      for Row in Map'Range ( 1 ) loop
         for Col in Map'Range ( 2 ) loop

            case Map ( Row, Col ) is

               when Empty => null;

               when Right =>
                  Tmp ( Row, Col ) := Right;

               when Down =>
                  Next_Row := ( if Row = Map'Last ( 1 ) then 1 else Row + 1 );
                  if Map ( Next_Row, Col ) = Empty then
                     Tmp ( Next_Row, Col ) := Down;
                     Changed := True;
                  else
                     Tmp ( Row, Col ) := Down;
                  end if;

            end case;

         end loop;
      end loop;

      if Visualizing then
         Write_To_Ppm ( Tmp, Step, 2 );
      end if;
      Map := Tmp;

   end Iterate;

   Iteration : Natural := 0;
   Map_Changed : Boolean := True;

begin

   Read_Input;

   if Visualizing then
      Write_To_Ppm ( Map, 0, 1 );
   end if;

   while Map_Changed loop
      Iteration := Iteration + 1;
      Iterate ( Map_Changed, Iteration );
   end loop;

   Text_IO.Put_Line ( "there were" & Iteration'Image & " iterations");

end Day25;
