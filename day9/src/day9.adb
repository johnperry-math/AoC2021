--  Advent of Code 2021
--
--  John Perry
--
--  Day 9: Smoke Basin
--
--  part 1: find the sum of the low points' risk levels
--
--  part 2: find the product of the sizes of the three largest basins

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

with Components.Queue;

procedure Day9 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   Num_Rows : constant Positive := (if Doing_Example then 5 else 100);
   Num_Cols : constant Positive := (if Doing_Example then 10 else 100);
   Height_Map : array (1 .. Num_Rows, 1 .. Num_Cols) of Natural;

   type Position is record
      Row, Col : Positive;
   end record;

   function Position_Hash (Pos : Position) return Ada.Containers.Hash_Type
   is
      (Ada.Containers.Hash_Type (Pos.Row * 10 + Pos.Col));

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => Position,
       Hash                => Position_Hash,
       Equivalent_Elements => "="
      );

   package Position_Queues is new Components.Queue
      (Element_Type => Position);

   --  SECTION
   --  I/O

   procedure Read_Input is
   --  read in the height map

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      for Row in 1 .. Num_Rows loop

         declare
            Input_String : constant String := Text_IO.Get_Line (Input_File);
         begin

            for Col in 1 .. Num_Cols loop
               Height_Map (Row, Col) := Character'Pos (Input_String (Col))
                  - Character'Pos ('0');
            end loop;

         end;

      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Is_Low_Point (Row, Col : Positive) return Boolean is
   --  returns True iff the indicated position has a height lower
   --  than its four cardinal neighbors (NSEW)
      ((if Row > 1 then Height_Map (Row - 1, Col) > Height_Map (Row, Col))
       and then
          (if Row < Num_Rows then
           Height_Map (Row + 1, Col) > Height_Map (Row, Col))
       and then
          (if Col > 1 then Height_Map (Row, Col - 1) > Height_Map (Row, Col))
       and then
          (if Col < Num_Cols then
           Height_Map (Row, Col + 1) > Height_Map (Row, Col))
      );

   --  SECTION
   --  PART 1

   function Risk_Level (Row, Col : Positive) return Positive is
   --  returns the risk level of the indicated position,
   --  which is one more than the height
      (Height_Map (Row, Col) + 1);

   function Sum_Risk_Levels return Positive is
   --  returns the sum of the lowest points' risk levels

      Result : Natural := 0;

   begin

      for Row in 1 .. Num_Rows loop
         for Col in 1 .. Num_Cols loop

            if Is_Low_Point (Row, Col) then
               Text_IO.Put_Line ("low point at" & Row'Image & Col'Image);
               Result := Result + Risk_Level (Row, Col);
            end if;

         end loop;
      end loop;

      return Result;

   end Sum_Risk_Levels;

   --  SECTION
   --  PART 2

   function Size_Of_Basin (Row, Col : Positive) return Positive is
   --  returns the size of the basin at the indicated position
   --  (expands until it reaches vents of height 9, which form the boundary
   --
   --  basically breadth-first search

      Positions : Position_Sets.Set;         --  known basin positions
      To_Do     : Position_Queues.Queue      --  positions to explore further
         := Position_Queues.With_Size (100);

   begin

      --  prime the search
      Positions.Include ((Row, Col));
      To_Do.Push ((Row, Col));

      while not To_Do.Is_Empty loop

         declare
            Pos : Position;
         begin

            To_Do.Pop (Pos);

            if Pos.Row > 1 and then
               Height_Map (Pos.Row - 1, Pos.Col) < 9 and then
               not Positions.Contains ((Pos.Row - 1, Pos.Col))
            then
               Positions.Include ((Pos.Row - 1, Pos.Col));
               To_Do.Push ((Pos.Row - 1, Pos.Col));
            end if;

            if Pos.Row < Num_Rows and then
               Height_Map (Pos.Row + 1, Pos.Col) < 9 and then
               not Positions.Contains ((Pos.Row + 1, Pos.Col))
            then
               Positions.Include ((Pos.Row + 1, Pos.Col));
               To_Do.Push ((Pos.Row + 1, Pos.Col));
            end if;

            if Pos.Col > 1 and then
               Height_Map (Pos.Row, Pos.Col - 1) < 9 and then
               not Positions.Contains ((Pos.Row, Pos.Col - 1))
            then
               Positions.Include ((Pos.Row, Pos.Col - 1));
                  To_Do.Push ((Pos.Row, Pos.Col - 1));
            end if;

            if Pos.Col < Num_Cols and then
               Height_Map (Pos.Row, Pos.Col + 1) < 9 and then
               not Positions.Contains ((Pos.Row, Pos.Col + 1))
            then
               Positions.Include ((Pos.Row, Pos.Col + 1));
               To_Do.Push ((Pos.Row, Pos.Col + 1));
            end if;

         end;

      end loop;

      Text_IO.Put_Line ("basin at" & Row'Image & Col'Image
                        & " has size" & Positions.Length'Image);
      return Positive (Positions.Length);

   end Size_Of_Basin;

   function Product_Of_Largest_Basin_Sizes return Positive is
   --  returns the product of the sizes of the three largest basins

      First, Second, Third : Positive := 1;

   begin

      for Row in 1 .. Num_Rows loop
         for Col in 1 .. Num_Cols loop

            if Is_Low_Point (Row, Col) then

               declare
                  This_Size : constant Positive := Size_Of_Basin (Row, Col);
               begin

                  if This_Size >= First then
                     Third := Second;
                     Second := First;
                     First := This_Size;

                  elsif This_Size >= Second then
                     Third := Second;
                     Second := This_Size;

                  elsif This_Size > Third then
                     Third := This_Size;

                  end if;

               end;

            end if;

         end loop;
      end loop;

      return First * Second * Third;

   end Product_Of_Largest_Basin_Sizes;

begin

   Read_Input;

   Text_IO.Put_Line ("the risk levels sum to" & Sum_Risk_Levels'Image);

   Text_IO.Put_Line ("the products of the sizes of the three largest basins is"
                     & Product_Of_Largest_Basin_Sizes'Image);

end Day9;
