--  Advent of Code 2021
--
--  John Perry
--
--  Day 15: Chiton
--
--  part 1: find the lowest-risk path through a 100x100 maze of chitons
--
--  part 2: oops, it's 500x500, with the other tiles' values
--     based on the first's

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

use type Ada.Containers.Count_Type;

procedure Day15 is

   package Text_IO renames Ada.Text_IO;
   package Positive_IO is new Ada.Text_IO.Integer_IO (Num => Positive);

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  the maze itself

   Dimension : constant Positive := (if Doing_Example then 10 else 100);

   type Maze_Type is array (Positive range <>, Positive range <>) of Positive;
   Maze_Part_1 : Maze_Type (1 .. Dimension, 1 .. Dimension);
   Maze_Part_2 : Maze_Type (1 .. 5 * Dimension, 1 .. 5 * Dimension);

   --  SUBSECTION
   --  tracking positions in the maze

   type Position is record
      Row, Col : Integer;
   end record;
   --  location in the maze of chiton

   --  SUBSECTION
   --  moving through the maze

   Deltas : constant array (1 .. 4) of Position
      := ((-1, 0), (1, 0), (0, -1), (0, 1));
   --  legal moves

   function "+" (Left, Right : Position) return Position is
   --  component-wise addition
      ((Left.Row + Right.Row, Left.Col + Right.Col));

   --  SECTION
   --  I/O

   package Path_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Position
      );

   pragma Warnings (Off, "procedure ""Put_Path"" is not referenced");
   procedure Put_Path (Path : Path_Vectors.Vector) is
   --  writes out the path in the format ( row, col ) ...
   begin
      for P of Path loop
         Text_IO.Put ("(" & P.Row'Image & "," & P.Col'Image & " ) ");
      end loop;
      Text_IO.New_Line;
   end Put_Path;
   pragma Warnings (On, "procedure ""Put_Path"" is not referenced");

   procedure Read_Input is
   --  reads in the maze for part 1

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      for Row in 1 .. Dimension loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
         begin

            for Col in 1 .. Dimension loop
               Maze_Part_1 (Row, Col)
                  := Character'Pos (Input_String (Col)) - Character'Pos ('0');
            end loop;

         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   pragma Warnings (Off, "procedure ""Put_Maze"" is not referenced");
   procedure Put_Maze (M : Maze_Type) is
   --  prints the maze
   begin

      for Row in M'Range (1) loop
         for Col in M'Range (2) loop

            Positive_IO.Put (M (Row, Col), 0);

         end loop;
         Text_IO.New_Line;
      end loop;

   end Put_Maze;
   pragma Warnings (On, "procedure ""Put_Maze"" is not referenced");

   --  SECTION
   --  PARTS 1 AND 2

   function Lowest_Risk (Maze : Maze_Type) return Positive is
   --  uses a breadth-first search to find the least risky path through Maze

      --  first we define some functions and packages specific to this one

      --  SUBSECTION
      --  the objective

      function Path_Risk (P : Path_Vectors.Vector) return Natural
      --  determines the risk of path P by adding the maze values
      --  for each position list in P
      --
      --  we assume that P's first position is (1, 1),
      --  and subtract that position's value from the risk,
      --  in accordance with the rules
      is

         Result : Natural := 0;

      begin

         for Pos of P loop
            Result := Result + Maze (Pos.Row, Pos.Col);
         end loop;

         return Result - Maze (1, 1);

      end Path_Risk;

      --  SUBSECTION
      --  tracking our work

      package Path_Queue_Interfaces
      is new Ada.Containers.Synchronized_Queue_Interfaces
         (Element_Type => Path_Vectors.Vector
         );

      package Path_Queues is new Ada.Containers.Unbounded_Priority_Queues
         (Queue_Interfaces => Path_Queue_Interfaces,
          Queue_Priority   => Natural,
          Get_Priority     => Path_Risk,
          Before           => "<"
         );

      function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
         (Ada.Containers.Hash_Type (P.Row * 101 + P.Col));

      package Position_Maps is new Ada.Containers.Hashed_Maps
         (Key_Type            => Position,
          Hash                => Position_Hash,
          Equivalent_Keys     => "=",
          Element_Type        => Natural
         );

      To_Do             : Path_Queues.Queue;
      Positions_Visited : Position_Maps.Map;

   begin

      --  prime the queue with a path starting at (1, 1)
      declare
         First_Path : Path_Vectors.Vector;
      begin

         First_Path.Append (Position'(1, 1));
         To_Do.Enqueue (First_Path);
         Positions_Visited.Include ((1, 1), 0);

      end;

      --  breadth-first search to find the lowest-cost path
      while To_Do.Current_Use > 0 loop

         --  Text_IO.Put_Line (To_Do.Current_Use'Image & " paths to consider");
         --  Text_IO.Put_Line (Posi.Length'Image & " paths have been queued");

         declare
            Risk          : Natural;
            Last, Forward : Position;
            Path, Next    : Path_Vectors.Vector;
         begin

            To_Do.Dequeue (Path);
            Last := Path.Last_Element;

            --  for each potential step forward,
            --  check whether there is a shorter path to this position
            --  compared to what was known before
            --  if so, enqueue it
            for D of Deltas loop

               Forward := Last + D;

               if Forward.Row in Maze'Range (1)
                  and then Forward.Col in Maze'Range (2)
               then

                  Next := Path_Vectors.Copy (Path);
                  Next.Append (Forward);
                  Risk := Path_Risk (Next);

                  if Next.Last_Element = ((Maze'Last (1), Maze'Last (2))) then
                     return Risk;
                  end if;

                  if not Positions_Visited.Contains (Forward)
                     or else Positions_Visited (Forward) > Risk
                  then
                     Positions_Visited.Include (Forward, Risk);
                     To_Do.Enqueue (Next);
                  end if;

               end if;

            end loop;

         end;

      end loop;

      return Positive'Last;

   end Lowest_Risk;

   procedure Create_Second_Maze is
   --  creates the second maze according to the problem's guidance:
   --  it consists of 5x5 tiles based on the original maze;
   --  the upper-left tile is identical to the original maze,
   --  while the ixj tile adds (i - 1) + (j - 1) to the values of each tile,
   --  with tiles over 9 "resetting" to a lower corresponding value
   begin

      for Maze_Row in 1 .. 5 loop
         for Maze_Col in 1 .. 5 loop

            for Row in 1 .. Dimension loop
               for Col in 1 .. Dimension loop

                  declare
                     New_Row : constant Positive
                        := (Maze_Row - 1) * Dimension + Row;
                     New_Col : constant Positive
                        := (Maze_Col - 1) * Dimension + Col;
                  begin

                     Maze_Part_2 (New_Row, New_Col)
                        := Maze_Part_1 (Row, Col) + Maze_Row + Maze_Col - 2;
                     while Maze_Part_2 (New_Row, New_Col) > 9 loop
                        Maze_Part_2 (New_Row, New_Col) := @ - 9;
                     end loop;

                  end;

               end loop;
            end loop;

         end loop;
      end loop;

   end Create_Second_Maze;

begin

   Read_Input;

   Text_IO.Put_Line ("the lowest cost to traverse the maze is"
                     & Lowest_Risk (Maze_Part_1)'Image);

   Create_Second_Maze;
   Text_IO.Put_Line ("the lowest cost to traverse the larger maze is"
                     & Lowest_Risk (Maze_Part_2)'Image);

end Day15;
