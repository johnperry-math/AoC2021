--  Advent of Code 2021
--
--  John Perry
--
--  Day 7: The Treachery of Whales
--
--  part 1: what's the lowest-cost position for crabs in submarines to align?
--
--  part 2: same, but with a different measure of cost

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day7 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   type Part_Number is (First, Second);

   --  SECTION
   --  global types and variables

   type Crab_Position is new Natural;

   package Crab_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Crab_Position
      );

   Crabs : Crab_Vectors.Vector;

   Min_Pos, Max_Pos : Crab_Position;

   --  SECTION
   --  I/O

   procedure Read_Input is
   --  reads the fish ages

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      declare
         Input_String : constant String := Text_IO.Get_Line (Input_File);
         Pos          : Positive := 1;
         Val          : Natural;
      begin
         Min_Pos := Crab_Position (Natural'Last);
         Max_Pos := 0;
         while Pos <= Input_String'Length loop
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), Val, Pos);
            Crabs.Append (Crab_Position (Val));
            Min_Pos := Crab_Position'Min (Min_Pos, Crab_Position (Val));
            Max_Pos := Crab_Position'Max (Max_Pos, Crab_Position (Val));
            Pos := Pos + 2;
         end loop;
      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   type Fuel_Expended is new Natural;

   function Minimize_Alignment (Part : Part_Number := First)
                                return Fuel_Expended
   is
      Cost : Fuel_Expended := Fuel_Expended'Last;
   begin
      for Pos in Min_Pos .. Max_Pos loop
         declare
            This_Cost : Fuel_Expended := 0;
         begin
            for Crab of Crabs loop
               if Part = First then
                  This_Cost := This_Cost + Fuel_Expended (abs (Crab - Pos));
               else
                  declare
                     Distance : constant Crab_Position := abs (Crab - Pos);
                  begin
                     This_Cost := This_Cost
                        + Fuel_Expended (Distance * (Distance + 1) / 2);
                  end;
               end if;
            end loop;
            Cost := Fuel_Expended'Min (Cost, This_Cost);
         end;
      end loop;
      return Cost;
   end Minimize_Alignment;

begin

   Read_Input;
   Text_IO.Put_Line ("there are" & Crabs.Length'Image & " crabs");

   Text_IO.Put_Line ("it will cost" & Minimize_Alignment'Image
                     & " fuel to align");

   Text_IO.Put_Line ("er, no... it will cost"
                     & Minimize_Alignment (Second)'Image
                     & " fuel to align");

end Day7;
