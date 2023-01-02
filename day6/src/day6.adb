--  Advent of Code 2021
--
--  John Perry
--
--  Day 6: Lanternfish
--
--  part 1: given a recursive growth model for lanternfish,
--     how many do we expect to have after 80 days?
--
--  part 2: ...after 256 days?

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day6 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   type Part_Number is (First, Second);

   --  SECTION
   --  global types and variables

   type Lanternfishonacci is record
      Age : Natural;
   end record;

   package Lanternfishonacci_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Lanternfishonacci
      );

   School : Lanternfishonacci_Vectors.Vector;

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
         Input_String  : constant String := Text_IO.Get_Line (Input_File);
         Pos           : Positive := 1;
         Value         : Natural;
      begin

         while Pos <= Input_String'Length loop
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), Value, Pos);
            School.Append (Lanternfishonacci'(Age => Value));
            Pos := Pos + 2;
         end loop;

      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   procedure Generate_Fish (Part : Part_Number) is
   --  simulates the lanternfish growth rate over a certain number of days
   --  depending on which part we're doing

      Fish_Count : Long_Long_Integer := 0;

      Days : constant Positive := (if Part = First then 80 else 256);
      Spawning_On_Day : array (1 .. Days) of Long_Long_Integer
         := (others => 0);
   begin
      --  setup initial spawns
      for Fish of School loop
         Spawning_On_Day (Fish.Age + 1) := @ + 1;
      end loop;
      --  initialize fish count
      Fish_Count := Long_Long_Integer (School.Length);
      for Day in 1 .. Days loop
         Fish_Count := Fish_Count + Spawning_On_Day (Day);
         if Day + 7 <= Days then
            Spawning_On_Day (Day + 7) := @ + Spawning_On_Day (Day);
            if Day + 9 <= Days then
               Spawning_On_Day (Day + 9) := @ + Spawning_On_Day (Day);
            end if;
         end if;
      end loop;
      Text_IO.Put_Line ("after" & Days'Image & " days there are"
                        & Fish_Count'Image & " fish");
   end Generate_Fish;

begin

   Read_Input;
   --  Text_IO.Put_Line ("read in" & School.Length'Image & " fish");

   Generate_Fish (First);
   Generate_Fish (Second);

end Day6;
