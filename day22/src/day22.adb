-- Advent of Code 2021
--
-- John Perry
--
-- Day 22: Reactor Reboot
--
-- part 1:
--
-- part 2:

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day22 is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );

   Doing_Example : constant Boolean := False;

   -- SECTION
   -- global types and variables

   type Setting is ( On, Off );
   subtype Init_Range is Integer range -50 .. 50;

   type Init_Cubes_Array is array ( Init_Range, Init_Range, Init_Range )
      of Setting;
   pragma Pack ( Init_Cubes_Array );
   Init_Cubes : Init_Cubes_Array
      := ( others => ( others => ( others => Off ) ) );

   type Instruction is record
      Set                    : Setting;
      X1, X2, Y1, Y2, Z1, Z2 : Integer;
   end record;

   package Instruction_Vectors is new Ada.Containers.Vectors
      ( Index_Type => Positive,
        Element_Type => Instruction
      );

   Instructions : Instruction_Vectors.Vector;

   -- SECTION
   -- I/O

   procedure Get_Integer (S      : String;
                          Result : out Integer;
                          Pos    : in out Positive)
   is
   -- Ada.Text_IO.Integer_IO has issues with "(\d)*:",
   -- so I have to roll my own;
   -- Pos needs indicate where to start reading in S,
   -- and after termination it indicates the first non-digit value
   -- (possibly out of bounds)

      Is_Negative : constant Boolean := S (Pos) = '-';

   begin

      if Is_Negative then
         Pos := Pos + 1;
      end if;

      Result := 0;
      while Pos <= S'Length and then S (Pos) in '0' .. '9' loop
         Result := Result * 10;
         Result := Result + Character'Pos (S (Pos)) - Character'Pos ('0');
         Pos := Pos + 1;
      end loop;

      if Is_Negative then
         Result := -Result;
      end if;

   end Get_Integer;

   procedure Read_Input is
   -- reads the input string and extract the target region dimensions

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop
         declare
            S : String := Text_IO.Get_Line ( Input_File );
            Set : Setting := Off;
            Offset : Positive;
            X1, X2, Y1, Y2, Z1, Z2 : Integer;
         begin
            if S ( 1 .. 2 ) = "on" then
               Set := On;
               Offset := 6;
            else
               Set := Off;
               Offset := 7;
            end if;
            Get_Integer ( S, X1, Offset );
            Offset := Offset + 2;
            Get_Integer ( S, X2, Offset );
            Offset := Offset + 3;
            Get_Integer ( S, Y1, Offset );
            Offset := Offset + 2;
            Get_Integer ( S, Y2, Offset );
            Offset := Offset + 3;
            Get_Integer ( S, Z1, Offset );
            Offset := Offset + 2;
            Get_Integer ( S, Z2, Offset );
            Offset := Offset + 3;
            Instructions.Append ( Instruction'( Set, X1, X2, Y1, Y2, Z1, Z2) );
         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   procedure Put_Instructions is
   begin
      for Cuboid of Instructions loop
         Text_IO.Put ( ( if Cuboid.Set = On then "on " else "off ") );
         Integer_IO.Put ( Cuboid.X1, 0);
         Text_IO.Put ( ".." );
         Integer_IO.Put ( Cuboid.X2, 0);
         Text_IO.Put ( ',' );
         Integer_IO.Put ( Cuboid.Y1, 0);
         Text_IO.Put ( ".." );
         Integer_IO.Put ( Cuboid.Y2, 0);
         Text_IO.Put ( ',' );
         Integer_IO.Put ( Cuboid.Z1, 0);
         Text_IO.Put ( ".." );
         Integer_IO.Put ( Cuboid.Z2, 0);
         Text_IO.New_Line;
      end loop;
   end Put_Instructions;

   -- SECTION
   -- common to both Parts 1 and 2

   -- SECTION
   -- Part 1

   function Is_Initializer ( I : Instruction ) return Boolean is
      ( I.X1 in Init_Range and then I.X2 in Init_Range
        and then I.Y1 in Init_Range and then I.Y2 in Init_Range
        and then I.Z1 in Init_Range and then I.Z2 in Init_Range
      );

   function Number_Cubes_Set return Positive is
      Result : Natural := 0;
   begin
      for X in Init_Range loop
         for Y in Init_Range loop
            for Z in Init_Range loop
               if Init_Cubes ( X, Y, Z ) = On then
                  Result := Result + 1;
               end if;
            end loop;
         end loop;
      end loop;
      return Result;
   end Number_Cubes_Set;

   procedure Initialize is
   begin
      for Cuboid of Instructions loop
         if Is_Initializer ( Cuboid ) then
            for X in Cuboid.X1 .. Cuboid.X2 loop
               for Y in Cuboid.Y1 .. Cuboid.Y2 loop
                  for Z in Cuboid.Z1 .. Cuboid.Z2 loop
                     Init_Cubes ( X, Y, Z ) := Cuboid.Set;
                  end loop;
               end loop;
            end loop;
         end if;
      end loop;
   end Initialize;

begin

   Read_Input;
   Put_Instructions;

   -- part 1
   Initialize;
   Integer_IO.Put ( Number_Cubes_Set, 0 ); Text_IO.Put_Line ( " are on" );

end Day22;
