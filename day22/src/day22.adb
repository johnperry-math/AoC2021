f-- Advent of Code 2021
--
-- John Perry
--
-- Day 22: Reactor Reboot
--
-- part 1: count lit cubes for instructions restricted to -50 .. 50
--
-- part 2: count lit cubes for all instructions

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day22 is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO ( Num => Integer );
   package Long_Long_IO is new Ada.Text_IO.Integer_IO
      ( Num => Long_Long_Integer );

   Doing_Example : constant Boolean := False;

   Doing_Part_1 : constant Boolean := False;

   -- SECTION
   -- global types and variables

   subtype Init_Range is Integer range -50 .. 50;

   type Setting is ( On, Off );

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

   pragma Warnings ( Off, "procedure ""Put_Instruction"" is never used" );
   procedure Put_Instruction ( Cuboid : Instruction ) is
   begin
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
   end Put_Instruction;
   pragma Warnings ( On, "procedure ""Put_Instruction"" is never used" );

   pragma Warnings ( Off, "procedure ""Put_Instructions"" is never used" );
   procedure Put_Instructions is
   begin
      for Cuboid of Instructions loop
         Put_Instruction ( Cuboid );
      end loop;
   end Put_Instructions;
   pragma Warnings ( On, "procedure ""Put_Instructions"" is never used" );

   -- SECTION
   -- common to both Parts 1 and 2

   -- SECTION
   -- Part 1

   function Is_Initializer ( I : Instruction ) return Boolean is
   -- returns True iff I's coordinates lie within Init_Range
      ( not Doing_Part_1
        or else ( I.X1 in Init_Range and then I.X2 in Init_Range
           and then I.Y1 in Init_Range and then I.Y2 in Init_Range
           and then I.Z1 in Init_Range and then I.Z2 in Init_Range )
      );

   function Intersection ( Left, Right : Instruction ) return Instruction is
   -- returns the cuboid that lies in the intersection of Left and Right
      ( (
        ( if Right.Set = On then Off else On ),
        Integer'Max ( Integer'Min ( Left.X2, Right.X1 ),
           Integer'Min ( Right.X2, Left.X1 ) ),
        Integer'Min ( Integer'Max ( Left.X2, Right.X1 ),
           Integer'Max ( Right.X2, Left.X1 ) ),
        Integer'Max ( Integer'Min ( Left.Y2, Right.Y1 ),
           Integer'Min ( Right.Y2, Left.Y1 ) ),
        Integer'Min ( Integer'Max ( Left.Y2, Right.Y1 ),
           Integer'Max ( Right.Y2, Left.Y1 ) ),
        Integer'Max ( Integer'Min ( Left.Z2, Right.Z1 ),
           Integer'Min ( Right.Z2, Left.Z1 ) ),
        Integer'Min ( Integer'Max ( Left.Z2, Right.Z1 ),
           Integer'Max ( Right.Z2, Left.Z1 ) )
          ) );

   function Intersects ( Left, Right : Instruction ) return Boolean is
   -- returns True if and only if Left and Right's cuboids
   -- have a nonempty intersection
      ( (
        Left.X1 in Right.X1 .. Right.X2
        or else Left.X2 in Right.X1 .. Right.X2
        or else Right.X1 in Left.X1 .. Left.X2
        or else Right.X2 in Left.X1 .. Left.X2
       )

        and then
           (
            Left.Y1 in Right.Y1 .. Right.Y2
            or else Left.Y2 in Right.Y1 .. Right.Y2
            or else Right.Y1 in Left.Y1 .. Left.Y2
            or else Right.Y2 in Left.Y1 .. Left.Y2
           )

        and then
           (
            Left.Z1 in Right.Z1 .. Right.Z2
            or else Left.Z2 in Right.Z1 .. Right.Z2
            or else Right.Z1 in Left.Z1 .. Left.Z2
            or else Right.Z2 in Left.Z1 .. Left.Z2
           )

       );

   procedure Apply ( C                : Instruction;
                     Old_Instructions : in out Instruction_Vectors.Vector
                    )
   is
   -- applies the new instruction C:
   -- 1. if C.Set is on, we append C to `Old_Instructions`
   -- 2. we also append every non-empty intersection of `C` with an old element,
   --    only with the _opposite_ sign as the old one's
   -- this essentially creates a list of cuboids that satisfy DeMorgan's Laws
   -- and allow us to count by adding and subtracting
   --
   -- in particular, | A u B | = | A | + | B| - | A n C | and so forth

      New_Instructions : Instruction_Vectors.Vector;

   begin

      for Instr of Old_Instructions loop
         if Intersects ( C, Instr ) then
            New_Instructions.Append ( Intersection ( C, Instr ) );
         end if;
      end loop;

      if C.Set = On then
         Old_Instructions.Append ( C );
      end if;

      Old_Instructions.Append_Vector ( New_Instructions );

   end Apply;

   function Volume ( Instr : Instruction ) return Long_Long_Integer is
   -- returns the volume of Instr
      ( Long_Long_Integer ( Instr.X2 - Instr.X1 + 1 )
            * Long_Long_Integer ( Instr.Y2 - Instr.Y1 + 1 )
            * Long_Long_Integer ( Instr.Z2 - Instr.Z1 + 1 )
      );

   function Initialize return Long_Long_Integer is

      Cubes_Lit         : Long_Long_Integer := 0;
      Full_Instructions : Instruction_Vectors.Vector;

   begin

      for Instr of Instructions loop
         if Is_Initializer ( Instr ) then
            Apply ( Instr, Full_Instructions );
         end if;
      end loop;

      for Instr of Full_Instructions loop

         if Instr.Set = On then
            Cubes_Lit := Cubes_Lit + Volume ( Instr );

         else
            Cubes_Lit := Cubes_Lit - Volume ( Instr );

         end if;

      end loop;

      return Cubes_Lit;

   end Initialize;

begin

   Read_Input;

   -- part 1 or 2; see global constants
   Long_Long_IO.Put ( Initialize ); Text_IO.Put_Line ( " are on" );

end Day22;
