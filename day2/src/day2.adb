--  Advent of Code 2021
--
--  John Perry
--
--  Day 2: Dive!
--
--  part 1: determine the sub's position after a lot of motion
--
--  part 2: determine the sub's position after realizing that "up" and "down"
--     don't affect depth directly, but rather affect aim

with Ada.Text_IO;

procedure Day2 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   type Position is record
      Forward : Natural := 0;
      Depth   : Natural := 0;
   end record;

   procedure Forward (P : in out Position; Amount : Natural) is
   begin
      P.Forward := P.Forward + Amount;
   end Forward;

   procedure Up (P : in out Position; Amount : Natural) is
   begin
      P.Depth := P.Depth - Amount;
   end Up;

   procedure Down (P : in out Position; Amount : Natural) is
   begin
      P.Depth := P.Depth + Amount;
   end Down;

   procedure Position_And_Depth is
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );
      Input_File  : Text_IO.File_Type;
      Submarine   : Position;
      Bad_Command : exception;
      Value       : Natural;
      Pos         : Positive;
   begin
      Text_IO.Open (Input_File, Text_IO.In_File, Filename);
      while not Text_IO.End_Of_File (Input_File) loop
         declare
            Input_String : constant String := Text_IO.Get_Line (Input_File);
         begin
            if Input_String (1) = 'f' then
               Pos := 9;
               Natural_IO.Get
                  (Input_String (Pos .. Input_String'Length), Value, Pos);
               Forward (Submarine, Value);
            elsif Input_String (1) = 'u' then
               Pos := 4;
               Natural_IO.Get
                  (Input_String (Pos .. Input_String'Length), Value, Pos);
               Up (Submarine, Value);
            elsif Input_String (1) = 'd' then
               Pos := 6;
               Natural_IO.Get
                  (Input_String (Pos .. Input_String'Length), Value, Pos);
               Down (Submarine, Value);
            else
               raise Bad_Command;
            end if;
         end;
      end loop;
      Text_IO.Close (Input_File);
      Text_IO.Put_Line ("position code is"
                        & Natural'Image (Submarine.Depth * Submarine.Forward)
                       );
   end Position_And_Depth;

   type Aimed_Position is record
      Forward : Natural := 0;
      Depth   : Natural := 0;
      Aim     : Integer := 0;
   end record;

   procedure Forward (P : in out Aimed_Position; Amount : Natural) is
   begin
      P.Forward := P.Forward + Amount;
      P.Depth := P.Depth + P.Aim * Amount;
   end Forward;

   procedure Up (P : in out Aimed_Position; Amount : Natural) is
   begin
      P.Aim := P.Aim - Amount;
   end Up;

   procedure Down (P : in out Aimed_Position; Amount : Natural) is
   begin
      P.Aim := P.Aim + Amount;
   end Down;

   procedure Aimed_Position_And_Depth is
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );
      Input_File  : Text_IO.File_Type;
      Submarine   : Aimed_Position;
      Bad_Command : exception;
      Value       : Natural;
      Pos         : Positive;
   begin
      Text_IO.Open (Input_File, Text_IO.In_File, Filename);
      while not Text_IO.End_Of_File (Input_File) loop
         declare
            Input_String : constant String := Text_IO.Get_Line (Input_File);
         begin
            if Input_String (1) = 'f' then
               Pos := 9;
               Natural_IO.Get
                  (Input_String (Pos .. Input_String'Length), Value, Pos);
               Forward (Submarine, Value);
            elsif Input_String (1) = 'u' then
               Pos := 4;
               Natural_IO.Get
                  (Input_String (Pos .. Input_String'Length), Value, Pos);
               Up (Submarine, Value);
            elsif Input_String (1) = 'd' then
               Pos := 6;
               Natural_IO.Get
                  (Input_String (Pos .. Input_String'Length), Value, Pos);
               Down (Submarine, Value);
            else
               raise Bad_Command;
            end if;
         end;
      end loop;
      Text_IO.Close (Input_File);
      Text_IO.Put_Line ("aimed position code is"
                        & Natural'Image (Submarine.Depth * Submarine.Forward)
                       );
   end Aimed_Position_And_Depth;

begin
   Position_And_Depth;
   Aimed_Position_And_Depth;
end Day2;
