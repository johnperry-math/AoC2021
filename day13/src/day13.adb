--  Advent of Code 2021
--
--  John Perry
--
--  Day 13: Transparent Origami
--
--  part 1: perform the first fold and report the number of dots
--
--  part 2: perform the remaining folds
--  and report the 8 capital letters that appear

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

procedure Day13 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   type Position is record
      X, Y : Natural;
   end record;

   function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (P.Y * 2_000 + P.X));

   package Dot_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Position,
       Hash         => Position_Hash,
       Equivalent_Elements => "="
      );

   Dots : Dot_Sets.Set;
   --  the dots initially read in

   type Fold_Dir is (Horizontal, Vertical);

   type Fold is record
      Value : Natural;
      Axis  : Fold_Dir;
   end record;

   package Fold_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Fold
      );

   Folds : Fold_Vectors.Vector;
   -- the fold instructions

   --  SECTION
   --  I/O

   procedure Get_Integer (S      : String;
                          Result : out Integer;
                          Pos    : in out Positive)
   is
   --  Ada.Text_IO.Integer_IO has issues with "(\d)*:",
   --  so I have to roll my own

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

   function Get_Position (S : String) return Position is
   --  assuming S has the form "x,y", returns the corresponding position

      Pos : Positive := S'First;
      X, Y : Natural;

   begin

      Get_Integer (S, X, Pos);
      Pos := Pos + 1;
      Get_Integer (S, Y, Pos);

      return ((X, Y));

   end Get_Position;

   function Get_Fold (S : String) return Fold is
   --  assuming S has the form "fold along (x|y)=value",
   --  returns the corresponding Fold object

      Value : Natural;                  --  ordinate along which to fold
      Pos   : Positive := S'First + 13; --  position to find the value
      Axis  : constant Fold_Dir         --  horizontal or vertical axis line?
         := (if S (S'First + 11) = 'x' then Vertical else Horizontal);

   begin

      Get_Integer (S, Value, Pos);
      return ((Value, Axis));

   end Get_Fold;

   procedure Read_Input is
   --  reads in the dots, then the folds

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
         begin

            if Input_String'Length = 0 then --  blank line; do nothing
               null;

            elsif Input_String (1) in '0' .. '9' then -- position
               Dots.Include (Get_Position (Input_String));

            else -- fold
               Folds.Append (Get_Fold (Input_String));

            end if;

         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Perform_Fold (Dots        : Dot_Sets.Set;
                          Fold_Number : Positive := Folds.First_Index)
                          return Dot_Sets.Set
   is
   --  performs the fold at Folds (Fold_Number) to the set of Dots,
   --  returning the result as a new set; the original is unaffected

      New_Dots : Dot_Sets.Set;
      Min_Value : Integer := 0;
      F : Fold renames Folds (Fold_Number);

   begin

      Text_IO.Put_Line ("performing " & F.Axis'Image
                        & " fold at" & F.Value'Image);

      --  first determine how far across the axis the dots go
      for Dot of Dots loop

         if F.Axis = Horizontal then
            Min_Value := Integer'Min (Min_Value,
                                      F.Value - (Dot.Y - F.Value));

         else
            Min_Value := Integer'Min (Min_Value,
                                      F.Value - (Dot.X - F.Value));

         end if;

      end loop;

      --  now adjust dots : those prior to the fold are translated, if need be;
      --  the others are reflected
      for Dot of Dots loop

         if F.Axis = Horizontal then

            if Dot.Y < F.Value then
               New_Dots.Include ((Dot.X, Dot.Y - Min_Value));

            else
               New_Dots.Include ((Dot.X,
                                 (F.Value - Min_Value) - (Dot.Y - F.Value)));

            end if;

         else

            if Dot.X < F.Value then
               New_Dots.Include ((Dot.X - Min_Value, Dot.Y));

            else
               New_Dots.Include (((F.Value - Min_Value) - (Dot.X - F.Value),
                                 Dot.Y));

            end if;

         end if;

      end loop;

      return New_Dots;

   end Perform_Fold;

   procedure Put_Dots (Dots : Dot_Sets.Set) is
   --  displays the image shown by the dots

      Min_X, Min_Y : Natural := Natural'Last;
      Max_X, Max_Y : Natural := Natural'First;

   begin

      --  find min, max x & y values
      for Dot of Dots loop
         Min_X := Natural'Min (Min_X, Dot.X);
         Min_Y := Natural'Min (Min_Y, Dot.Y);
         Max_X := Natural'Max (Max_X, Dot.X);
         Max_Y := Natural'Max (Max_Y, Dot.Y);
      end loop;

      --  draw to an array, then print the array
      declare
         Image : array (Min_X .. Max_X, Min_Y .. Max_Y) of Character
            := (others => (others => '.'));
      begin

         for Dot of Dots loop
            Image (Dot.X, Dot.Y) := '#';
         end loop;

         for Y in Image'Range (2) loop
            for X in Image'Range (1) loop
               Text_IO.Put (Image (X, Y));
            end loop;
            Text_IO.New_Line;
         end loop;

      end;

   end Put_Dots;

begin

   Read_Input;

   Text_IO.Put_Line (Dots.Length'Image & " dots");
   Text_IO.Put_Line (Folds.Length'Image & " folds");

   --  parts 1 and 2 together
   declare
      --  part 1 here
      New_Dots : Dot_Sets.Set := Perform_Fold (Dots);
   begin

      Text_IO.Put_Line ("after one fold, there are" & New_Dots.Length'Image);

      --  part 2 here
      for I in 2 .. Folds.Last_Index loop
         New_Dots := Perform_Fold (New_Dots, I);
      end loop;

      Put_Dots (New_Dots);

   end;

end Day13;
