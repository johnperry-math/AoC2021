--  Advent of Code 2021
--
--  John Perry
--
--  Day 17: Trick Shot
--
--  part 1: Find the highest y-value
--     possible where the probe reaches the target
--
--  part 2: Find the number of trajectories where the probe reaches the target

pragma Ada_2020;

with Ada.Text_IO;

procedure Day17 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   type Position is record
      X, Y : Integer;
   end record;

   type Velocity is record
      Dx, Dy : Integer;
   end record;

   type Target is record
      X_Min, X_Max, Y_Min, Y_Max : Integer;
   end record;

   Target_Region : Target;

   function In_Target_Region (P : Position) return Boolean is
   --  true if and only if P is in Target_Region
      (P.X in Target_Region.X_Min .. Target_Region.X_Max
       and then P.Y in Target_Region.Y_Min .. Target_Region.Y_Max);

   --  SECTION
   --  I/O

   procedure Get_Integer (S      : String;
                          Result : out Integer;
                          Pos    : in out Positive)
   is
   --  Ada.Text_IO.Integer_IO has issues with "(\d)*:",
   --  so I have to roll my own;
   --  Pos needs indicate where to start reading in S,
   --  and after termination it indicates the first non-digit value
   --  (possibly out of bounds)

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
   --  reads the input string and extract the target region dimensions

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      declare
         S : constant String := Text_IO.Get_Line (Input_File);
         Pos : Positive;
      begin
         Pos := 16;
         Get_Integer (S, Target_Region.X_Min, Pos);
         Pos := Pos + 2;
         Get_Integer (S, Target_Region.X_Max, Pos);
         Pos := Pos + 4;
         Get_Integer (S, Target_Region.Y_Min, Pos);
         Pos := Pos + 2;
         Get_Integer (S, Target_Region.Y_Max, Pos);
      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Accelerate (V : Velocity) return Velocity is
   --  modifies V according to drag and gravity
      ((
       Dx => Integer'Max (0, V.Dx - 1),
       Dy => V.Dy - 1
      ));

   function Move (P : Position; V : Velocity) return Position is
   --  moves P according to V
      ((
       X => P.X + V.Dx,
       Y => P.Y + V.Dy
      ));

   function Last_Possible (Initial : Velocity) return Position is
   --  returns the last position obtained from the trajectory
   --  determined by the given initial velocity that is no deeper and
   --  no further away than the target region

      Previous : Position;
      V        : Velocity := Initial;
      P        : Position := ((0, 0));

   begin

      while P.X <= Target_Region.X_Max and then P.Y >= Target_Region.Y_Min loop
         Previous := P;
         P := Move (P, V);
         V := Accelerate (V);
      end loop;

      return Previous;

   end Last_Possible;

   --  PART 1
   Best_Launch_Value : Integer;
   Best_Height       : Integer;

   --  PART 2
   Num_Solutions : Natural := 0;

begin

   if Doing_Example then

      --  a bunch of tests

      Target_Region := ((
                        X_Min => 20,
                        X_Max => 30,
                        Y_Min => -10,
                        Y_Max => -5
                       ));
      Text_IO.Put_Line ("(7,2)? " & Last_Possible ((7, 2))'Image);
      Text_IO.Put_Line ("(6,3)? " & Last_Possible ((6, 3))'Image);
      Text_IO.Put_Line ("(6,9)? " & Last_Possible ((6, 9))'Image);
      Text_IO.Put_Line ("(9,0)? " & Last_Possible ((9, 0))'Image);
      Text_IO.Put_Line ("(17,-4)? " & Last_Possible ((17, -4))'Image);

   else

      Read_Input;

   end if;

   --  PART 1
   --  on account of symmetry, the best possible height _must_ occur
   --  when the first step after the probe returns to the submarine's level
   --  takes the probe the lowest point in the target region;
   --  since x and y are independent, many values of x will satisfy this!
   --  the height is simply the triangular number corresponding to that dy

   Best_Launch_Value := abs (Target_Region.Y_Min) - 1;
   Best_Height := Best_Launch_Value * (Best_Launch_Value + 1) / 2;

   Text_IO.Put_Line ("best height?" & Best_Height'Image);

   --  PART 2
   --  we sort-of brute-force it:
   --  negative x values won't work, but negative y-values might, and
   --  large x values also might, so check within the range where
   --  the probe could enter the target region on its first step

   for Dx in 0 .. Integer'Max (Target_Region.X_Max, abs (Target_Region.Y_Min))
   loop

      for Dy in Integer'Min (-Target_Region.X_Max, Target_Region.Y_Min) ..
         Integer'Max (Target_Region.X_Max, abs (Target_Region.Y_Min))
      loop

         if In_Target_Region (Last_Possible ((Dx, Dy))) then
            Num_Solutions := Num_Solutions + 1;
         end if;

      end loop;

   end loop;

   Text_IO.Put_Line (Num_Solutions'Image & " initial velocities work");

end Day17;
