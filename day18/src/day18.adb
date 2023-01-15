--  Advent of Code 2021
--
--  John Perry
--
--  Day 18: Snailfish
--
--  part 1: find the magnitude of the sum of all the snailfish numbers
--
--  part 2: find the largest magnitude of any pair of snailfish numbers

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Multiway_Trees;

use type Ada.Containers.Count_Type;

procedure Day18 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   --  SUBSECTION
   --  the snailfish numbers

   type Number_Kind is (Pair, Immediate);

   type Snail_Num (Kind : Number_Kind) is record
      case Kind is
         when Immediate => Value : Natural := 0;
         when Pair => null;
      end case;
   end record;

   --  SUBSECTION
   --  snailfish number containers

   package Snail_Num_Trees is new Ada.Containers.Indefinite_Multiway_Trees
      (Element_Type => Snail_Num
      );

   use type Snail_Num_Trees.Cursor;

   package Snail_Num_Vectors is new Ada.Containers.Indefinite_Vectors
      (Index_Type   => Positive,
       Element_Type => Snail_Num_Trees.Tree,
       "="          => Snail_Num_Trees."="
      );

   All_Trees : Snail_Num_Vectors.Vector;

   --  SUBSECTION
   --  snailfish number output

   pragma Warnings
      (Off, "procedure ""Put_Snail_Num"" is not referenced");
   --  useful for debugging

   procedure Put_Snail_Num
      (T : Snail_Num_Trees.Tree; C : Snail_Num_Trees.Cursor)
   is
   --  prints a snailfish number to standard output
   --  in the same format as shown in the puzzle
   --  recursive, naturally

      E : constant Snail_Num := Snail_Num_Trees.Element (C);

   begin

      if E.Kind = Immediate then
         Natural_IO.Put (E.Value, 0);

      else -- E.Kind = Pair
         Text_IO.Put ('[');
         Put_Snail_Num (T, Snail_Num_Trees.First_Child (C));
         Text_IO.Put (',');
         Put_Snail_Num (T, Snail_Num_Trees.Last_Child (C));
         Text_IO.Put (']');

      end if;

   end Put_Snail_Num;

   pragma Warnings
      (On, "procedure ""Put_Snail_Num"" is not referenced");

   --  SUBSECTION
   --  snailfish number reduction

   --  SUBSUBSECTION
   --  explosions

   procedure Propagate_Left
      (T : in out Snail_Num_Trees.Tree;
       N : Snail_Num_Trees.Cursor;
       V : Natural)
   is
   --  use for the left sides of an explosion;
   --  this propagates V up through T until
   --  the parent's right child, rather than the left, is the previous node,
   --  then moves to the left node,
   --  then down through right nodes as far as it can

      P : Snail_Num_Trees.Cursor := N;
      C : Snail_Num_Trees.Cursor := Snail_Num_Trees.Parent (N);

   begin

      --  move up until we can move left
      while C /= T.Root and then P = Snail_Num_Trees.First_Child (C) loop
         P := C;
         C := Snail_Num_Trees.Parent (C);
      end loop;

      --  if we're at root, then this number does not propagate
      if C /= T.Root then

         --  move left once
         C := Snail_Num_Trees.First_Child (C);

         --  now move right
         while Snail_Num_Trees.Element (C).Kind = Pair loop
            C := Snail_Num_Trees.Last_Child (C);
         end loop;

         --  add the value
         Snail_Num_Trees.Reference (T, C).Value := @ + V;

      end if;

   end Propagate_Left;

   procedure Propagate_Right
      (T : in out Snail_Num_Trees.Tree;
       N : Snail_Num_Trees.Cursor;
       V : Natural)
   is
   --  use for the right sides of an explosion;
   --  this propagates V up through T until
   --  the parent's left child, rather than the right, is the previous node,
   --  then moves to the right node,
   --  then down through left nodes as far as it can

      P : Snail_Num_Trees.Cursor := N;
      C : Snail_Num_Trees.Cursor := Snail_Num_Trees.Parent (N);

   begin

      --  move up until we can move right
      while C /= T.Root and then P = Snail_Num_Trees.Last_Child (C) loop
         P := C;
         C := Snail_Num_Trees.Parent (C);
      end loop;

      --  if we're at root, then this number does not propagate
      if C /= T.Root then

         --  move right once
         C := Snail_Num_Trees.Last_Child (C);

         --  now move left
         while Snail_Num_Trees.Element (C).Kind = Pair loop
            C := Snail_Num_Trees.First_Child (C);
         end loop;

         --  add the value
         Snail_Num_Trees.Reference (T, C).Value := @ + V;

      end if;

   end Propagate_Right;

   procedure Explode
      (T : in out Snail_Num_Trees.Tree;
       N : in out Snail_Num_Trees.Cursor
      )
   is
   --  takes the left and right values of the pair number at N,
   --  adds them to the first immediate number on the corresponding side,
   --  then replaces N by the immediate number of 0
   --
   --  notice the assumption that N is a pair number
   --  whose entries are immediate numbers;
   --  we do not check this, and the procedure will fail if you are not careful
   --
   --  this uses Propagate_Left and Propagate_Right to add left and right

      C : Snail_Num_Trees.Cursor;

   begin

      --  add left immediate to first immediate at left, if any
      Propagate_Left
         (T, N, Snail_Num_Trees.First_Child_Element (N).Value);

      --  add right immediate to first immediate at right, if any
      Propagate_Right
         (T, N, Snail_Num_Trees.Last_Child_Element (N).Value);

      --  delete the children
      C := Snail_Num_Trees.First_Child (N);
      Snail_Num_Trees.Delete_Leaf (T, C);
      C := Snail_Num_Trees.Last_Child (N);
      Snail_Num_Trees.Delete_Leaf (T, C);

      --  replace the pair element by an immediate element
      T.Replace_Element
         (N, (Kind => Immediate, Value => 0));

   end Explode;

   --  SUBSUBSECTION
   --  splits

   procedure Split
      (T : in out Snail_Num_Trees.Tree; N : in out Snail_Num_Trees.Cursor)
   is
   --  replaces the immediate number at N by a pair number
   --  with the immediate number value of half of N on the left, rounded down,
   --  and the immediate value of half of N on the right, rounded up if need be
   --
   --  notice the assumption that N is an immediate number;
   --  we do not check this, and the procedure will fail if you are not careful

      --  the immediate number at N
      P : constant Snail_Num := Snail_Num_Trees.Element (N);

      --  new entries for immediate numbers
      New_Left_Value  : constant Natural := P.Value / 2;
      New_Right_Value : constant Natural
         := (if P.Value mod 2 = 0
             then P.Value / 2
             else P.Value / 2 + 1);

      --  new immediate numbers
      New_Left  : constant Snail_Num := (Kind  => Immediate,
                                                Value => New_Left_Value);
      New_Right : constant Snail_Num := (Kind  => Immediate,
                                         Value => New_Right_Value);

   begin

      T.Replace_Element (N, (Kind => Pair));
      T.Prepend_Child (N, New_Left);
      T.Append_Child (N, New_Right);

   end Split;

   --  SUBSUBSECTION
   --  reductions

   function First_Explosion (T : Snail_Num_Trees.Tree)
                             return Snail_Num_Trees.Cursor
   is
   --  returns the first explosion found in T
   begin

      for C in T.Iterate loop

         if Snail_Num_Trees.Depth (C) > 5
            and then Snail_Num_Trees.Element (C).Kind = Pair
         then
            return C;
         end if;

      end loop;

      return Snail_Num_Trees.No_Element;

   end First_Explosion;

   function First_Split (T : Snail_Num_Trees.Tree)
                         return Snail_Num_Trees.Cursor
   is
   --  returns the first split found in T
   begin

      for C in T.Iterate loop

         if Snail_Num_Trees.Element (C).Kind = Immediate
            and then Snail_Num_Trees.Element (C).Value > 9
         then
            return C;
         end if;

      end loop;

      return Snail_Num_Trees.No_Element;

   end First_Split;

   procedure Reduce (T : in out Snail_Num_Trees.Tree)
   is
   --  reduces T according to the directions given in the puzzle:
   --  reduce all explosions, and once there are no explosions left,
   --  reduce all splits,
   --  checking immediately for explosions after each split

      Changed : Boolean := True;
      C       : Snail_Num_Trees.Cursor;

   begin

      while Changed loop

         --  innocent until proven guilty
         Changed := False;

         C := First_Explosion (T);

         if C /= Snail_Num_Trees.No_Element then
            --  always reduce explosions
            Changed := True;
            Explode (T, C);

         else
            --  reduce splits only if there are no explosiosn
            C := First_Split (T);
            if C /= Snail_Num_Trees.No_Element then
               Changed := True;
               Split (T, C);
            end if;

         end if;

      end loop;

   end Reduce;

   function "+" (Left, Right : Snail_Num_Trees.Tree)
                 return Snail_Num_Trees.Tree
   is
   --  returns the reduced sum of Left and Right

      Result     : Snail_Num_Trees.Tree;
      C          : Snail_Num_Trees.Cursor;
      New_Number : constant Snail_Num := (Kind => Pair);

   begin

      --  Ada doesn't allow the root element to hold data
      Result.Prepend_Child (Result.Root, New_Number);
      C := Snail_Num_Trees.First_Child (Result.Root);

      --  at least it allows us to copy the trees! :-D
      Result.Copy_Subtree (C,
                           Snail_Num_Trees.No_Element,
                           Snail_Num_Trees.First_Child (Left.Root)
                          );
      Result.Copy_Subtree (C,
                           Snail_Num_Trees.No_Element,
                           Snail_Num_Trees.First_Child (Right.Root)
                          );

      Reduce (Result);

      return Result;

   end "+";

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

   procedure Read_Snail_Num_Number
      (S   : String;
       Pos : in out Positive;
       T   : in out Snail_Num_Trees.Tree;
       C   : Snail_Num_Trees.Cursor)
   is
   --  expects Snail_Num_Trees.Element (C) to be a pair waiting for values
   --  and Pos to be first position _after_ the opening bracket for this pair
   begin

      if S (Pos) = '[' then
         Pos := Pos + 1; -- [
         Snail_Num_Trees.Replace_Element (T, C, (Kind => Pair));
         Snail_Num_Trees.Prepend_Child (T, C, (Immediate, 0));
         Snail_Num_Trees.Append_Child (T, C, (Immediate, 0));
         Read_Snail_Num_Number (S, Pos, T, Snail_Num_Trees.First_Child (C));
         Pos := Pos + 1; -- ,
         Read_Snail_Num_Number (S, Pos, T, Snail_Num_Trees.Last_Child (C));
         Pos := Pos + 1; -- ]

      else
         declare
            Value : Natural;
         begin
            Get_Integer (S, Value, Pos);
            Snail_Num_Trees.Replace_Element
               (T, C, (Kind => Immediate, Value => Value));
         end;

      end if;

   end Read_Snail_Num_Number;

   procedure Read_Input is
   --  reads the input string and extract the target region dimensions

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

      Pos : Positive := 1;

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            T : Snail_Num_Trees.Tree;
            C : Snail_Num_Trees.Cursor;
            S : constant String := Text_IO.Get_Line (Input_File);
         begin
            Snail_Num_Trees.Prepend_Child (T, T.Root, (Kind => Pair));
            C := Snail_Num_Trees.First_Child (T.Root);
            Pos := 1;
            Read_Snail_Num_Number (S, Pos, T, C);
            All_Trees.Append (Snail_Num_Trees.Copy (T));
         end;

      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Magnitude (T : Snail_Num_Trees.Tree; C : Snail_Num_Trees.Cursor)
                       return Natural
   is
   --  per puzzle:
   --  when a pair, the sum of thrice the first's magnitude
   --  and twice the second's;
   --  when immediate, simply the value

      (case Snail_Num_Trees.Element (C).Kind is

          when Immediate => Snail_Num_Trees.Element (C).Value,

          when Pair      =>
             3 * Magnitude (T, Snail_Num_Trees.First_Child (C)) +
             2 * Magnitude (T, Snail_Num_Trees.Last_Child (C))
      );

begin

   Read_Input;

   --  part 1
   declare
      Sum : Snail_Num_Trees.Tree := Snail_Num_Trees.Copy
         (All_Trees.Constant_Reference (All_Trees.First));
   begin

      for I in All_Trees.First_Index + 1 .. All_Trees.Last_Index loop
         Sum := Sum + All_Trees (I);
      end loop;

      Text_IO.Put_Line
         (Magnitude
             (Sum, Snail_Num_Trees.First_Child (Sum.Root)
             )'Image
         );

   end;

   --  part 2
   --  I thought briefly about finding a clever way to do this,
   --  but there are only 100 numbers,
   --  which means only Choose(100,2) = 50*99 = 4950 pairs,
   --  which is... not a lot!
   --  so brute force it is
   declare
      Largest_Magnitude : Natural := 0;
   begin

      for I in All_Trees.First_Index .. All_Trees.Last_Index - 1 loop
         for J in I + 1 .. All_Trees.Last_Index loop

            declare
               Sum : constant Snail_Num_Trees.Tree
                  := All_Trees (I) + All_Trees (J);
            begin

               Largest_Magnitude := Natural'Max
                  (Largest_Magnitude,
                   Magnitude (Sum, Snail_Num_Trees.First_Child (Sum.Root))
                  );

            end;

         end loop;
      end loop;

      Text_IO.Put_Line ("the largest magnitude is" & Largest_Magnitude'Image);

   end;

end Day18;
