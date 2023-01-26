-- Advent of Code 2021
--
-- John Perry
--
-- Day 19: Beacon Scanner
--
-- part 1: orient the scanners
--
-- part 2: find the largest distance between two scanners

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

use type Ada.Containers.Count_Type;

procedure Day19 is

   package Text_IO renames Ada.Text_IO;
   package Integer_IO is new Ada.Text_IO.Integer_IO (Num => Integer);

   Doing_Example : constant Boolean := False;

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- Position-related

   type Position is record
      X, Y, Z: Integer;
   end record;

   package Position_Vectors is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Position
      );

   function Position_Hash (P : Position) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type(abs(P.X * 71 + P.Y * 83 + P.Z + 101))
      );

   package Position_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => Position,
       Hash                => Position_Hash,
       Equivalent_Elements => "="
      );

   -- SUBSECTION
   -- scanners

   type Scanner_Data is record
      Correlated       : Boolean;  -- whether i've yet been correlated to 0
      Relative_To_Zero : Position; -- my location relative to 0
      Beacons_Relative : Position_Vectors.Vector; -- relative to me
      Beacons_Absolute : Position_Vectors.Vector; -- relative to 0
   end record;

   package Scanner_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural,
       Element_Type => Scanner_Data
      );

   Scanners: Scanner_Vectors.Vector;

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

      Text_IO.Skip_Line(Input_File); --  skip the "scanner 0" line

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Pos_Rel_Me, Pos_Rel_0 : Position_Vectors.Vector;
         begin
            while not Text_IO.End_Of_File (Input_File) loop
               declare
                  S : constant String := Text_IO.Get_Line (Input_File);
               begin
                  if S'Length > 0 and then S (2) /= '-' then
                     declare
                        X, Y, Z : Integer;
                        Pos     : Positive := 1;
                     begin
                        Get_Integer (S, X, Pos);
                        Pos := Pos + 1;
                        Get_Integer (S, Y, Pos);
                        Pos := Pos + 1;
                        Get_Integer (S, Z, Pos);
                        Pos_Rel_Me.Append (Position'(X, Y, Z));
                     end;
                  end if;
                  if S'Length = 0 or else Text_IO.End_Of_File (Input_File) then
                     Scanners.Append
                        (Scanner_Data'(Correlated      => False,
                                       Relative_To_Zero => (0, 0, 0),
                                       Beacons_Relative => Pos_Rel_Me,
                                       Beacons_Absolute => Pos_Rel_0
                                      ));
                     Pos_Rel_Me.Clear;
                     if not Text_IO.End_Of_File (Input_File) then
                        --  skip the "scanner 0" line
                        Text_IO.Skip_Line (Input_File);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   pragma Warnings (Off, "procedure ""Put_Position"" is not referenced");
   procedure Put_Position (P : Position) is
   -- useful for debugging
   begin
      Text_IO.Put ("(");
      Integer_IO.Put (P.X, 0);
      Text_IO.Put (",");
      Integer_IO.Put (P.Y, 0);
      Text_IO.Put (",");
      Integer_IO.Put (P.Z, 0);
      Text_IO.Put (")");
   end Put_Position;
   pragma Warnings (On, "procedure ""Put_Position"" is not referenced");

   pragma Warnings (Off, "procedure ""Put_Scanner"" is not referenced");
   procedure Put_Scanner (I : Natural) is
   -- useful for debugging
   begin
      Text_IO.Put ("--- scanner ");
      Integer_IO.Put (I, 0);
      Text_IO.Put_Line (" ---");
      for Beacon of Scanners (I).Beacons_Relative loop
         Integer_IO.Put (Beacon.X, 0);
         Text_IO.Put (",");
         Integer_IO.Put (Beacon.Y, 0);
         Text_IO.Put (",");
         Integer_IO.Put (Beacon.Z, 0);
         Text_IO.New_Line;
      end loop;
      Text_IO.Put_Line
         ("   with " & Scanners (I).Correlated'Image & " correlation to 0");
      if Scanners (I).Correlated then
         for Beacon of Scanners (I).Beacons_Absolute loop
            Integer_IO.Put (Beacon.X, 0);
            Text_IO.Put (",");
            Integer_IO.Put (Beacon.Y, 0);
            Text_IO.Put (",");
            Integer_IO.Put (Beacon.Z, 0);
            Text_IO.New_Line;
         end loop;
      end if;
   end Put_Scanner;
   pragma Warnings (On, "procedure ""Put_Scanner"" is not referenced");

   -- SECTION
   -- PART 1

   function Natural_Hash (X : Natural) return Ada.Containers.Hash_Type is
      (Ada.Containers.Hash_Type (X));

   package Scanner_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type => Natural,
       Equivalent_Elements => "=",
       Hash => Natural_Hash
      );

   -- SUBSECTION
   -- rotation matrices (if used)

   type Rotation_Matrix is array (1 .. 3, 1 .. 3) of Integer;

   Matrices : constant array (1 .. 24) of Rotation_Matrix
      := (
          ( ( 1,  0,  0), ( 0,  1,  0), ( 0,  0,  1) ),
          ( ( 1,  0,  0), ( 0,  0, -1), ( 0,  1,  0) ),
          ( ( 1,  0,  0), ( 0, -1,  0), ( 0,  0, -1) ),
          ( ( 1,  0,  0), ( 0,  0,  1), ( 0, -1,  0) ),
          ( (-1,  0,  0), ( 0, -1,  0), ( 0,  0,  1) ),
          ( (-1,  0,  0), ( 0,  0, -1), ( 0, -1,  0) ),
          ( (-1,  0,  0), ( 0,  1,  0), ( 0,  0, -1) ),
          ( (-1,  0,  0), ( 0,  0,  1), ( 0,  1,  0) ),

          ( ( 0, -1,  0), ( 1,  0,  0), ( 0,  0,  1) ),
          ( ( 0,  0,  1), ( 1,  0,  0), ( 0,  1,  0) ),
          ( ( 0,  1,  0), ( 1,  0,  0), ( 0,  0, -1) ),
          ( ( 0,  0, -1), ( 1,  0,  0), ( 0, -1,  0) ),
          ( ( 0,  1,  0), (-1,  0,  0), ( 0,  0,  1) ),
          ( ( 0,  0,  1), (-1,  0,  0), ( 0, -1,  0) ),
          ( ( 0, -1,  0), (-1,  0,  0), ( 0,  0, -1) ),
          ( ( 0,  0, -1), (-1,  0,  0), ( 0,  1,  0) ),

          ( ( 0,  0, -1), ( 0,  1,  0), ( 1,  0,  0) ),
          ( ( 0,  1,  0), ( 0,  0,  1), ( 1,  0,  0) ),
          ( ( 0,  0,  1), ( 0, -1,  0), ( 1,  0,  0) ),
          ( ( 0, -1,  0), ( 0,  0, -1), ( 1,  0,  0) ),
          ( ( 0,  0, -1), ( 0, -1,  0), (-1,  0,  0) ),
          ( ( 0, -1,  0), ( 0,  0,  1), (-1,  0,  0) ),
          ( ( 0,  0,  1), ( 0,  1,  0), (-1,  0,  0) ),
          ( ( 0,  1,  0), ( 0,  0, -1), (-1,  0,  0) )
         );

   function "*" (M : Rotation_Matrix; P : Position) return Position is
      ((
       M (1, 1) * P.X + M (1, 2) * P.Y + M (1, 3) * P.Z,
       M (2, 1) * P.X + M (2, 2) * P.Y + M (2, 3) * P.Z,
       M (3, 1) * P.X + M (3, 2) * P.Y + M (3, 3) * P.Z
      ));

   procedure Reorient_By_Matrix (S : in out Scanner_Data; M : Rotation_Matrix)
   is
   -- assigns to S.Beacons_Absolute the re-orientation by M
   -- of elements of S.Beacons_Relative
   begin
      S.Beacons_Absolute.Clear;
      for P of S.Beacons_Relative loop
         S.Beacons_Absolute.Append (M * P);
      end loop;

   end Reorient_By_Matrix;

   -- SUBSECTION
   -- rotations, when rotation matrices are not used

   type Rotations is mod 4;

   function Rotate_X (P : Position; Times : Rotations) return Position is
      (case Times is
          when 0 => P,
          when 1 => (P.X, P.Z, -P.Y),
          when 2 => (P.X, -P.Y, -P.Z),
          when 3 => (P.X, -P.Z, P.Y)
      );

   function Rotate_Y (P : Position; Times: Rotations) return Position is
      (case Times is
          when 0 => P,
          when 1 => (P.Z, P.Y, -P.X),
          when 2 => (-P.X, P.Y, -P.Z),
          when 3 => (-P.Z, P.Y, P.X)
       );

   function Rotate_Z (P : Position; Times : Rotations) return Position is
      (case Times is
          when 0 => P,
          when 1 => (P.Y, -P.X, P.Z),
          when 2 => (-P.X, -P.Y, P.Z),
          when 3 => (-P.Y, P.X, P.Z)
      );

   procedure Reorient (S : in out Scanner_Data; Xr, Yr, Zr : Rotations) is
   -- reorients S by appending to S.Beacons_Absolute the rotated values
   -- of S.Beacons_Relative according to Xr, Yr, Zr
   -- (the rotations on each axis)
   begin
      S.Beacons_Absolute.Clear;
      for P of S.Beacons_Relative loop
         declare
            New_Position : Position := P;
         begin
            New_Position := Rotate_X (New_Position, Xr);
            New_Position := Rotate_Y (New_Position, Yr);
            New_Position := Rotate_Z (New_Position, Zr);
            S.Beacons_Absolute.Append (New_Position);
         end;
      end loop;
   end Reorient;

   function Matches_After_Translation
      (First, Second : Scanner_Data; P, Q : Position)
       return Natural
   is
   -- returns the number of elements of Second.Beacons_Absolute
   -- that appear in First.Beacons_Absolute
      Dx : constant Integer := Q.X - P.X;
      Dy : constant Integer := Q.Y - P.Y;
      Dz : constant Integer := Q.Z - P.Z;
      Number_Matching : Natural := 0;
   begin
      for R of Second.Beacons_Absolute
      when First.Beacons_Absolute.Contains ((R.X - Dx, R.Y - Dy, R.Z - Dz))
      loop
         Number_Matching := Number_Matching + 1;
      end loop;
      return Number_Matching;
   end Matches_After_Translation;

   procedure Translate
      (S_Unknown : in out Scanner_Data;
       P         : Position;
       Q         : Position
      )
   is
   -- translates the elements of S_Unknown.Beacons_Absolute
   -- according to the distance P - Q
   -- in particular, if P is an element of S_Unknown.Beacons_Absolute
   -- and Q is a position relative to 0, then the elements of
   -- S_Unknown.Beacons_Absolute will be the correct values of
   -- S_Unknown.Beacons_Relative with respect to the 0 scanner,
   -- and while this does not update S_Unkown.Correlated, it does update
   -- S_Unknown.Relative_To_Zero, in case that is indeed the comparison
      Dx : constant Integer := P.X - Q.X;
      Dy : constant Integer := P.Y - Q.Y;
      Dz : constant Integer := P.Z - Q.Z;
      Old_Beacons : constant Position_Vectors.Vector
         := Position_Vectors.Copy (S_Unknown.Beacons_Absolute);
   begin
      S_Unknown.Beacons_Absolute.Clear;
      S_Unknown.Relative_To_Zero := (Dx, Dy, Dz);
      for B of Old_Beacons loop
         S_Unknown.Beacons_Absolute.Append
            (Position'(B.X - Dx, B.Y - Dy, B.Z - Dz));
      end loop;
   end Translate;

   pragma Warnings (Off, "function ""Correlate"" is not referenced");
   function Correlate
      (S_Known : Scanner_Data; S_Unknown : in out Scanner_Data)
       return Boolean
   is
   -- returns True if and only if S_Known and S_Unknown both spot 12 beacons,
   -- and if so it also updates S_Unknown so that
   -- it knows its positions relative to 0 (including S_Unknown.Correlated)
   -- this variant does not use rotation matrices
      Q, R : Position;
      Ref_Pos : constant Position := (1, 2, 3);
      Tried_Positions : Position_Sets.Set;
      Ref_Pos_Vec     : Scanner_Data;
      Attempts: Natural := 0;
   begin
      Ref_Pos_Vec.Beacons_Relative.Append (Ref_Pos);
      Outermost_Loop :
      for Xr in Rotations loop
         for Yr in Rotations loop
            for Zr in Rotations loop
               Reorient (Ref_Pos_Vec, Xr, Yr, Zr);
               if not Tried_Positions.Contains
                  (Ref_Pos_Vec.Beacons_Absolute.First_Element)
               then
                  Attempts := Attempts + 1;
                  Tried_Positions.Include
                     (Ref_Pos_Vec.Beacons_Absolute.First_Element);
                  Reorient (S_Unknown, Xr, Yr, Zr);
                  for P of S_Unknown.Beacons_Absolute loop
                     for I in S_Known.Beacons_Absolute.First_Index
                        .. S_Known.Beacons_Absolute.Last_Index
                     loop
                        Q := S_Known.Beacons_Absolute (I);
                        declare
                           Num_Matches : constant Natural
                              := Matches_After_Translation
                                 (S_Known, S_Unknown, Q, P);
                        begin
                           if Num_Matches >= 12 then
                              R := P;
                              S_Unknown.Correlated := True;
                              exit Outermost_Loop;
                           end if;
                        end;
                     end loop;
                  end loop;
               end if;
            end loop;
         end loop;
      end loop Outermost_Loop;
      if S_Unknown.Correlated then
         Translate (S_Unknown, R, Q);
      end if;
      --  Text_IO.Put_Line
      --     ("      returning after" & Attempts'Image & " attempts");
      return S_Unknown.Correlated;
   end Correlate;
   pragma Warnings (On, "function ""Correlate"" is not referenced");

   pragma Warnings (Off, "function ""Correlate_By_Matrix"" is not referenced");
   function Correlate_By_Matrix
      (S_Known : Scanner_Data; S_Unknown : in out Scanner_Data)
       return Boolean
   is
   -- returns True if and only if S_Known and S_Unknown both spot 12 beacons,
   -- and if so it also updates S_Unknown so that
   -- it knows its positions relative to 0 (including S_Unknown.Correlated)
   -- this variant uses rotation matrices
      Q, R : Position;
      Ref_Pos : constant Position := (1, 2, 3);
      Ref_Pos_Vec     : Scanner_Data;
      Attempts: Natural := 0;
   begin
      Ref_Pos_Vec.Beacons_Relative.Append (Ref_Pos);
      Outermost_Loop : for M of Matrices loop
         Attempts := Attempts + 1;
         Reorient_By_Matrix (S_Unknown, M);
         for P of S_Unknown.Beacons_Absolute loop
            for I in S_Known.Beacons_Absolute.First_Index
               .. S_Known.Beacons_Absolute.Last_Index
            loop
               Q := S_Known.Beacons_Absolute (I);
               declare
                  Num_Matches : constant Natural
                     := Matches_After_Translation (S_Known, S_Unknown, Q, P);
               begin
                  if Num_Matches >= 12 then
                     R := P;
                     S_Unknown.Correlated := True;
                     exit Outermost_Loop;
                  end if;
               end;
            end loop;
         end loop;
      end loop Outermost_Loop;
      if S_Unknown.Correlated then
         Translate (S_Unknown, R, Q);
      end if;
      --  Text_IO.Put_Line
      --     ("      returning after" & Attempts'Image & " attempts");
      return S_Unknown.Correlated;
   end Correlate_By_Matrix;
   pragma Warnings (On, "function ""Correlate_By_Matrix"" is not referenced");

   function Number_Of_Beacons return Natural is
   -- returns the number of unique beacons that all the scanners see
      To_Do, Just_Done, Newly_Done : Scanner_Sets.Set;
      Beacons                      : Position_Sets.Set;
   begin
      -- queue up all scanners after the first
      for I in Scanners.First_Index + 1 .. Scanners.Last_Index loop
         To_Do.Include (I);
      end loop;
      Just_Done.Include (0);
      -- keep going until they're all done
      while To_Do.Length > 0 loop
         for I of Just_Done when not To_Do.Contains (I) loop
            -- correlate with the ones just done any we can that are undone
            --  Text_IO.Put_Line ("looking for correlations to" & I'Image);
            for J of To_Do when Correlate (Scanners (I), Scanners (J)) loop
               --  Text_IO.Put_Line ("   trying" & J'Image);
               Newly_Done.Include (J);
               --  Text_IO.Put_Line ("      yes");
            end loop;
            To_Do := To_Do.Difference (Newly_Done);
         end loop;
         Just_Done := Scanner_Sets.Copy (Newly_Done);
         Newly_Done.Clear;
      end loop;
      for S of Scanners loop
         for P of S.Beacons_Absolute loop
            Beacons.Include(P);
         end loop;
      end loop;
      return Natural(Beacons.Length);
   end Number_Of_Beacons;

   -- PART 2

   function Max_Distance return Natural is
   -- returns the maximum distance between any two scanners
      Result : Natural := 0;
   begin
      for I in Scanners.First_Index .. Scanners.Last_Index - 1 loop
         for J in I + 1 .. Scanners.Last_Index loop
            declare
               I_Rel_Loc renames Scanners (I).Relative_To_Zero;
               J_Rel_Loc renames Scanners (J).Relative_To_Zero;
            begin
               Result := Natural'Max
                  (Result,
                   abs (I_Rel_Loc.X - J_Rel_Loc.X)
                   + abs (I_Rel_Loc.Y - J_Rel_Loc.Y)
                   + abs (I_Rel_Loc.Z - J_Rel_Loc.Z)
                  );
            end;
         end loop;
      end loop;
      return Result;
   end Max_Distance;

begin

   Read_Input;
   -- our algorithm needs Scanners(0) to know it correlates to itself :-D
   Scanners (0).Correlated := True;
   Scanners (0).Beacons_Absolute
      := Position_Vectors.Copy (Scanners (0).Beacons_Relative);

   Text_IO.Put_Line ("there are" & Number_Of_Beacons'Image & " beacons");
   Text_IO.Put_Line ("the maximum distance between scanners is"
                     & Max_Distance'Image);

end Day19;
