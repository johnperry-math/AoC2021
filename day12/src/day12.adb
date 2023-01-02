--  Advent of Code 2021
--
--  John Perry
--
--  Day 12: Passage Pathing
--
--  part 1: determine the number of paths through a graph
--     without passing through a small chamber more than once
--
--  part 2: repeat, but allowed to pass through ONE small chamber twice;
--     this takes nearly a minute,
--     and could be sped up by introducing a type that ,
--     which however I am loath to do at the moment

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;

with Components.Queue;

procedure Day12 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   type Part_Number is (First, Second);

   --  SECTION
   --  global types and variables

   type Vertex is new String (1 .. 2);

   Start_Vertex : constant Vertex := "00";
   End_Vertex   : constant Vertex := "99";

   package Vertex_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Vertex
      );

   function Vertex_Hash (V : Vertex) return Ada.Containers.Hash_Type is
      Result : Natural := 0;
   begin
      for Ch of V loop
         Result := Result * 128 + Character'Pos (Ch) - Character'Pos (' ');
      end loop;
      return Ada.Containers.Hash_Type (Result);
   end Vertex_Hash;

   package Vertex_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => Vertex,
       Hash                => Vertex_Hash,
       Equivalent_Elements => "="
      );

   use type Vertex_Sets.Set;

   Vertices : Vertex_Sets.Set;

   package Connection_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type => Vertex,
       Hash     => Vertex_Hash,
       Element_Type => Vertex_Sets.Set,
       Equivalent_Keys => "="
      );

   Connections : Connection_Maps.Map;

   package Vertex_Vector_Queues is new Components.Queue
      (Element_Type => Vertex_Vectors.Vector
      );

   overriding function "<" (Left, Right : Vertex) return Boolean is
      (Left (1) < Right (1) or else
       (Left (1) = Right (1) and then Left (2) < Right (2)));

   function "<" (Left, Right : Vertex_Vectors.Vector) return Boolean is
      Result : Boolean := False;
      use type Ada.Containers.Count_Type;
   begin
      if Left.Length < Right.Length then
         Result := True;
      elsif Left.Length > Right.Length then
         Result := False;
      else
         for I in 1 .. Positive (Left.Length) loop
            if Left (I) > Right (I) then
               exit;
            elsif Left (I) < Right (I) then
               Result := True;
               exit;
            end if;
         end loop;
      end if;
      return Result;
   end "<";

   use type Ada.Containers.Count_Type;

   function "=" (Left, Right : Vertex_Vectors.Vector) return Boolean is
      (Left.Length = Right.Length and then
          (for all I in 1 .. Positive (Left.Length) => Left (I) = Right (I))
      );

   package Vertex_Vector_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type        => Vertex_Vectors.Vector
      );

   --  SECTION
   --  I/O

   Unexpected_Vertex : exception;

   function Get_Vertex (S : String; Start, Stop : Positive) return Vertex is
         ((
          if Stop - Start > 1 then
             (if S (Start .. Stop) = "start" then
                    Start_Vertex
              elsif S (Start .. Stop) = "end" then
                    End_Vertex
              else
                 raise Unexpected_Vertex with S (Start .. Stop)
             )
          elsif Stop = Start then
             Vertex '(S (Start), ' ')
          else
             Vertex (S (Start .. Stop))
         ));

   procedure Read_Input is
   --

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example3.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      while not Text_IO.End_Of_File (Input_File) loop

         declare
            Input_String  : constant String := Text_IO.Get_Line (Input_File);
            Start, Stop   : Positive := 1;
            First, Second : Vertex := "  ";
         begin
            --  get first vertex
            while Input_String (Stop) /= '-' loop
               Stop := Stop + 1;
            end loop;
            First := Get_Vertex (Input_String, Start, Stop - 1);
            if not Vertices.Contains (First) then
               Vertices.Include (First);
            end if;
            --  get second vertex
            Start := Stop + 1;
            while Stop <= Input_String'Length loop
               Stop := Stop + 1;
            end loop;
            Second := Get_Vertex (Input_String, Start, Stop - 1);
            if not Vertices.Contains (Second) then
               Vertices.Include (Second);
            end if;
            --  insert connections
            if Connections.Contains (First) then
               Connections (First).Include (Second);
            else
               declare
                  New_Set : Vertex_Sets.Set;
               begin
                  New_Set.Include (Second);
                  Connections.Include (First, New_Set);
               end;
            end if;
            if Connections.Contains (Second) then
               Connections (Second).Include (First);
            else
               declare
                  New_Set : Vertex_Sets.Set;
               begin
                  New_Set.Include (First);
                  Connections.Include (Second, New_Set);
               end;
            end if;
         end;

      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   --  SECTION
   --  PARTS 1 AND 2

   function Is_Large (V : Vertex) return Boolean is
      (V (1) in 'A' .. 'Z' and then (V (2) in 'A' .. 'Z' or else V (2) = ' '));

   function Has_Repeated_Small (Path : Vertex_Vectors.Vector) return Boolean
   is
   begin
      for I in Path.First_Index .. Path.Last_Index loop
         if not Is_Large (Path (I)) then
            for J in I + 1 .. Path.Last_Index loop
               if Path (J) = Path (I) then
                  return True;
               end if;
            end loop;
         end if;
      end loop;
      return False;
   end Has_Repeated_Small;

   function Number_Of_Paths (Part : Part_Number := First) return Natural is
      Result : Natural := 0;
      To_Do  : Vertex_Vector_Queues.Queue;
      Done   : Vertex_Vector_Sets.Set;
   begin
      --  start from, well, start!
      declare
         New_Path : Vertex_Vectors.Vector;
      begin
         New_Path.Append (Start_Vertex);
         To_Do.Push (New_Path);
      end;
      --  bfs
      while not To_Do.Is_Empty loop
         declare
            Path : Vertex_Vectors.Vector;
         begin
            To_Do.Pop (Path);
            --  Text_IO.Put_Line ("popped " & Path'Image); Text_IO.New_Line;
            for V of Connections (Path.Last_Element) loop
               --  Text_IO.Put_Line ("   connected to " & V'Image);
               if V = End_Vertex then
                  --  Text_IO.Put_Line ("      " & Path'Image
                  --                    & " " & End_Vertex'Image);
                  Result := Result + 1;
               elsif Is_Large (V) or else
                  (Part = First and then not Path.Contains (V)) or else
                  (Part = Second and then
                     V /= Start_Vertex and then
                        (not Path.Contains (V) or else
                                 not Has_Repeated_Small (Path)))
               then
                  declare
                     New_Path : Vertex_Vectors.Vector
                        := Vertex_Vectors.Copy (Path);
                  begin
                     New_Path.Append (V);
                     if not Done.Contains (New_Path) then
                        --  Text_IO.Put_Line ("      adding to path...");
                        Done.Include (New_Path);
                        To_Do.Push (New_Path);
                        --  Text_IO.Put_Line ("      ...added");
                     end if;
                  end;
               --  elsif not Is_Large(V) and then Part = Second then
               --     Text_IO.Put_Line ("      rejected because"
               --                       & Boolean'Image(V /= Start_Vertex)
               --                       & Has_Repeated_Small (Path)'image
               --                      );
               end if;
            end loop;
         end;
      end loop;
      return Result;
   end Number_Of_Paths;

begin

   Read_Input;
   --  Text_IO.Put ("vertices " & Vertices'Image); Text_IO.New_Line;
   --  Text_IO.Put ("connections " & Connections'Image); Text_IO.New_Line;

   Text_IO.Put_Line ("number of paths" & Number_Of_Paths'Image);
   Text_IO.Put_Line ("number of paths with some repetition"
                     & Number_Of_Paths (Second)'Image);

end Day12;
