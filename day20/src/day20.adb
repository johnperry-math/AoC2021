-- Advent of Code 2021
--
-- John Perry
--
-- Day 20: Trench Map
--
-- part 1: iterate an "image enhancement Remapping" over raw data two times,
--    report the number of lit pixels
--
-- part 2: iterate fifty times, report the number of lit pixels

with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;

procedure Day20 is

   package Text_IO renames Ada.Text_IO;
   --  package Integer_IO is new Ada.Text_IO.Integer_IO (Num => Integer);

   Doing_Example : constant Boolean := False;

   -- SECTION
   -- global types and variables

   type Response is ( Dark, Light );

   Remapping : array ( 0 .. 511 ) of Response;

   type Position is record
      Row, Col: Integer;
   end record;

   function Position_Hash ( P : Position ) return Ada.Containers.Hash_Type is
      ( Ada.Containers.Hash_Type( abs( P.Row ) * 100 + abs( P.Col ) ) );

   package Pixel_Sets is new Ada.Containers.Hashed_Sets
      ( Element_Type => Position,
        Hash                => Position_Hash,
        Equivalent_Elements => "="
       );

   Pixels : Pixel_Sets.Set;

   Row_Min, Col_Min : Integer := 1;
   Row_Max, Col_Max : Integer := ( if Doing_Example then 5 else 100 );

   function Is_Lit ( Is_Even_Iter : Boolean; Row, Col : Integer )
                    return Boolean
   is
   -- to account for the "twist" in this problem we check
   -- whether we are doing the example, whose Remapping differs from the puzzle
   -- essentially, the puzzle responds to 0 and 511 in the OPPOSITE manner
   -- as the example, so that the infinite region beyond raw data is always
   -- alternating between lit and unlit
      (
       if Remapping ( 0 ) = Dark or else not Is_Even_Iter
       then Pixels.Contains ( ( Row, Col ) )
       else Row < Row_Min
       or else Row > Row_Max
       or else Col < Col_Min
       or else Col > Col_Max
       or else Pixels.Contains ( ( Row, Col ) )
      );

   -- SECTION
   -- I/O

   procedure Read_Input is
   -- reads the input string and extract the target region dimensions

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      -- get Remapping
      declare
         S : constant String := Text_IO.Get_Line ( Input_File );
      begin
         for I in Remapping'Range loop
            Remapping( I ) := ( if S( I + 1 ) = '.' then Dark else Light );
         end loop;
      end;

      -- skip blank line
      Text_IO.Skip_Line ( Input_File );

      -- get raw image data
      for Row in Row_Min .. Row_Max loop

         declare
            S : constant String := Text_IO.Get_Line (Input_File);
         begin

            for Col in Col_Min .. Col_Max loop
               if S ( Col ) = '#' then
                  Pixels.Include ( ( Row, Col ) );
               end if;
            end loop;

         end;
      end loop;

      Text_IO.Close (Input_File);

   end Read_Input;

   pragma Warnings ( Off, "procedure ""Put_Image"" is not referenced" );
   procedure Put_Image is
   -- displays image; useful for debugging
   begin
      for Row in Row_Min .. Row_Max loop
         for Col in Col_Min .. Col_Max loop
            Text_IO.Put ( ( if Is_Lit ( False, Row, Col ) then '#' else '.' ) );
         end loop;
         Text_IO.New_Line;
      end loop;
   end Put_Image;
   pragma Warnings ( On, "procedure ""Put_Image"" is not referenced" );

   -- SECTION
   -- Parts 1 and 2

   procedure Apply_Algorithm ( Is_Even_Iter : Boolean ) is
   -- applies the Remapping rather thoughtlessly,
   -- relying on Is_Lit for the hard thinking

      New_Pixels : Pixel_Sets.Set;
      New_Row_Min : Integer := Row_Min;
      New_Row_Max : Integer := Row_Max;
      New_Col_Min : Integer := Col_Min;
      New_Col_Max : Integer := Col_Max;

   begin

      for Row in Row_Min - 1 .. Row_Max + 1 loop
         for Col in Col_Min - 1 .. Col_Max + 1 loop

            declare
               Key : constant Integer
                  := (
                      ( if Is_Lit ( Is_Even_Iter, Row - 1, Col - 1 ) then 256 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row - 1, Col ) then 128 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row - 1, Col + 1 ) then 64 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row, Col - 1 ) then 32 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row, Col ) then 16 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row, Col + 1 ) then 8 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row + 1, Col - 1 ) then 4 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row + 1, Col ) then 2 else 0 )
                      + ( if Is_Lit ( Is_Even_Iter, Row + 1, Col + 1 ) then 1 else 0 )
                     );
            begin

               if Remapping ( Key ) = Light then
                  New_Pixels.Include ( ( Row, Col ) );
                  New_Row_Min := Integer'Min ( New_Row_Min, Row );
                  New_Row_Max := Integer'Max ( New_Row_Max, Row );
                  New_Col_Min := Integer'Min ( New_Col_Min, Col );
                  New_Col_Max := Integer'Max ( New_Col_Max, Col );
               end if;

            end;

         end loop;
      end loop;

      Pixels := New_Pixels;
      Row_Min := New_Row_Min;
      Row_Max := New_Row_Max;
      Col_Min := New_Col_Min;
      Col_Max := New_Col_Max;

   end Apply_Algorithm;

begin

   Read_Input;

   -- Part 1
   Apply_Algorithm ( False );
   Apply_Algorithm ( True );

   Text_IO.Put_Line ( "after two iterations"
                      & Pixels.Length'Image & " pixels are lit");

   for I in 3 .. 50 loop
      Apply_Algorithm ( I mod 2 = 0 );
      Text_IO.Put_Line ( "after" & I'Image & " iterations"
                         & Pixels.Length'Image & " pixels are lit");
   end loop;


end Day20;
