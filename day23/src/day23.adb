-- Advent of Code 2021
--
-- John Perry
--
-- Day 23: Amphipod
--
-- part 1: help the amphipods rearrange themselves into their rooms
--
-- part 2: whoops, there are more amphipods -- do the same

with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

procedure Day23 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Doing_Part_1 : Boolean := True;

   Visualize : Boolean := False;

   -- SECTION
   -- global types and variables

   -- SUBSECTION
   -- colors and what's associated with them

   type Color is ( Amber, Bronze, Copper, Desert );

   Energy : array ( Color ) of Positive
   -- the amount of energy it takes to move a step
      := ( Amber => 1,
           Bronze => 10,
           Copper => 100,
           Desert => 1000
          );

   Home_Room_Col : array ( Color ) of Positive
   -- home column
      := ( Amber => 4,
           Bronze => 6,
           Copper => 8,
           Desert => 10
          );

   -- SUBSECTION
   -- the map

   type Filler is ( Wall, Space, Unreachable, Creature );

   Num_Cols : constant Positive := 13;
   Map      : constant array ( 1 .. 7, 1 .. Num_Cols ) of Filler
   -- this is just the map; amphipods are not stored in this structure
   -- this map is designed to work for both parts 1 and 2, which means
   -- i have to be careful when reading it
      := ( ( others => Wall ),
           ( 1 | 13 => Wall,
             others => Space
            ),
           ( 4 | 6 | 8 | 10 => Space,
             others => Wall
            ),
           ( 4 | 6 | 8 | 10 => Space,
             3 | 5 | 7 | 9 | 11 => Wall,
             others => Unreachable
            ),
           -- for part 2
           ( 4 | 6 | 8 | 10 => Space,
             3 | 5 | 7 | 9 | 11 => Wall,
             others => Unreachable
            ),
           -- for part 2
           ( 4 | 6 | 8 | 10 => Space,
             3 | 5 | 7 | 9 | 11 => Wall,
             others => Unreachable
            ),
           ( 3 .. 11 => Wall,
             others  => Unreachable
            )
          );

   -- SUBSECTION
   -- Amphipods

   type Position is record
      Row, Col : Natural;
   end record;

   type Amphipod is record
      Clr : Color;
      Pos : Position;
   end record;

   subtype Part_1_Range is Positive range 1 .. 8;
   -- amphipods for part 1
   subtype Part_2_Range is Positive range 1 .. 16;
   -- amphipods for part 2
   type Amphipod_Array is array ( Positive range <> ) of Amphipod;
   subtype Amphipod_Array_1 is Amphipod_Array ( Part_1_Range );
   subtype Amphipod_Array_2 is Amphipod_Array ( Part_2_Range );

   Setup_Part_1 : Amphipod_Array_1
      := ( ( Clr => Amber, Pos => ( 4, 4 ) ),
           ( Clr => Amber, Pos => ( 4, 10 ) ),
           ( Clr => Bronze, Pos => ( 3, 4 ) ),
           ( Clr => Bronze, Pos => ( 3, 8 ) ),
           ( Clr => Copper, Pos => ( 3, 6 ) ),
           ( Clr => Copper, Pos => ( 4, 8 ) ),
           ( Clr => Desert, Pos => ( 4, 6 ) ),
           ( Clr => Desert, Pos => ( 3, 10 ) )
          );

   Setup_Part_2 : Amphipod_Array_2
      := ( ( Clr => Amber, Pos => ( 6, 4 ) ),
           ( Clr => Amber, Pos => ( 6, 10 ) ),
           ( Clr => Bronze, Pos => ( 3, 4 ) ),
           ( Clr => Bronze, Pos => ( 3, 8 ) ),
           ( Clr => Copper, Pos => ( 3, 6 ) ),
           ( Clr => Copper, Pos => ( 6, 8 ) ),
           ( Clr => Desert, Pos => ( 6, 6 ) ),
           ( Clr => Desert, Pos => ( 3, 10 ) ),
           -- for part 2
           ( Clr => Desert, Pos => ( 4, 4 ) ),
           ( Clr => Copper, Pos => ( 4, 6 ) ),
           ( Clr => Bronze, Pos => ( 4, 8 ) ),
           ( Clr => Amber, Pos => ( 4, 10 ) ),
           ( Clr => Desert, Pos => ( 5, 4 ) ),
           ( Clr => Bronze, Pos => ( 5, 6 ) ),
           ( Clr => Amber, Pos => ( 5, 8 ) ),
           ( Clr => Copper, Pos => ( 5, 10 ) )
          );

   -- SUBSECTION
   -- amphipod tracking

   type Part is ( One, Two );

   package Amphipod_Array_1_Vectors is new Ada.Containers.Indefinite_Vectors
      ( Index_Type => Positive,
        Element_Type => Amphipod_Array_1
       );

   package Amphipod_Array_2_Vectors is new Ada.Containers.Indefinite_Vectors
      ( Index_Type => Positive,
        Element_Type => Amphipod_Array_2
       );

   type State ( Which : Part := One ) is record
   -- used for breadth-first search lower down
      Energy : Natural;
      case Which is
         when One => Amphipods_1 : Amphipod_Array_1_Vectors.Vector;
         when Two => Amphipods_2 : Amphipod_Array_2_Vectors.Vector;
      end case;
   end record;

   function State_Priority ( S : State ) return Natural is ( S.Energy );
   -- for the priority queue

   function State_Hash ( Amphipods : Amphipod_Array )
                           return Ada.Containers.Hash_Type
   is
   -- for keeping track of previous work
   -- this is probably not the best approach
      ( if Amphipods'Length = 8 then
           Ada.Containers.Hash_Type
              (  Amphipods ( 1 ).Pos.Row * 3
               + Amphipods ( 1 ).Pos.Col * 5
               + Amphipods ( 2 ).Pos.Row * 7
               + Amphipods ( 2 ).Pos.Col * 11
               + Amphipods ( 3 ).Pos.Row * 13
               + Amphipods ( 3 ).Pos.Col * 17
               + Amphipods ( 4 ).Pos.Col * 19
               + Amphipods ( 4 ).Pos.Col * 23
               + Amphipods ( 5 ).Pos.Row * 29
               + Amphipods ( 5 ).Pos.Col * 31
               + Amphipods ( 6 ).Pos.Row * 37
               + Amphipods ( 6 ).Pos.Col * 41
               + Amphipods ( 7 ).Pos.Row * 43
               + Amphipods ( 7 ).Pos.Col * 47
               + Amphipods ( 8 ).Pos.Col * 53
               + Amphipods ( 8 ).Pos.Col * 57
              )
        else
           Ada.Containers.Hash_Type
              (  Amphipods ( 1 ).Pos.Row * 3
               + Amphipods ( 1 ).Pos.Col * 5
               + Amphipods ( 2 ).Pos.Row * 7
               + Amphipods ( 2 ).Pos.Col * 11
               + Amphipods ( 3 ).Pos.Row * 13
               + Amphipods ( 3 ).Pos.Col * 17
               + Amphipods ( 4 ).Pos.Col * 19
               + Amphipods ( 4 ).Pos.Col * 23
               + Amphipods ( 5 ).Pos.Row * 29
               + Amphipods ( 5 ).Pos.Col * 31
               + Amphipods ( 6 ).Pos.Row * 37
               + Amphipods ( 6 ).Pos.Col * 41
               + Amphipods ( 7 ).Pos.Row * 43
               + Amphipods ( 7 ).Pos.Col * 47
               + Amphipods ( 8 ).Pos.Col * 53
               + Amphipods ( 8 ).Pos.Col * 59
               + Amphipods ( 9 ).Pos.Row * 61
               + Amphipods ( 9 ).Pos.Col * 67
               + Amphipods ( 10 ).Pos.Row * 71
               + Amphipods ( 10 ).Pos.Col * 73
               + Amphipods ( 11 ).Pos.Row * 79
               + Amphipods ( 11 ).Pos.Col * 83
               + Amphipods ( 12 ).Pos.Col * 89
               + Amphipods ( 12 ).Pos.Col * 97
               + Amphipods ( 13 ).Pos.Row * 101
               + Amphipods ( 13 ).Pos.Col * 103
               + Amphipods ( 14 ).Pos.Row * 107
               + Amphipods ( 14 ).Pos.Col * 109
               + Amphipods ( 15 ).Pos.Row * 113
               + Amphipods ( 15 ).Pos.Col * 127
               + Amphipods ( 16 ).Pos.Col * 131
               + Amphipods ( 16 ).Pos.Col * 137
              )
       );

   package Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
      ( Element_Type => State );

   package Queues is new Ada.Containers.Unbounded_Priority_Queues
      ( Queue_Interfaces => Interfaces,
        Queue_Priority   => Natural,
        Get_Priority     => State_Priority,
        Before           => "<"
       );

   package State_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      ( Key_Type       => Amphipod_Array,
        Element_Type    => Natural,
        Hash            => State_Hash,
        Equivalent_Keys => "="
       );

   -- SECTION
   -- I/O

   Invalid_Amphipod : exception;
   -- never triggered, fortunately
   Invalid_Amphipod_Start : exception;
   -- never triggered, fortunately

   procedure Place_Amphipod
      ( S : String; Row, Col : Positive; Setup : in out Amphipod_Array )
   is
   -- parses S ( Clr ) to place an amphipod
   -- in short, don't send a value for C if S ( Clr ) is not one of 'A'..'D'
   -- it works by setting the first amphipod corresponding to S ( Clr )
   -- that is not at ( 1, 1 )
   -- if both amphipods corresponding to S ( Clr ) are set,
   -- it raises Invalid_Amphipod
   begin

      case S ( Col ) is

         when 'A' =>
            if Setup ( 1 ).Pos = ( 1, 1 ) then
               Setup ( 1 ).Pos := ( Row, Col );
            elsif Setup ( 2 ).Pos = ( 1, 1 ) then
               Setup ( 2 ).Pos := ( Row, Col );
            else
               raise Invalid_Amphipod with Positive'Image ( Col );
            end if;

         when 'B' =>
            if Setup ( 3 ).Pos = ( 1, 1 ) then
               Setup ( 3 ).Pos := ( Row, Col );
            elsif Setup ( 4 ).Pos = ( 1, 1 ) then
               Setup ( 4 ).Pos := ( Row, Col );
            else
               raise Invalid_Amphipod with Positive'Image ( Col );
            end if;

         when 'C' =>
            if Setup ( 5 ).Pos = ( 1, 1 ) then
               Setup ( 5 ).Pos := ( Row, Col );
            elsif Setup ( 6 ).Pos = ( 1, 1 ) then
               Setup ( 6 ).Pos := ( Row, Col );
            else
               raise Invalid_Amphipod with Positive'Image ( Col );
            end if;

         when 'D' =>
            if Setup ( 7 ).Pos = ( 1, 1 ) then
               Setup ( 7 ).Pos := ( Row, Col );
            elsif Setup ( 8 ).Pos = ( 1, 1 ) then
               Setup ( 8 ).Pos := ( Row, Col );
            else
               raise Invalid_Amphipod with Positive'Image ( Col );
            end if;

         when others =>
            raise Invalid_Amphipod_Start with Positive'Image ( Col );

      end case;

   end Place_Amphipod;

   procedure Read_Input ( Setup : in out Amphipod_Array ) is
   -- reads the input and emplaces the amphipods

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := "input.txt";
      Bottom_Row : Positive := ( if Doing_Part_1 then 4 else 6 );

   begin

      for A of Setup ( 1 .. 8 ) loop
         A.Pos := ( 1, 1 );
      end loop;

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);
      -- first two rows tell us nothing about amphipods
      Text_IO.Skip_Line ( Input_File );
      Text_IO.Skip_Line ( Input_File );

      declare
         S : String := Text_IO.Get_Line ( Input_File );
      begin
         for Col in 2 .. 5 loop
            Place_Amphipod ( S, 3, 2 * Col, Setup );
         end loop;
      end;

      declare
         S : String := Text_IO.Get_Line ( Input_File );
      begin
         for Col in 2 .. 5 loop
            Place_Amphipod ( S, Bottom_Row, 2 * Col, Setup );
         end loop;
      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   procedure Draw_Amphipods ( Amphipods : Amphipod_Array ) is
      type Raster is array ( 1 .. 7, 1 .. 13 ) of Character;

      Last_Row : Positive := ( if Doing_Part_1 then 5 else 7 );

      Background : Raster
         := ( ( others => '#' ),
              ( 1 | 13 => '#', others => '.' ),
              ( 4 | 6 | 8 | 10 => '.', others => '#' ),
              ( 4 | 6 | 8 | 10 => '.', 1 | 2 | 12 | 13 => ' ', others => '#' ),
              ( 4 | 6 | 8 | 10 => ( if Doing_Part_1 then '#' else '.' ),
                1 | 2 | 12 | 13 => ' ',
                others          => '#' ),
              ( 4 | 6 | 8 | 10 => ( if Doing_Part_1 then '#' else '.' ),
                1 | 2 | 12 | 13 => ' ',
                others          => '#' ),
              ( 1 | 2 | 12 | 13 => ' ', others => '#' )
             );
   begin

      for A of Amphipods loop
         Background ( A.Pos.Row , A.Pos.Col ) :=
            ( case A.Clr is
                 when Amber => 'A',
                 when Bronze => 'B',
                 when Copper => 'C',
                 when Desert => 'D',
                 when others => raise Invalid_Amphipod
             );
      end loop;

      for Row in 1 .. Last_Row loop
         for Col in Raster'Range (2) loop
            Text_IO.Put ( Background ( Row, Col ) );
         end loop;
         Text_IO.New_Line;
      end loop;

   end Draw_Amphipods;

   procedure Draw_Map ( S : State ) is
   -- draws the current state of the map corresponding to S
   -- we start with a generic raster that represents the map,
   -- write the amphipods onto it, then print that out

   begin

      if Doing_Part_1 then

         declare
            Vec : Amphipod_Array_1_Vectors.Vector := S.Amphipods_1;
         begin

            for Amphipods of Vec loop
               Draw_Amphipods ( Amphipods );
            end loop;

         end;

      else

         declare
            Vec : Amphipod_Array_2_Vectors.Vector := S.Amphipods_2;
         begin

            for Amphipods of Vec loop
               Draw_Amphipods ( Amphipods );
            end loop;

         end;


      end if;

      Text_IO.Put_Line ( "final energy" & S.Energy'Image );

   end Draw_Map;

   procedure Write_To_Ppm ( S : State ) is

      Output_File : Text_IO.File_Type;

      Scale       : constant Positive := 100;
      Last_Row    : constant Positive := ( if Doing_Part_1 then 5 else 7 );

      subtype Rows is Integer range 1 .. Scale * Last_Row;
      subtype Cols is Integer range 1 .. Scale * Num_Cols;

      Width       : constant Positive := Cols'Last - Cols'First + 1;
      Height      : constant Positive := Rows'Last - Rows'First + 1;

      Num_States  : constant Positive
         := Positive ( if Doing_Part_1 then S.Amphipods_1.Length
                       else S.Amphipods_2.Length );

   begin

      for Step in 1 .. Num_States loop

         Text_IO.Create (Output_File,
                         Name => "state_"
                         & ( if Doing_Part_1 then "1_" else "2_" )
                         & Step'Image & ".ppm");
         Text_IO.Put (Output_File, "P3");
         Text_IO.Put (Output_File, Width'Image);
         Text_IO.Put (Output_File, Height'Image);
         Text_IO.Put (Output_File, " 255"); -- max color
         Text_IO.New_Line (Output_File);

         for Row in 1 .. Last_Row loop

            declare
               type Color is ( Red, Green, Blue );
               type Colors is array ( Color ) of Natural;
               Scanline : array ( 1 .. Num_Cols ) of Colors;
               Pixels_Written : Natural := 0;
            begin

               for Col in 1 .. Num_Cols loop

                  declare
                     Pixel : Colors;
                     Amphipods : Amphipod_Array
                        := ( if Doing_Part_1 then S.Amphipods_1 ( Step )
                             else S.Amphipods_2 ( Step ) );
                  begin

                     Pixel := ( case Map ( Row, Col ) is
                                   when Space => ( 255, 255, 255 ),
                                   when others => ( 0, 0, 0 )
                               );

                     if Row = Last_Row then Pixel := ( 0, 0, 0 ); end if;

                     for A of Amphipods loop
                        if A.Pos.Row = Row and then A.Pos.Col = Col then
                           Pixel := ( case A.Clr is
                                         when Amber => ( 255, 191, 1 ),
                                         when Bronze => ( 205, 127, 50 ),
                                         when Copper => ( 184, 115, 51 ),
                                         when Desert => ( 193, 154, 107 )
                                     );
                           exit;
                        end if;
                     end loop;

                     Scanline ( Col ) := Pixel;

                  end;

               end loop;

               for Scale_Row in 1 .. Scale loop
                  for Col in 1 .. Num_Cols loop

                     for Scale_Col in 1 .. Scale loop
                        Text_IO.Put_Line ( Output_File,
                                           Scanline ( Col ) ( Red )'Image
                                           & Scanline ( Col ) ( Green )'Image
                                           & Scanline ( Col ) ( Blue )'Image );
                        Pixels_Written := Pixels_Written + 1;
                     end loop;

                  end loop;
               end loop;

            end;

         end loop;

         Text_IO.Close (Output_File);

      end loop;

   end Write_To_Ppm;

   -- SECTION
   -- common to both Parts 1 and 2

   function Is_Room ( P : Position ) return Boolean is
   -- returns True if P is the position of a room
      ( P.Row > 2
        and then
           ( P.Col = 4 or else P.Col = 6 or else P.Col = 8 or else P.Col = 10 )
      );

   function Room_Adjacent ( P : Position ) return Boolean is
      -- returns True if P is a position in row 2 adjacent to a room
      ( P.Row = 2
        and then
           ( P.Col = 4 or else P.Col = 6 or else P.Col = 8 or else P.Col = 10 )
       );

   function Room_Ready ( C : Color; S : State ) return Boolean is
   -- returns True if the only occupants of C's home room are of color C

      Amphipods : Amphipod_Array
         := ( if S.Which = One then S.Amphipods_1.Last_Element
              else S.Amphipods_2.Last_Element );

   begin

      return
         ( for all A of Amphipods =>
              A.Clr = C
           or else ( A.Pos.Row = 2  )
           or else ( A.Pos.Col /= Home_Room_Col ( C ) )
          );

   end Room_Ready;

   function Rule_1_Satisfied ( P : Position ) return Boolean is
   -- True iff P is not "adjacent" to a room
   --
   -- this is puzzle rule 1:
   -- never stop on the space immediately outside any room
      ( not Room_Adjacent ( P ) );

   function Rule_2_Satisfied ( A : Color; P : Position; S : State )
                              return Boolean
   is
   -- True iff P is not a room or else it's A's home room
   -- and no other colors are there
   --
   -- this is puzzle rule 2:
   -- never move into a room unless it's their homeroom
   -- AND its only occupants are of its color
      ( not Is_Room ( P )
        or else
           ( P.Col = Home_Room_Col ( A ) and then Room_Ready ( A, S ) )
       );

   function Rule_3_Satisfied ( A : Amphipod; P : Position ) return Boolean
   -- True iff A is not in a hallway or P is a room
   --
   -- this is puzzle rule 3:
   -- once in the hallway, stay in that spot until it can move into a room
   is ( A.Pos.Row /= 2 or else Is_Room ( P ) );

   function Reached_Goal ( Amphipods : Amphipod_Array ) return Boolean is
   -- True iff all amphipods are in their correct home rooms
      ( for all A of Amphipods =>
           ( A.Pos.Col = Home_Room_Col ( A.Clr ) )
       );

   function Following_Rows_Filled
      ( A : Amphipod; Pos : Position; Amphipods : Amphipod_Array )
       return Boolean
   is
   -- True iff amphipod A is asking to move to position P
   -- and all the columns below P are filled amphipods of the same color
   --
   -- this prevents us from having to waste time queuing moves
   -- that merely move an amphipod down the same column so as to fit others
      ( for all R in Pos.Row + 1 .. ( if Doing_Part_1 then 4 else 6 ) =>
              ( for some B of Amphipods =>
                      B.Clr = A.Clr
                and then A /= B
                and then B.Pos.Col = Pos.Col
                and then B.Pos.Row = R
               )
       );

   function Way_Is_Blocked ( A : Amphipod; Pos : Position; Current : State )
                            return Boolean
   is
      Amphipods : Amphipod_Array
         := ( if Current.Which = One then Current.Amphipods_1.Last_Element
              else Current.Amphipods_2.Last_Element );
   begin
      -- when moving in row 2, make sure no one else stands in the way
      for Col in Positive'Min ( Pos.Col, A.Pos.Col )
         .. Positive'Max ( Pos.Col, A.Pos.Col )
      loop
         for B of Amphipods loop
            if A /= B and then B.Pos.Row = 2 and then B.Pos.Col = Col then
               return True;
            end if;
         end loop;
      end loop;

      -- when moving into or out of row 2, make sure no one else is in the way
      if Pos.Row /= A.Pos.Row then

         -- moving from row 3 is taken care of by loop on moving in row 2
         if A.Pos.Row > 3
            and then ( for some B of Amphipods =>
                           B.Pos.Col = A.Pos.Col
                        and then B.Pos.Row < A.Pos.Row )
         then
            return True;
         end if;

         -- moving to row 3 is taken care of by loop on moving in row 2
         if Pos.Row > 3
            and then ( for some B of Amphipods =>
                           B.Pos.Col = Pos.Col
                        and then B.Pos.Row < Pos.Row )
         then
            return True;
         end if;

      elsif Pos.Row > 3 and then A.Pos.Row > 3 then

         if ( for some B of Amphipods =>
                  B.Pos.Row < A.Pos.Row and then B.Pos.Col = A.Pos.Col )
            or else
               ( for some B of Amphipods =>
                     B.Pos.Row < Pos.Row and then B.Pos.Col = Pos.Col )
         then
            return True;
         end if;

      end if;
      return False;
   end Way_Is_Blocked;

   function Can_Travel ( A : Amphipod; Pos : Position; Current : State )
                        return Boolean
   is
   -- True iff A can travel to Pos given the Current state

      Amphipods : Amphipod_Array
         := ( if Current.Which = One then Current.Amphipods_1.Last_Element
              else Current.Amphipods_2.Last_Element );

   begin

      -- don't move to same place
      if A.Pos.Row = Pos.Row and then A.Pos.Col = Pos.Col then
         return False;
      end if;

      -- don't move if already in home room
      if A.Pos.Col = Home_Room_Col ( A.Clr )
         and then
            ( for all B of Amphipods =>
                  ( B.Clr = A.Clr or else B.Pos.Col /= A.Pos.Col ) )
      then
         return False;
      end if;

      -- check rule 1
      if not Rule_1_Satisfied ( Pos ) then return False; end if;

      -- check rule 3
      if not Rule_3_Satisfied ( A, Pos ) then return False; end if;

      -- don't move if someone else is there
      if ( for some B of Amphipods => A /= B and then Pos = B.Pos )
      then
         return False;
      end if;

      -- can't be a wall or a void
      if Map ( Pos.Row, Pos.Col ) /= Space then return False; end if;

      -- impossible to move in row 2
      if Pos.Row = 2 and then Pos.Row = A.Pos.Row then return False; end if;

      -- make sure the way isn't blocked
      if Way_Is_Blocked ( A, Pos, Current ) then
         return False;
      end if;

      -- when moving into room, make sure rule 2 is satisfied
      if Pos.Row > 2 then
         if not Rule_2_Satisfied ( A.Clr, Pos, Current ) then
            return False;
         end if;
      end if;

      -- don't leave gaps when moving into home room
      if Pos.Row > 2 and then not Following_Rows_Filled ( A, Pos, Amphipods )
      then
         return False;
      end if;

      return True;

   end Can_Travel;

   function Can_Travel_Home ( A : Amphipod; Current : State ) return Boolean is
   -- True iff A can travel to its home room

      Col : Positive := Home_Room_Col ( A.Clr );
      Result : Boolean := Room_Ready ( A.Clr, Current )
         and then not Way_Is_Blocked ( A, ( 3, Col ), Current );

   begin
      return Result;
   end Can_Travel_Home;

   function Energy_Cost ( A : Amphipod; Pos : Position ) return Natural is
   -- returns the cost for A to move to position Pos

      -- move along row 2
      Spaces_Moved : Natural := abs ( Pos.Col - A.Pos.Col );

   begin

      -- if we have to move into row 2
      if A.Pos.Row > 2 then
         Spaces_Moved := Spaces_Moved + ( A.Pos.Row - 2 );
      end if;

      -- if we have to move out of row 2
      if Pos.Row > 2 then
         Spaces_Moved := Spaces_Moved +  ( Pos.Row - 2 );
      end if;

      return Spaces_Moved * Energy ( A.Clr );

   end Energy_Cost;

   procedure Enqueue ( I                  : Positive;
                       Pos                : Position;
                       Current            : State;
                       To_Do              : in out Queues.Queue;
                       Explored           : in out State_Maps.Map;
                       Min_Energy_To_Goal : in out Natural
                      )
   is
   -- enqueues in To_Do a modification of Current where
   -- amphipod I moves to position Pos
   --
   -- does not verify that amphipod I can in fact move there,
   -- so do that using the functions supplied above

      New_Amphipods : Amphipod_Array
         := ( if Doing_Part_1 then Current.Amphipods_1.Last_Element
              else Current.Amphipods_2.Last_Element );
      A : Amphipod := New_Amphipods ( I );
      New_Energy : Positive := Current.Energy + Energy_Cost ( A, Pos );

   begin

      New_Amphipods ( I ).Pos := Pos;

      if New_Energy < Min_Energy_To_Goal and then
         ( not Explored.Contains ( New_Amphipods )
            or else Explored ( New_Amphipods ) > New_Energy
           )
      then

         Explored.Include ( New_Amphipods, New_Energy );

         if Reached_Goal ( New_Amphipods ) then
            Min_Energy_To_Goal
               := Natural'Min
                  ( Min_Energy_To_Goal, New_Energy );
         end if;

         if Doing_Part_1 then

            declare
               New_Vecs : Amphipod_Array_1_Vectors.Vector
                  := Amphipod_Array_1_Vectors.Copy ( Current.Amphipods_1 );
            begin
               if Visualize then
                  New_Vecs.Append ( New_Amphipods );
               else
                  New_Vecs ( 1 ) := New_Amphipods;
               end if;
               To_Do.Enqueue ( ( Which    => One,
                                 Energy      => New_Energy,
                                 Amphipods_1 => New_Vecs
                                ) );
            end;

         else

            declare
               New_Vecs : Amphipod_Array_2_Vectors.Vector
                  := Amphipod_Array_2_Vectors.Copy ( Current.Amphipods_2 );
            begin
               if Visualize then
                  New_Vecs.Append ( New_Amphipods );
               else
                  New_Vecs ( 1 ) := New_Amphipods;
               end if;
               To_Do.Enqueue ( ( Which    => Two,
                                 Energy      => New_Energy,
                                 Amphipods_2 => New_Vecs
                                ) );
            end;

         end if;

      end if;
   end Enqueue;

   function Lowest_Energy return Natural is
   -- determines the least energy needed to rearrange the amphipods
   -- so that each is in its home room
   --
   -- the algorithm is breadth-first search, with ordering and pruning
   -- based on weight

      -- used throughout the algorithm
      To_Do    : Queues.Queue;
      Explored : State_Maps.Map;

      -- used on each iteration
      A             : Amphipod;
      Pos           : Position;
      Current       : State;
      Last_Energy   : Natural := 0;
      Amphipods     : aliased Amphipod_Array
         := ( if Doing_Part_1 then Setup_Part_1 else Setup_Part_2 );

      Min_Energy_To_Goal : Natural := Natural'Last;

   begin

      -- prime the queue
      if Doing_Part_1 then

         declare
            New_Vec : Amphipod_Array_1_Vectors.Vector;
         begin
            New_Vec.Append ( Setup_Part_1 );
            To_Do.Enqueue
               ( ( Energy => 0, Which => One, Amphipods_1 => New_Vec ) );
            Explored.Include ( Setup_Part_1, 0 );
         end;

      else

         declare
            New_Vec : Amphipod_Array_2_Vectors.Vector;
         begin
            New_Vec.Append ( Setup_Part_2 );
            To_Do.Enqueue
               ( ( Energy => 0, Which => Two, Amphipods_2 => New_Vec ) );
            Explored.Include ( Setup_Part_2, 0 );
         end;

      end if;

      loop

         To_Do.Dequeue ( Current );
         if Reached_Goal
            ( ( if Doing_Part_1 then Current.Amphipods_1.Last_Element
              else Current.Amphipods_2.Last_Element ) )
         then
            if Visualize then
               Draw_Map ( Current );
               Write_To_Ppm ( Current );
            end if;
            return Current.Energy;
         end if;

         -- don't panic, user! I really am working!
         if Current.Energy > Last_Energy then
            Last_Energy := Current.Energy;
            if Last_Energy mod 100 = 0 then
               Text_IO.Put ( "considering energy" & Last_Energy'Image );
               Text_IO.Put_Line ( " queue size:" & To_Do.Current_Use'Image );
               Draw_Amphipods
                  ( if Doing_Part_1 then Current.Amphipods_1.Last_Element
                    else Current.Amphipods_2.Last_Element);
               Text_IO.New_Line;
            end if;
         end if;

         Amphipods := ( if Doing_Part_1 then Current.Amphipods_1.Last_Element
                        else Current.Amphipods_2.Last_Element );

         if Current.Energy <= Explored ( Amphipods )
            and then Current.Energy <= Min_Energy_To_Goal
         then

            -- see where each amphipod can go
            for I in Amphipods'Range loop

               A := Amphipods ( I );

               if A.Pos.Col /= Home_Room_Col (A.Clr)
                  and then Can_Travel_Home ( A, Current )
               then
                  -- choose best!

                  declare
                     Row : Positive := ( if Doing_Part_1 then 4 else 6 );
                     Col : Positive := Home_Room_Col ( A.Clr );
                  begin

                     -- find lowest row A can move into
                     for B of Amphipods
                     when B.Clr = A.Clr and then B.Pos.Col = Col
                     loop
                           Row := Positive'Min ( Row, B.Pos.Row - 1 );
                     end loop;

                     Enqueue ( I, ( Row, Col ), Current,
                               To_Do, Explored, Min_Energy_To_Goal );
                     --  Text_IO.Put_Line (A.Clr'Image & " at"
                     --                    & A.Pos.Row'Image & A.Pos.Col'Image
                     --                    & " traveled home to"
                     --                    & Row'Image & Col'Image );
                     --  Draw_Map ( Current );
                     --  Text_IO.New_Line;

                  end;

               else
                  -- try others

                  for Row in 2 .. Map'Last ( 1 ) - 1 loop
                     for Col in 2 .. 12 loop

                        Pos := ( Row, Col );

                        if Can_Travel ( A, Pos, Current ) then

                           Enqueue ( I, Pos, Current,
                                     To_Do, Explored, Min_Energy_To_Goal );

                        end if;

                     end loop;
                  end loop;

               end if;

            end loop;

         end if;

      end loop;

   end Lowest_Energy;

begin

   if not Doing_Example then
      Read_Input ( Setup_Part_1 );
   end if;
   Text_IO.Put_Line ( "the lowest energy is" & Lowest_Energy'Image );
   Text_IO.New_Line;
   Text_IO.Put_Line ( "----------------------------------------" );

   Doing_Part_1 := False;
   if not Doing_Example then
      Read_Input ( Setup_Part_2 );
   end if;
   Text_IO.Put_Line ( "the lowest energy is" & Lowest_Energy'Image );

end Day23;
