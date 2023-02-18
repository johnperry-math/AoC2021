-- Advent of Code 2021
--
-- John Perry
--
-- Day 23: Amphipod
--
-- part 1: help the amphipods rearrange themselves into their rooms
--
-- part 2:

with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

procedure Day23 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   Doing_Part_1 : constant Boolean := False;

   -- SECTION
   -- global types and variables

   type Color is ( Amber, Bronze, Copper, Desert );

   Energy : array ( Color ) of Positive
      := ( Amber => 1,
           Bronze => 10,
           Copper => 100,
           Desert => 1000
          );

   Preferred_Room_Col : array ( Color ) of Positive
      := ( Amber => 4,
           Bronze => 6,
           Copper => 8,
           Desert => 10
          );

   type Position is record
      Row, Col : Natural;
   end record;

   type Amphipod is record
      Col : Color;
      Pos : Position;
   end record;

   type Filler is ( Wall, Space, Unreachable, Creature );

   Map : array ( 1 .. 5, 1 .. 13 ) of Filler
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
           ( 3 .. 11 => Wall,
             others  => Unreachable
            )
          );

   type Amphipod_Array is array ( 1 .. 8 ) of Amphipod;

   Setup : Amphipod_Array
      := ( ( Col => Amber, Pos => ( 4, 4 ) ),
           ( Col => Amber, Pos => ( 4, 10 ) ),
           ( Col => Bronze, Pos => ( 3, 4 ) ),
           ( Col => Bronze, Pos => ( 3, 8 ) ),
           ( Col => Copper, Pos => ( 3, 6 ) ),
           ( Col => Copper, Pos => ( 4, 8 ) ),
           ( Col => Desert, Pos => ( 4, 6 ) ),
           ( Col => Desert, Pos => ( 3, 10 ) )
          );

   type State is record
      Energy : Natural;
      Amphipods : Amphipod_Array;
   end record;

   -- SECTION
   -- I/O

   Invalid_Amphipod : exception;
   Invalid_Amphipod_Start : exception;

   procedure Place_Amphipod ( S : String; Row, Col : Positive ) is
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

   procedure Read_Input is
   -- reads the input string and extract the target region dimensions

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := "input.txt";

   begin

      for A of Setup loop
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
            Place_Amphipod ( S, 3, 2 * Col );
         end loop;
      end;

      declare
         S : String := Text_IO.Get_Line ( Input_File );
      begin
         for Col in 2 .. 5 loop
            Place_Amphipod ( S, 4, 2 * Col );
         end loop;
      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   procedure Draw_Map ( S : State ) is

      Raster : array ( 1 .. 5, 1 .. 13 ) of Character
         := ( ( others => '#' ),
              ( 1 | 13 => '#', others => '.' ),
              ( 4 | 6 | 8 | 10 => '.', others => '#' ),
              ( 4 | 6 | 8 | 10 => '.', 1 | 2 | 12 | 13 => ' ', others => '#' ),
              ( 1 | 2 | 12 | 13 => ' ', others => '#' )
             );

   begin

      for A of S.Amphipods loop
         Raster ( A.Pos.Row , A.Pos.Col ) :=
            ( case A.Col is
                 when Amber => 'A',
                 when Bronze => 'B',
                 when Copper => 'C',
                 when Desert => 'D',
                 when others => raise Invalid_Amphipod
             );
      end loop;

      Text_IO.Put_Line ( "state with energy" & S.Energy'Image );

      for Row in Raster'Range (1) loop
         for Col in Raster'Range (2) loop
            Text_IO.Put ( Raster ( Row, Col ) );
         end loop;
         Text_IO.New_Line;
      end loop;

   end Draw_Map;

   -- SECTION
   -- common to both Parts 1 and 2

   function Is_Room ( P : Position ) return Boolean is
      ( P.Row > 2
        and then
           ( P.Col = 4 or else P.Col = 6 or else P.Col = 8 or else P.Col = 10 )
      );

   function Room_Adjacent ( P : Position ) return Boolean is
      ( P.Row = 2
        and then
           ( P.Col = 4 or else P.Col = 6 or else P.Col = 8 or else P.Col = 10 )
       );

   function Room_Ready ( Col : Color; S : State ) return Boolean is
      (
       for all A of S.Amphipods =>
          A.Col = Col
       or else ( A.Pos.Row = 2  )
       or else ( A.Pos.Col /= Preferred_Room_Col ( Col ) )
      );

   function Rule_1_Satisfied ( P : Position ) return Boolean is
      ( not Room_Adjacent ( P ) );

   function Rule_2_Satisfied ( A : Color; P : Position; S : State )
                              return Boolean
   is
      ( not Is_Room ( P )
        or else
           ( P.Col = Preferred_Room_Col ( A ) and then Room_Ready ( A, S ) )
       );

   function Rule_3_Satisfied ( A : Amphipod; P : Position ) return Boolean
   is ( A.Pos.Row /= 2 or else Is_Room ( P ) );

   -- SECTION
   -- Part 1

   function Reached_Goal ( S : State ) return Boolean is
      ( for all A of S.Amphipods =>
           ( A.Pos.Col = Preferred_Room_Col ( A.Col ) )
       );

   function Can_Travel ( A : Amphipod; Pos : Position; Current : State )
                        return Boolean
   is
   begin

      -- don't move to same place
      if A.Pos.Row = Pos.Row and then A.Pos.Col = Pos.Col then
         return False;
      end if;

      -- don't move if already in home room
      if A.Pos.Col = Preferred_Room_Col ( A.Col )
         and then
            ( for all B of Current.Amphipods =>
                  ( B.Col = A.Col or else B.Pos.Col /= A.Pos.Col ) )
      then
         return False;
      end if;

      -- check rule 1
      if not Rule_1_Satisfied ( Pos ) then return False; end if;

      -- check rule 3
      if not Rule_3_Satisfied ( A, Pos ) then return False; end if;

      -- don't move if someone else is there
      if ( for some B of Current.Amphipods => A /= B and then Pos = B.Pos )
      then
         return False;
      end if;

      -- can't be a wall or a void
      if Map ( Pos.Row, Pos.Col ) /= Space then return False; end if;

      -- impossible to move in row 2
      if Pos.Row = 2 and then Pos.Row = A.Pos.Row then return False; end if;

      -- when moving in row 2, make sure no one else stands in the way
      for Col in Positive'Min ( Pos.Col, A.Pos.Col )
         .. Positive'Max ( Pos.Col, A.Pos.Col )
      loop
         for B of Current.Amphipods loop
            if A /= B and then B.Pos.Row = 2 and then B.Pos.Col = Col then
               return False;
            end if;
         end loop;
      end loop;

      -- when moving into or out of row 2, make sure no one else is in the way
      if Pos.Row /= A.Pos.Row then

         --  if Pos.Row = 2 then
            -- moving from row 3 is taken care of by loop on moving in row 2
            if A.Pos.Row > 3
               and then ( for some B of Current.Amphipods =>
                              B.Pos.Col = A.Pos.Col
                           and then B.Pos.Row < A.Pos.Row )
            then
               return False;
            end if;

         --  elsif A.Pos.Row = 2 then
            -- moving to row 3 is taken care of by loop on moving in row 2
            if Pos.Row > 3
               and then ( for some B of Current.Amphipods =>
                              B.Pos.Col = Pos.Col
                           and then B.Pos.Row < Pos.Row )
            then
               return False;
            end if;

         --  elsif A.Pos.Row  4 then
         --     if ( for some B of Current.Amphipods => B.Pos.Col = A.Pos.Col ) then
         --        return False;
         --     end if;
         --
         --  elsif Pos.Row = 4 then
         --     if ( for some B of Current.Amphipods => B.Pos.Col = Pos.Col ) then
         --        return False;
         --     end if;
         --  end if;

      elsif Pos.Row > 3 and then A.Pos.Row > 3 then
         if ( for some B of Current.Amphipods =>
                  B.Pos.Row < A.Pos.Row and then B.Pos.Col = A.Pos.Col )
            or else
               ( for some B of Current.Amphipods =>
                     B.Pos.Row < Pos.Row and then B.Pos.Col = Pos.Col )
         then
            return False;
         end if;

      end if;

      -- when moving into room, make sure rule 2 is satisfied
      if Pos.Row > 2 then
         if not Rule_2_Satisfied ( A.Col, Pos, Current ) then
            return False;
         end if;
      end if;

      -- when moving into a room, make sure you move all the way
      if Pos.Row = 3 and then
         ( for some B of Current.Amphipods =>
               B.Col = A.Col
            and then A /= B
            and then B.Pos.Col /= Preferred_Room_Col ( A.Col )
           ) then
         return False;
      end if;

      return True;

   end Can_Travel;

   function Energy_Cost ( A : Amphipod; Pos : Position ) return Natural is
      Spaces_Moved : Natural := abs ( Pos.Col - A.Pos.Col );
   begin

      if Pos.Row > 2 then
         Spaces_Moved := Spaces_Moved +  ( Pos.Row - 2 );
      end if;

      if A.Pos.Row > 2 then
         Spaces_Moved := Spaces_Moved + ( A.Pos.Row - 2 );
      end if;

      return Spaces_Moved * Energy ( A.Col );

   end Energy_Cost;

   function Lowest_Energy return Natural is

      function State_Priority ( S : State ) return Natural is ( S.Energy );
      function State_Hash ( Amphipods : Amphipod_Array )
                           return Ada.Containers.Hash_Type
      is
         ( Ada.Containers.Hash_Type
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
          );

      package Interfaces is new Ada.Containers.Synchronized_Queue_Interfaces
         ( Element_Type => State );

      package Queues is new Ada.Containers.Unbounded_Priority_Queues
         ( Queue_Interfaces => Interfaces,
           Queue_Priority   => Natural,
           Get_Priority     => State_Priority,
           Before           => "<"
          );

      package State_Maps is new Ada.Containers.Hashed_Maps
         ( Key_Type => Amphipod_Array,
           Element_Type => Natural,
           Hash         => State_Hash,
           Equivalent_Keys => "="
          );

      Queue    : Queues.Queue;
      Explored : State_Maps.Map;

      A             : Amphipod;
      Pos           : Position;
      Current, Next : State;
      Last_Energy   : Natural := 0;

      Result : Natural := 0;

   begin

      Queue.Enqueue ( ( Energy => 0, Amphipods => Setup ) );
      Explored.Include ( Setup, 0 );

      loop

         Queue.Dequeue ( Current );
         if Reached_Goal ( Current ) then
            Draw_Map ( Current );
            return Current.Energy;
         end if;

         if Current.Energy <= Explored ( Current.Amphipods ) then

            for I in Current.Amphipods'Range loop

               A := Current.Amphipods ( I );

               for Row in 2 .. 4 loop
                  for Col in 2 .. 12 loop

                     Pos := ( Row, Col );

                     if Can_Travel ( A, Pos, Current ) then

                        Next := Current;
                        Next.Energy := Next.Energy + Energy_Cost ( A, Pos );
                        Next.Amphipods ( I ).Pos := ( Row, Col );

                        if not Explored.Contains ( Next.Amphipods )
                           or else Explored ( Next.Amphipods ) > Next.Energy
                        then
                           Explored.Include ( Next.Amphipods, Next.Energy );
                           Queue.Enqueue ( Next );
                        end if;

                     end if;

                  end loop;

               end loop;

            end loop;

         end if;

      end loop;

   end Lowest_Energy;

begin

   if not Doing_Example then
      Read_Input;
   end if;

   Text_IO.Put_Line ( "the lowest energy is" & Lowest_Energy'Image );

end Day23;
