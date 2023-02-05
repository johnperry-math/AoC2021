-- Advent of Code 2021
--
-- John Perry
--
-- Day 21: Dirac Dice
--
-- part 1: using a deterministic die, determine who gets to 1000 first,
--    and report the "score": lower score times number of rolls
--
-- part 2: using a 3-sided die that splits the universe into 3 every time
--    you roll it, determine who wins the most games, and report that value

with Ada.Text_IO;

procedure Day21 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   -- SECTION
   -- global types and variables

   Max_Position : constant Positive := 10;
   subtype Position_Range is Positive range 1 .. Max_Position;
   type Player is record
      Score : Natural := 0;
      Pos : Position_Range;
   end record;

   First_Player, Second_Player: Player;

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

      declare
         S1: constant String := Text_IO.Get_Line(Input_File);
         S2: constant String := Text_IO.Get_Line(Input_File);
      begin
         First_Player.Pos := Character'Pos(S1(29)) - Character'Pos('0');
         Second_Player.Pos := Character'Pos(S2(29)) - Character'Pos('0');
      end;

      Text_IO.Close (Input_File);

   end Read_Input;

   -- SECTION
   -- common to both Parts 1 and 2

   procedure Update_By ( P : in out Player; Amount : Natural ) is
   -- updates P given a roll of the indicated amount,

   begin

      if Max_Position >= P.Pos + Amount then
         P.Pos := P.Pos + Amount;

      else
         P.Pos := P.Pos + Amount - Max_Position;

      end if;

      P.Score := P.Score + P.Pos;

   end Update_By;

   -- SECTION
   -- Part 1 : the deterministic die

   Die : Natural := 0;

   procedure Roll_Deterministic_Die is
   -- increases Die by 1 each time
   begin

      Die := Die + 1;

   end Roll_Deterministic_Die;

   procedure Move (P : in out Player) is
   -- apply to P a turn of the game using the deterministic die

      Die_Sum : Natural := 0;

   begin

      for I in 1 .. 3 loop
         Roll_Deterministic_Die;
         Die_Sum := Die_Sum + Die;
      end loop;

      Die_Sum := Die_Sum mod 10;
      Update_By ( P, Die_Sum );

   end Move;

   -- PART 2
   -- the universe-splitting die

   type Outcome is record
      P1_Wins, P2_Wins : Long_Long_Integer;
   end record;
   -- used in conjunction with State

   State : array ( 0 .. 20, 0 .. 20, 1 .. 10, 1 .. 10 ) of Outcome
      := ( others => ( others => ( others => ( others => ( 0, 0 ) ) ) ) );
   -- a record of each player's wins, given the indicated scores and positions

   procedure Determine_State_Of
      ( Score1, Score2 : Natural; Pos1, Pos2 : Position_Range )
      with Pre =>
         ( for all S1 in Score1 + 1 .. 20 =>
              ( for all S2 in Score2 + 1 .. 20 =>
                   ( for all P1 in 1 .. 10 =>
                           ( for all P2 in 1 .. 10 =>
                                   State ( S1, S2, P1, P2 ) /= ( 0, 0 )
                            ) ) ) )
   is
   -- records in State the potential wins and losses of the given state
   -- IT IS ASSUMED that the higher states have already been computed;
   -- otherwise, this has random output

      type Possibility_Range is new Positive range 1 .. 7;
      -- number of **distinct** possible outcomes on each turn
      -- this is because some combinations of die rolls
      -- produce the same result, such as (1, 1, 2), (1, 2, 1), and (2, 1, 1)

      type Universe_Array is array ( Possibility_Range ) of Player;
      -- each distinct outcome for a player

      type Sum_Array is array ( Possibility_Range ) of Positive;
      -- each distinct sum of die rolls

      type Multiplier_Array is array ( Possibility_Range ) of Long_Long_Integer;
      -- the number of repetitions of each element
      -- of Universe_Array and Sum_Array

      P, Q          : Player;
      Current_State : Outcome;
      Wins1, Wins2  : Long_Long_Integer := 0;

      P1_States     : Universe_Array := ( others => ( Score1, Pos1 ) );
      P2_States     : Universe_Array;

      Sums          : constant Sum_Array := ( 3, 4, 5, 6, 7, 8, 9 );
      Multipliers   : constant Multiplier_Array := ( 1, 3, 6, 7, 6, 3, 1 );

   begin

      -- determine p1's states
      for I in Possibility_Range loop
         Update_By ( P1_States ( I ), Sums ( I ) );
      end loop;

      -- check each state
      for I in Possibility_Range loop

         P := P1_States ( I );

         if P.Score > 20 then
            -- when p1 wins, update by the number of universes with that state

            Wins1 := Wins1 + Multipliers (I);

         else
            -- when p1 doesn't win, consider p2's states

            P2_States := ( others => ( Score2, Pos2 ) );
            for J in Possibility_Range loop
               Update_By ( P2_States ( J ), Sums ( J ) );
            end loop;

            -- check each state
            for J in Possibility_Range loop

               Q := P2_States ( J );

               if Q.Score > 20 then
                  -- when p2 wins, update by the number of universes
                  -- with that state

                  Wins2 := Wins2 + Multipliers ( J ) * Multipliers ( I );

               else
                  -- when p2 doesn't win, we are now in a "higher" position;
                  -- given the assumption that "higher" positions
                  -- are computed first, we can look it up

                  Current_State := State ( P.Score, Q.Score, P.Pos, Q.Pos );
                  Wins1 := Wins1
                     + Multipliers ( J ) * Current_State.P1_Wins
                     * Multipliers ( I );
                  Wins2 := Wins2
                     + Multipliers ( J ) * Current_State.P2_Wins
                     * Multipliers ( I );

               end if;

            end loop;

         end if;

      end loop;

      State ( Score1, Score2, Pos1, Pos2 )
         := ( P1_Wins => Wins1, P2_Wins => Wins2 );

   end Determine_State_Of;

   procedure Fill_State is
   begin
      for Score1 in reverse 0 .. 20 loop
         for Score2 in reverse 0 .. 20 loop
            for Pos1 in reverse 1 .. 10 loop
               for Pos2 in reverse 1 .. 10 loop
                  Determine_State_Of ( Score1, Score2, Pos1, Pos2);
               end loop;
            end loop;
         end loop;
      end loop;
   end Fill_State;

begin

   -- part 1

   declare
      Num_Rolls     : Natural := 0;
      P1, P2, Loser : Player;
   begin
      if Doing_Example then
         P1.Pos := 4;
         P2.Pos := 8;
      else
         Read_Input;
         P1 := First_Player;
         P2 := Second_Player;
      end if;
      loop
         Move ( P1 );
         Num_Rolls := Num_Rolls + 3;
         if P1.Score >= 1000 then
            Loser := P2;
            exit;
         end if;
         Move ( P2 );
         Num_Rolls := Num_Rolls + 3;
         if P2.Score >= 1000 then
            Loser := P1;
            exit;
         end if;
      end loop;
      Text_IO.Put_Line ( "product is"
                         & Positive'Image (Loser.Score * Num_Rolls) );
   end;

   -- part 2

   Fill_State;

   declare
      Result : constant Outcome
         := ( if Doing_Example then State ( 0, 0, 4, 8 )
              else State
                 ( 0, 0, First_Player.Pos, Second_Player.Pos )
             );
   begin
      Text_IO.Put_Line ( "winner wins"
                        & Long_Long_Integer'Image
                         (Long_Long_Integer'Max
                                ( Result.P1_Wins, Result.P2_Wins )
                            )
                         & " games"
                        );
   end;

end Day21;
