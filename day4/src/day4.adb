--  Advent of Code 2021
--
--  John Perry
--
--  Day 4: Giant Squid
--
--  part 1: find the first bingo board that will win
--
--  part 2: maybe beating a giant squid at bingo isn't the best idea;
--     find the _last_ board that will win

pragma Ada_2020;

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day4 is

   package Text_IO renames Ada.Text_IO;
   package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   --  SECTION
   --  global types and variables

   package Nat_Vecs is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Natural
      );

   Bingo_Values : Nat_Vecs.Vector;

   type Bingo_Position is record
      Value : Natural;
      Called : Boolean;
   end record;
   --  records the value of a position on a bingo card,
   --  as well as whether it's been called

   type Bingo_Board is array (1 .. 5, 1 .. 5) of Bingo_Position;
   --  board, card, same thing

   package Bingo_Vecs is new Ada.Containers.Vectors
      (Index_Type => Positive,
       Element_Type => Bingo_Board
      );

   Bingo_Boards : Bingo_Vecs.Vector;

   --  SECTION
   --  I/O

   pragma Warnings (Off, "procedure * is not referenced");
   procedure Put_Board (Board : Bingo_Board) is
   --  proved useful in debugging

   begin

      for Row in 1 .. 5 loop
         for Col in 1 .. 5 loop

            Natural_IO.Put (Board (Row, Col).Value, 4);
            if Board (Row, Col).Called then
               Text_IO.Put ('X');
            end if;

         end loop;

         Text_IO.New_Line;

      end loop;

   end Put_Board;
   pragma Warnings (On, "procedure * is not referenced");

   procedure Read_Input is
   --  reads the bingo numbers, then the bingo cards

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      declare
         Input_String : constant String := Text_IO.Get_Line (Input_File);
         Pos          : Positive := 1;
         Val          : Natural;
      begin

         while Pos <= Input_String'Length loop
            Natural_IO.Get
               (Input_String (Pos .. Input_String'Length), Val, Pos);
            Bingo_Values.Append (Val);
            Pos := Pos + 2;
         end loop;

      end;

      while not Text_IO.End_Of_File (Input_File) loop

            --  empty row
         declare
            pragma Warnings (Off, "constant * is not referenced");
            Input_String : constant String
               := Text_IO.Get_Line (Input_File);
            pragma Warnings (On, "constant * is not referenced");
         begin
            null;
         end;

         declare
            Board : Bingo_Board;
         begin

            for Row in 1 .. 5 loop
               declare
                  Input_String : constant String
                     := Text_IO.Get_Line (Input_File);
                  Pos          : Positive := 1;
                  Val          : Natural;
               begin
                  for Col in 1 .. 5 loop
                     Natural_IO.Get
                        (Input_String
                            (Pos .. Input_String'Length), Val, Pos
                        );
                     Board (Row, Col) := (Val, False);
                     Pos := Pos + 2;
                  end loop;
               end;

            end loop;

            Bingo_Boards.Append (Board);

         end;

      end loop;

      Text_IO.Close (Input_File);

      Text_IO.Put_Line ("there are" & Bingo_Values.Length'Image
                        & " numbers and" & Bingo_Boards.Length'Image
                        & " cards");

   end Read_Input;

   --  SECTION
   --  functions useful to both PARTS 1 AND 2

   function Has_Bingo (Card : Bingo_Board) return Boolean is
   --  returns True iff Card has a row or column of called values
      (
          (for some Row in 1 .. 5 =>
                 (for all Col in 1 .. 5 =>
                        Card (Row, Col).Called
                 )
          )
       or else
          (for some Col in 1 .. 5 =>
                 (for all Row in 1 .. 5 =>
                        Card (Row, Col).Called
                 )
          )
      );

   function Sum_Unmarked (Board : Bingo_Board) return Natural is
   --  sums the unmarked values of Board
      Result : Natural := 0;

   begin

      for Row in 1 .. 5 loop
         for Col in 1 .. 5 loop

            if not Board (Row, Col).Called then
               Result := Result + Board (Row, Col).Value;
            end if;

         end loop;
      end loop;

      return Result;

   end Sum_Unmarked;

   --  SECTION
   --  PART 1

   function Best_Final_Score return Natural is
   --  determines the score of the board that first achieves bingo
   --  with the given values

   begin

      Values : for Value of Bingo_Values loop
         Boards : for Board_Number in 1 .. Positive (Bingo_Boards.Length) loop

            declare
               Board : Bingo_Board renames Bingo_Boards (Board_Number);
            begin

               Rows : for Row in 1 .. 5 loop
                  Cols : for Col in 1 .. 5 loop

                     if Board (Row, Col).Value = Value then
                        Board (Row, Col).Called := True;
                        if Has_Bingo (Board) then
                           return Value * Sum_Unmarked (Board);
                        end if;
                     end if;

                  end loop Cols;
               end loop Rows;

            end;

         end loop Boards;
      end loop Values;

      return 0;

   end Best_Final_Score;

   --  SECTION
   --  PART 2

   function Last_Winning_Board_Score return Natural is
   --  determines the score of the last board to hit bingo
   --  WHEN IT FIRST HITS BINGO

      Result : Natural := 0;
      Board_Has_Won : array (1 .. Bingo_Boards.Length) of Boolean
         := (others => False);
      --  marked as True whenever the board has won,
      --  so that it is not checked again

   begin

      Values : for Value of Bingo_Values loop
         Boards : for Board_Number in 1 .. Bingo_Boards.Length loop

            if not Board_Has_Won (Board_Number) then

               declare
                  Board : Bingo_Board
                  renames Bingo_Boards (Positive (Board_Number));
               begin

                  Rows : for Row in 1 .. 5 loop
                     Cols : for Col in 1 .. 5 loop

                        if Board (Row, Col).Value = Value then
                           Board (Row, Col).Called := True;
                           if Has_Bingo (Board) then
                              Result := Value * Sum_Unmarked (Board);
                              Board_Has_Won (Board_Number) := True;
                              if (for all Board_Won of Board_Has_Won
                                    => Board_Won)
                              then
                                 return Result;
                              end if;
                           end if;
                        end if;

                     end loop Cols;
                  end loop Rows;

               end;

            end if;

         end loop Boards;
      end loop Values;

      return 0;

   end Last_Winning_Board_Score;

   procedure Clear_Boards is
   begin
      for Board of Bingo_Boards loop
         for Row in 1 .. 5 loop
            for Col in 1 .. 5 loop
               Board (Row, Col).Called := False;
            end loop;
         end loop;
      end loop;
   end Clear_Boards;

begin

   Read_Input;

   Text_IO.Put_Line ("the best final score is" & Best_Final_Score'Image);
   Clear_Boards;
   Text_IO.Put_Line ("the last winning board's score is"
                     & Last_Winning_Board_Score'Image);

end Day4;
