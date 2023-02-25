-- Advent of Code 2021
--
-- John Perry
--
-- Day 24: Arithmetic Logic Unit
--
-- part 1: find the largest model number accepted by MONAD
--    (the model number is a sequence of 14 non-zero digits,
--     and MONAD is a program to input)
--
-- part 2: find the smallest

with Ada.Text_IO;

procedure Day24 is

   package Text_IO renames Ada.Text_IO;

   Doing_Example : constant Boolean := False;

   -- SECTION
   -- global types and variables

   Input_Digits : constant Positive := 14;
   -- number of digits in model number

   subtype Index_Range is Positive range 1 .. Input_Digits;
   -- for type checking

   type Encoded_Digit ( Has_Index : Boolean := False ) is record
   -- records what the program does to an input digit

      Offset : Integer;

      case Has_Index is
         when True   => Index : Index_Range;
         when others => null;
      end case;

   end record;

   Subtype Half_Range is Positive range 1 .. Input_Digits / 2 + 1;
   -- if more base-26 digits than this are added to register Z,
   -- the program cannot reduce it to 0

   type Stack_Array is array ( Half_Range ) of Encoded_Digit;
   -- max number of base-26 digits register Z will need to track

   type Base_26_Value is record
   -- only Z needs all of this
      Top    : Half_Range;
      Values : Stack_Array;
   end record;

   type Register is ( X, Z ); -- don't need W or Y in this algorithm

   Registers : array ( Register ) of Base_26_Value
      := ( others =>
              ( Top   => 1,
                Values =>
                   ( others =>
                        ( Has_Index => False, Offset => 0 )
                    )
               )
          );

   Invalid_Register : exception;

   type Constraint is record
   -- captures the relationship "Dependent = Independent + Offset"
      Dependent, Independent : Index_Range;
      Offset                 : Integer;
   end record;

   Constraints : array ( Half_Range ) of Constraint;
   Num_Constraints : Half_Range := 1;
   -- probably should have used a vector, as this is kind of lame

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

   Unexpected_Instruction : exception;

   procedure Read_Input is
   -- reads the input and determines the constraints

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := "input.txt";

      Add_To_Z : Boolean := True;
      -- whether this block of 18 statements adds a new base-26 value to Z

      Last_Z renames Registers ( Z ).Top;

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      -- 14 times we parse 18 statements
      -- I have optimized them into a more concise form,
      -- but we still have to read the corresponding lines,
      -- so I have labeled each with a comment
      All_Digits : for Index_Digit in 1 .. 14 loop

         -- 1. inp w
         Text_IO.Skip_Line ( Input_File );

         -- 2. mul x 0
         Text_IO.Skip_Line ( Input_File );

         -- 3. add x z
         Text_IO.Skip_Line ( Input_File );
         if Registers ( Z ).Values ( Last_Z ).Has_Index then
            Registers ( X ).Values ( 1 )
               := ( Has_Index => True,
                    Index     => Registers ( Z ).Values ( Last_Z ).Index,
                    Offset    => Registers ( Z ).Values ( Last_Z ).Offset );
         else
            Registers ( X ).Values ( 1 )
               := ( Has_Index => False, Offset => 0 );
         end if;

         -- 4. mod x 26
         Text_IO.Skip_Line ( Input_File );

         -- 5. div z ( 1 or 26 )
         declare
            S : String := Text_IO.Get_Line ( Input_File );
         begin

            if S ( 7 ) = '2' then
               -- dividing by 26, so drop most recent input
               Registers ( Z ).Values ( Last_Z )
                  := ( Has_Index => False, Offset => 0 );
               if Last_Z > 1 then
                  Last_Z := Last_Z - 1;
               end if;
            end if;

         end;

         -- 6. add x ( something )
         declare
            S : String := Text_IO.Get_Line ( Input_File );
            Offset : Integer;
            Pos : Positive := 7;
         begin

            Get_Integer ( S, Offset, Pos );
            --  Text_IO.Put_Line ( "read offset " & Offset'Image );
            Registers ( X ).Values ( 1 ).Offset := @ + Offset;
            --  Text_IO.Put_Line ( "x: " & Registers ( X ).Values ( 1 )'Image );

         end;

         -- 7. eql x w
         Text_IO.Skip_Line ( Input_File );

         -- 8. eql x 0
         Text_IO.Skip_Line ( Input_File );
         Add_To_Z := not ( Registers ( X ).Values ( 1 ).Offset in -9 .. 9 );
         if not Add_To_Z then
            -- we have a new constraint
            Constraints ( Num_Constraints )
               := (Dependent   => Index_Digit,
                   Independent => Registers ( X ).Values ( 1 ).Index,
                   Offset => Registers ( X ).Values ( 1 ).Offset
                  );
            Num_Constraints := Num_Constraints + 1;
         end if;

         -- 9. mul y 0
         Text_IO.Skip_Line ( Input_File );

         -- 10. add y 25
         Text_IO.Skip_Line ( Input_File );

         -- 11. mul y x
         Text_IO.Skip_Line ( Input_File );

         -- 12. add y 1
         Text_IO.Skip_Line ( Input_File );

         -- 13. mul z y
         Text_IO.Skip_Line ( Input_File );

         -- 14. mul y 0
         Text_IO.Skip_Line ( Input_File );

         -- 15. add y w
         Text_IO.Skip_Line ( Input_File );

         -- 16. add y ( something )
         if not Add_To_Z then
            Text_IO.Skip_Line ( Input_File );

         else

            declare
               Offset : Integer;
               -- value to add to `Z`
               Pos    : Positive := 7;
               -- Offset's position in S
               S      : String := Text_IO.Get_Line ( Input_File );
               New_Z  : Half_Range
                  := ( if Registers ( Z ).Values ( Last_Z ).Has_Index
                       then Last_Z + 1
                       else Last_Z );
            begin

               Get_Integer ( S, Offset, Pos );
               Registers ( Z ).Values ( New_Z )
                  := ( Has_Index => True,
                       Offset    => Offset,
                       Index     => Index_Digit );
               Last_Z := New_Z;

            end;

         end if;

         -- 17. mul y x
         Text_IO.Skip_Line ( Input_File );

         -- 18. add z y
         Text_IO.Skip_Line ( Input_File );

      end loop All_Digits;

      Text_IO.Close (Input_File);

   end Read_Input;

   -- SECTION
   -- parts 1 and 2

   type Model_Type is array ( 1 .. 14 ) of Character;
   -- return type for the functions

   -- SECTION
   -- part 1

   function Max_Model return Model_Type is
      Result : Model_Type;
   begin

      for I in Model_Type'Range loop
         for J in 1 .. Num_Constraints loop

            declare
              C renames Constraints ( J );
            begin

               if C.Independent = I then

                  if C.Offset < 0 then
                     Result ( I ) := '9';
                  else
                     Result ( I ) := Character'Val
                        ( Character'Pos ( '0' ) + 9 - C.Offset );
                  end if;

                  exit;

               elsif C.Dependent = I then

                  if C.Offset < 0 then
                     Result ( I ) := Character'Val
                        ( Character'Pos ( '0' ) + 9 + C.Offset );
                  else
                     Result ( I ) := '9';
                  end if;

                  exit;

               end if;

            end;

         end loop;
      end loop;

      return Result;

   end Max_Model;

   -- SECTION
   -- part 2

   function Min_Model return Model_Type is
      Result : Model_Type;
   begin

      for I in Model_Type'Range loop
         for J in 1 .. Num_Constraints loop

            declare
              C renames Constraints ( J );
            begin

               if C.Independent = I then

                  if C.Offset < 0 then
                     Result ( I ) := Character'Val
                           ( Character'Pos ( '0' ) + 1 - C.Offset );
                  else
                     Result ( I ) := '1';
                  end if;

                  exit;

               elsif C.Dependent = I then

                  if C.Offset < 0 then
                     Result ( I ) := '1';
                  else
                     Result ( I )
                        := Character'Val
                           ( Character'Pos ( '0' ) + 1 + C.Offset );
                  end if;

                  exit;

               end if;

            end;

         end loop;
      end loop;

      return Result;

   end Min_Model;

begin

   Read_Input;

   Text_IO.Put_Line ( "found the following constraints:" );
   for I in 1 .. Num_Constraints - 1 loop
      declare
         C : Constraint := Constraints ( I );
      begin
         Text_IO.Put ( "   " );
         Text_IO.Put_Line ("digit" & C.Dependent'Image & " = "
                           & "digit" & C.Independent'Image & " + ("
                           & C.Offset'Image & ")"
                          );
      end;
   end loop;

   Text_IO.Put_Line ( "maximum model number: " & String ( Max_Model ) );
   Text_IO.Put_Line ( "minimum model number: " & String ( Min_Model ) );

end Day24;
