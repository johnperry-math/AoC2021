--  Advent of Code 2021
--
--  John Perry
--
--  Day 3: Binary Diagnostic
--
--  part 1: determine power consumption
--
--  part 2: determine life support rating

with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day3 is

   package Text_IO renames Ada.Text_IO;
   --  package Natural_IO is new Ada.Text_IO.Integer_IO (Num => Natural);

   Doing_Example : constant Boolean := False;

   type Diagnostic_String is new String (1 .. 12);

   package Diagnostic_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Diagnostic_String
      );
   Diagnostic_Report : Diagnostic_Vectors.Vector;

   procedure Determine_Power_Consumption is

      Input_File  : Text_IO.File_Type;
      Filename    : constant String := (if Doing_Example then "example.txt"
                                        else "input.txt"
                                       );

      Ones        : array (1 .. 12) of Natural := (others => 0);
      Total       : Natural := 0;

   begin

      Text_IO.Open (Input_File, Text_IO.In_File, Filename);

      --  get values as strings, store to Diagnostic_Report for part 2,
      --  and determine the number of 1's in each bit
      while not Text_IO.End_Of_File (Input_File) loop

         Total := Total + 1;

         declare
            Input_String : constant String := Text_IO.Get_Line (Input_File);
         begin

            Diagnostic_Report.Append (Diagnostic_String (Input_String));

            for I in 1 .. 12 loop
               if Input_String (I) = '1' then
                  Ones (I) := Ones (I) + 1;
               end if;
            end loop;

         end;

      end loop;

      Text_IO.Close (Input_File);

      --  determine the gamma and epsilon values
      --  from what we know about the ones
      declare
         Gamma, Epsilon : Natural := 0;
      begin

         for I in 1 .. 12 loop

            Gamma := Gamma * 2;
            Epsilon := Epsilon * 2;

            if Ones (I) > Total - Ones (I) then
               Gamma := Gamma + 1;
            else
               Epsilon := Epsilon + 1;
            end if;

         end loop;

         Text_IO.Put_Line ("power consumption is"
                           & Natural'Image (Gamma * Epsilon)
                          );

      end;

   end Determine_Power_Consumption;

   function Most_Common_Bit (Diagnostics : Diagnostic_Vectors.Vector;
                             I           : Natural
                            )
                            return Character
   is
   --  returns '1' if 1 is the most common bit; otherwise, it returns '0'
   --  ties are broken in favor of 1

      Ones : Natural := 0;

   begin

      for Diagnostic of Diagnostics loop
         if Diagnostic (I) = '1' then
            Ones := Ones + 1;
         end if;
      end loop;

      return (if Ones >= Natural (Diagnostics.Length) - Ones
              then '1'
              else '0'
             );

   end Most_Common_Bit;

   function Least_Common_Bit (Diagnostics : Diagnostic_Vectors.Vector;
                              I           : Natural
                             )
                              return Character
   is
   --  returns 1 if it is the *least* common bit; otherwise, returns 0
   --  ties are broken in favor of 0

      Ones : Natural := 0;

   begin

      for Diagnostic of Diagnostics loop
         if Diagnostic (I) = '1' then
            Ones := Ones + 1;
         end if;
      end loop;

      return (if Ones >= Natural (Diagnostics.Length) - Ones
              then '0'
              else '1'
             );

   end Least_Common_Bit;

   procedure Determine_Life_Support_Rating is

      use type Ada.Containers.Count_Type;

      I : Positive := 1;
      Lcb, Mcb : Character;
      O2_Filter, Co2_Filter, New_Filter : Diagnostic_Vectors.Vector;

   begin

      --  copy the diagnostics into each "filter" vector
      for Diagnostic of Diagnostic_Report loop
         O2_Filter.Append (Diagnostic);
         Co2_Filter.Append (Diagnostic);
      end loop;

      --  going bit-by-bit, filter diagnostics
      --  with least common bit from O2 filter, and
      --  with most common bit from Co2 filter
      while I <= 12 loop

         Mcb := Most_Common_Bit (O2_Filter, I);
         Lcb := Least_Common_Bit (Co2_Filter, I);

         --  filter O2 first
         if O2_Filter.Length > 1 then

            --  filter into New_Filter
            New_Filter.Clear;
            for Diagnostic of O2_Filter loop
               if Diagnostic (I) = Mcb then
                  New_Filter.Append (Diagnostic);
               end if;
            end loop;

            --  copy back into O2_Filter
            O2_Filter.Clear;
            for Diagnostic of New_Filter loop
               O2_Filter.Append (Diagnostic);
            end loop;
         end if;

         --  now filter Co2 filter
         if Co2_Filter.Length > 1 then

            --  filter into New_Filter
            New_Filter.Clear;
            for Diagnostic of Co2_Filter loop
               if Diagnostic (I) = Lcb then
                  New_Filter.Append (Diagnostic);
               end if;
            end loop;

            --  copy back into Co2 filter
            Co2_Filter.Clear;
            for Diagnostic of New_Filter loop
               Co2_Filter.Append (Diagnostic);
            end loop;

         end if;

         I := I + 1;

      end loop;

      --  amazingly, I got these right on the first try...
      if O2_Filter.Length > 1 then
         Text_IO.Put_Line ("o2 filter too long");
         return;
      end if;
      if Co2_Filter.Length > 1 then
         Text_IO.Put_Line ("co2 filter too long");
         return;
      end if;

      --  now determine the ratings
      --  by converting the last remaining filter of each to a number
      declare
         O2_Rating, Co2_Rating : Natural := 0;
      begin

         --  go bit-by-bit
         for I in 1 .. 12 loop

            O2_Rating := O2_Rating * 2;
            if O2_Filter (1)(I) = '1' then
               O2_Rating := O2_Rating + 1;
            end if;

            Co2_Rating := Co2_Rating * 2;
            if Co2_Filter (1)(I) = '1' then
               Co2_Rating := Co2_Rating + 1;
            end if;

         end loop;

         Text_IO.Put_Line ("life support rating is"
                           & Natural'Image (O2_Rating * Co2_Rating));

      end;

   end Determine_Life_Support_Rating;

begin
   Determine_Power_Consumption;
   Determine_Life_Support_Rating;
end Day3;
