-- AGreatstWeightCalculator: A program for weight related calculations.

-- Copyright (C) 2020-2021 Stavros Filippidis
-- Contact: sfilippidis@gmail.com

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with QAGreatstWeightCalculator.  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Float_Text_IO;
with Ada.IO_Exceptions;

procedure AGreatstWeightCalculator is

   type Gender_Type is (Male, Female);
   gender : Gender_Type;

   type Activity_Type is (No_Exercise, Little_Exercise, Light_Exercise, Moderate_Exercise, Heavy_Exercise, Very_Heavy_Exercise);
   activity : Activity_Type;

   subtype Weight_Type is Float range 30.0..300.0;
   weight : Weight_Type;

   subtype Height_Type is Float range 100.0..270.0;
   height : Height_Type;

   subtype Age_Type is Float range 20.0..110.0;
   age : Age_Type;

   subtype Gender_Choose_Type is Integer range 1..2;
   gender_choose : Gender_Choose_Type;

   subtype Activity_Choose_Type is Integer range 1..6;
   activity_choose : Activity_Choose_Type;

   bmi, idealWeightLow, idealWeightHigh, kcal : Float;
   is_data_ok : Boolean;

begin

   Ada.Text_IO.Put_Line("AGreatstWeightCalculator. Version 1.0.3+. A program for weight related calculations.");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("Copyright (C) 2020-2021 Stavros Filippidis.");
   Ada.Text_IO.Put_Line("email: sfilippidis@gmail.com");
   Ada.Text_IO.Put_Line("www: https://sfil.mysch.gr/");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("AGreatstWeightCalculator is free software: you can redistribute it and/or modify");
   Ada.Text_IO.Put_Line("it under the terms of the GNU General Public License as published by");
   Ada.Text_IO.Put_Line("the Free Software Foundation, either version 3 of the License, or");
   Ada.Text_IO.Put_Line("(at your option) any later version.");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("AGreatstWeightCalculator is distributed in the hope that it will be useful,");
   Ada.Text_IO.Put_Line("but WITHOUT ANY WARRANTY; without even the implied warranty of");
   Ada.Text_IO.Put_Line("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
   Ada.Text_IO.Put_Line("GNU General Public License for more details.");
   Ada.Text_IO.Put_Line("You should have received a copy of the GNU General Public License");
   Ada.Text_IO.Put_Line("along with AGreatstWeightCalculator.  If not, see http://www.gnu.org/licenses/");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("");

   Ada.Text_IO.Put("Please enter your weight in kg (valid range: 30 =< weight =< 300): ");

   is_data_ok := True;

   loop
      begin
         Ada.Float_Text_IO.Get(weight);
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Skip_Line;
            is_data_ok := False;
         when Constraint_Error =>
            is_data_ok := False;
      end;
      exit when is_data_ok;
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("Wrong! Please enter valid value for weight: value should be in the range 30 =< weight =< 300.");
      Ada.Text_IO.Put("Please enter your weight in kg (valid range: 30 =< weight =< 300): ");
      is_data_ok := True;
   end loop;

   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put("Please enter your height in cm (valid range: 100<height<270): ");

   loop
      begin
         Ada.Float_Text_IO.Get(height);
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Skip_Line;
            is_data_ok := False;
         when Constraint_Error =>
            is_data_ok := False;
      end;
      exit when is_data_ok;
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("Wrong! Please enter valid value for height: value should be in the range 100<height<270.");
      Ada.Text_IO.Put("Please enter your height in cm (valid range: 100<height<270): ");
      is_data_ok := True;
   end loop;

   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put("Please enter your age in years (valid range: 20<age<110): ");

   loop
      begin
         Ada.Float_Text_IO.Get(age);
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Skip_Line;
            is_data_ok := False;
         when Constraint_Error =>
            is_data_ok := False;
      end;
      exit when is_data_ok;
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("Wrong! Please enter valid value for age: value should be in the range 20<age<110.");
      Ada.Text_IO.Put("Please enter your age in years (valid range: 20<age<110): ");
      is_data_ok := True;
   end loop;

   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put("Please enter your gender: enter 1 for male or 2 for female: ");

   loop
      begin
         Ada.Integer_Text_IO.Get(gender_choose);
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Skip_Line;
            is_data_ok := False;
         when Constraint_Error =>
            is_data_ok := False;
      end;
      exit when is_data_ok;
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("Wrong! Please enter valid value for age: value should be 1 for male or 2 for female.");
      Ada.Text_IO.Put("Please enter your gender: enter 1 for male or 2 for female: ");
      is_data_ok := True;
   end loop;

   if gender_choose = 1 then
      gender := Male;
   else
      gender := Female;
   end if;

   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("Select your activity level. Use a value from 1 to 6, where:");
   Ada.Text_IO.Put_Line("1 - no exercise");
   Ada.Text_IO.Put_Line("2 - little exercise");
   Ada.Text_IO.Put_Line("3 - light exercise (1-3 days per week)");
   Ada.Text_IO.Put_Line("4 - moderate exercise (3-5 days per week)");
   Ada.Text_IO.Put_Line("5 - heavy exercise (6-7 days per week)");
   Ada.Text_IO.Put_Line("6 - very heavy exercise (twice per day, extra heavy workouts)");
   Ada.Text_IO.Put("Please enter your activity level in the range 1<=level<=6: ");

   loop
      begin
         Ada.Integer_Text_IO.Get(activity_choose);
      exception
         when Ada.Text_IO.Data_Error =>
            Ada.Text_IO.Skip_Line;
            is_data_ok := False;
         when Constraint_Error =>
            is_data_ok := False;
      end;
      exit when is_data_ok;
      Ada.Text_IO.Put_Line("");
      Ada.Text_IO.Put_Line("Wrong! Please enter valid value for activity level: value should be in the range 1<=level<=6.");
      Ada.Text_IO.Put_Line("Select your activity level. Use a value from 1 to 6, where:");
      Ada.Text_IO.Put_Line("1 - no exercise");
      Ada.Text_IO.Put_Line("2 - little exercise");
      Ada.Text_IO.Put_Line("3 - light exercise (1-3 days per week)");
      Ada.Text_IO.Put_Line("4 - moderate exercise (3-5 days per week)");
      Ada.Text_IO.Put_Line("5 - heavy exercise (6-7 days per week)");
      Ada.Text_IO.Put_Line("6 - very heavy exercise (twice per day, extra heavy workouts)");
      Ada.Text_IO.Put("Please enter your activity level in the range 1<=level<=6: ");
      is_data_ok := True;
   end loop;

   case activity_choose is
      when 1 =>
         activity := No_Exercise;
      when 2 =>
         activity := Little_Exercise;
      when 3 =>
         activity := Light_Exercise;
      when 4 =>
         activity := Moderate_Exercise;
      when 5 =>
         activity := Heavy_Exercise;
      when others =>
         activity := Very_Heavy_Exercise;
   end case;

   bmi := weight / ( (height / 100.0) * (height / 100.0) );
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("");
   Ada.Text_IO.Put_Line("Here are your results (approximately):");
   Ada.Text_IO.Put("Your body mass index is ");
   Ada.Float_Text_IO.Put(bmi, Fore => 3, Aft => 2, Exp => 0);
   Ada.Text_IO.Put(", so your standard weight status category is ");

   if bmi < 18.5 then
      Ada.Text_IO.Put("underweight");
   elsif bmi >= 18.5 and bmi < 25.0 then
      Ada.Text_IO.Put("normal");
   elsif bmi >= 25.0 and bmi < 30.0 then
      Ada.Text_IO.Put("overweight");
   else
      Ada.Text_IO.Put("obese");
   end if;

   Ada.Text_IO.Put_Line(".");

   idealWeightLow := 18.50 * (height / 100.0) * (height / 100.0);
   idealWeightHigh := 24.99999 * (height / 100.0) * (height / 100.0);
   Ada.Text_IO.Put("Based on your height, your normal weight range is from ");
   Ada.Float_Text_IO.Put(idealWeightLow, Fore => 3, Aft => 2, Exp => 0);
   Ada.Text_IO.Put(" to ");
   Ada.Float_Text_IO.Put(idealWeightHigh, Fore => 3, Aft => 2, Exp => 0);
   Ada.Text_IO.Put_Line(".");

   if gender = Male then
      kcal := 66.0 + weight * 13.70 + (height / 100.0) * 5.00 * 100.0 - age * 6.80;
   else
      kcal := 655.0 + weight * 9.60 + (height / 100.0) * 1.80 * 100.0 - age * 4.70;
   end if;

   case activity is
      when No_Exercise =>
         kcal := kcal * 1.00;
      when Little_Exercise =>
         kcal := kcal * 1.20;
      when Light_Exercise =>
         kcal := kcal * 1.375;
      when Moderate_Exercise =>
         kcal := kcal * 1.55;
      when Heavy_Exercise =>
         kcal := kcal * 1.725;
      when Very_Heavy_Exercise =>
         kcal := kcal * 1.90;
   end case;

   Ada.Text_IO.Put("Based on the data you entered, to maintain your current weight you need ");
   Ada.Float_Text_IO.Put(kcal, Fore => 3, Aft => 0, Exp => 0);
   Ada.Text_IO.Put_Line(" Calories (kCal) per day.");

end AGreatstWeightCalculator;
