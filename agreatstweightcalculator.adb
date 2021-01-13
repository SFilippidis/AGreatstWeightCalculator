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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.IO_Exceptions;

procedure AGreatstWeightCalculator is

   type Gender_Type is (Male, Female);
   type Activity_Type is (No_Exercise, Little_Exercise, Light_Exercise, Moderate_Exercise, Heavy_Exercise, Very_Heavy_Exercise);

   weight, height, age, bmi, idealWeightLow, idealWeightHigh, kcal : Float := -1.0;
   gender_choose, activity_choose : Integer := -1;
   gender : Gender_Type;
   activity : Activity_Type;

begin

   Put_Line("AGreatstWeightCalculator. Version 1.0. A program for weight related calculations.");
   Put_Line("");
   Put_Line("Copyright (C) 2020-2021 Stavros Filippidis.");
   Put_Line("email: sfilippidis@gmail.com");
   Put_Line("www: https://sfil.mysch.gr/");
   Put_Line("");
   Put_Line("");
   Put_Line("AGreatstWeightCalculator is free software: you can redistribute it and/or modify");
   Put_Line("it under the terms of the GNU General Public License as published by");
   Put_Line("the Free Software Foundation, either version 3 of the License, or");
   Put_Line("(at your option) any later version.");
   Put_Line("");
   Put_Line("");
   Put_Line("AGreatstWeightCalculator is distributed in the hope that it will be useful,");
   Put_Line("but WITHOUT ANY WARRANTY; without even the implied warranty of");
   Put_Line("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the");
   Put_Line("GNU General Public License for more details.");
   Put_Line("You should have received a copy of the GNU General Public License");
   Put_Line("along with AGreatstWeightCalculator.  If not, see http://www.gnu.org/licenses/");
   Put_Line("");
   Put_Line("");

   Put("Please enter your weight in kg (valid range: 30<weight<300): ");

   loop
      begin
         Get(weight);
      exception
         when Data_Error =>
            Skip_Line;
            weight := -1.0;
      end;
      exit when weight > 30.0 and weight < 300.0;
      Put_Line("");
      Put_Line("Wrong! Please enter valid value for weight: value should be in the range 30<weight<300.");
      Put("Please enter your weight in kg (valid range: 30<weight<300): ");
   end loop;

   Put_Line("");
   Put("Please enter your height in cm (valid range: 100<height<270): ");

   loop
      begin
         Get(height);
      exception
         when Data_Error =>
            Skip_Line;
            height := -1.0;
      end;
      exit when height > 100.0 and height < 270.0;
      Put_Line("");
      Put_Line("Wrong! Please enter valid value for height: value should be in the range 100<height<270.");
      Put("Please enter your height in cm (valid range: 100<height<270): ");
   end loop;

   Put_Line("");
   Put("Please enter your age in years (valid range: 20<age<110): ");

   loop
      begin
         Get(age);
      exception
         when Data_Error =>
            Skip_Line;
            age := -1.0;
      end;
      exit when age > 20.0 and age < 110.0;
      Put_Line("");
      Put_Line("Wrong! Please enter valid value for age: value should be in the range 20<age<110.");
      Put("Please enter your age in years (valid range: 20<age<110): ");
   end loop;

   Put_Line("");
   Put("Please enter your gender: enter 1 for male or 2 for female: ");

   loop
      begin
         Get(gender_choose);
      exception
         when Data_Error =>
            Skip_Line;
            gender_choose := -1;
      end;
      exit when gender_choose in 1..2;
      Put_Line("");
      Put_Line("Wrong! Please enter valid value for age: value should be 1 for male or 2 for female.");
      Put("Please enter your gender: enter 1 for male or 2 for female: ");
   end loop;

   if gender_choose = 1 then
      gender := Male;
   else
      gender := Female;
   end if;

   Put_Line("");
   Put_Line("Select your activity level. Use a value from 1 to 6, where:");
   Put_Line("1 - no exercise");
   Put_Line("2 - little exercise");
   Put_Line("3 - light exercise (1-3 days per week)");
   Put_Line("4 - moderate exercise (3-5 days per week)");
   Put_Line("5 - heavy exercise (6-7 days per week)");
   Put_Line("6 - very heavy exercise (twice per day, extra heavy workouts)");
   Put("Please enter your activity level in the range 1<=level<=6: ");

   loop
      begin
         Get(activity_choose);
      exception
         when Data_Error =>
            Skip_Line;
            activity_choose := -1;
      end;
      exit when activity_choose in 1..6;
      Put_Line("");
      Put_Line("Wrong! Please enter valid value for activity level: value should be in the range 1<=level<=6.");
      Put_Line("Select your activity level. Use a value from 1 to 6, where:");
      Put_Line("1 - no exercise");
      Put_Line("2 - little exercise");
      Put_Line("3 - light exercise (1-3 days per week)");
      Put_Line("4 - moderate exercise (3-5 days per week)");
      Put_Line("5 - heavy exercise (6-7 days per week)");
      Put_Line("6 - very heavy exercise (twice per day, extra heavy workouts)");
      Put("Please enter your activity level in the range 1<=level<=6: ");
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

   height := height / 100.0;

   bmi := weight/(height*height);
   Put_Line("");
   Put_Line("");
   Put_Line("Here are your results (approximately):");
   Put("Your body mass index is ");
   Put(bmi, Fore => 3, Aft => 2, Exp => 0);
   Put(", so your standard weight status category is ");

   if bmi < 18.5 then
      Put("underweight");
   elsif bmi >= 18.5 and bmi < 25.0 then
      Put("normal");
   elsif bmi >= 25.0 and bmi < 30.0 then
      Put("overweight");
   else
      Put("obese");
   end if;

   Put_Line(".");

   idealWeightLow := 18.50 * height * height;
   idealWeightHigh := 24.99999 * height * height;
   Put("Based on your height, your normal weight range is from ");
   Put(idealWeightLow, Fore => 3, Aft => 2, Exp => 0);
   Put(" to ");
   Put(idealWeightHigh, Fore => 3, Aft => 2, Exp => 0);
   Put_Line(".");

   kcal := 0.0;

   if gender = Male then
      kcal := kcal + 66.0 + weight * 13.70 + height * 5.00 * 100.0 - age * 6.80;
   else
      kcal := kcal + 655.0 + weight * 9.60 + height * 1.80 * 100.0 - age * 4.70;
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

   Put("Based on the data you entered, to maintain your current weight you need ");
   Put(kcal, Fore => 3, Aft => 0, Exp => 0);
   Put_Line(" Calories (kCal) per day.");

end AGreatstWeightCalculator;
