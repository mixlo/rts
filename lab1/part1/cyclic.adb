with Ada.Calendar;
with Ada.Text_IO;
use Ada.Calendar;
use Ada.Text_IO;

procedure cyclic is
   Message: constant String := "Cyclic scheduler";
   -- change/add your declarations here
   Start_Time: Time := Clock;
   Period: Duration := 1.0;
   Next_Time: Time := Start_Time + Period;
   F1_Start: Time;
   
   procedure f1 is 
      Message: constant String := "f1 executing, time is now";
   begin
      Put(Message);
      Put_Line(Duration'Image(Clock - Start_Time));
   end f1;
   
   procedure f2 is 
      Message: constant String := "f2 executing, time is now";
   begin
      Put(Message);
      Put_Line(Duration'Image(Clock - Start_Time));
   end f2;
   
   procedure f3 is 
      Message: constant String := "f3 executing, time is now";
   begin
      Put(Message);
      Put_Line(Duration'Image(Clock - Start_Time));
      New_Line;
   end f3;
   
begin
   loop
      -- change/add your code inside this loop
      f1;
      f2;
      
      delay until Next_Time;
      Next_Time := Next_Time + Period;
      
      F1_Start := Clock;
      f1;
      f2;
      delay until F1_Start + 0.5; 
      f3;
      
      delay until Next_Time;
      Next_Time := Next_Time + Period;
   end loop;
end cyclic;

