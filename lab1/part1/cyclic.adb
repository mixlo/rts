with Ada.Calendar;
with Ada.Text_IO;
use Ada.Calendar;
use Ada.Text_IO;

procedure cyclic is
   Message: constant String := "Cyclic scheduler";
   -- change/add your declarations here
   -- Keep track of start time
   Start_Time: Time := Clock;
   -- Period of task(s) with shortest period
   Period: Duration := 1.0;
   -- Next time for action, from start time 
   Next_Time: Time := Start_Time + Period;
   -- Variable to keep track of start time of f1 for each period
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
   -- One iteration of the loop contains the execution of all procedures 
   -- such that the procedure with the longest period is executed once.
   -- By using "delay until" for each period, we make sure not to let 
   -- any jitter stack up and cause a drift.
   loop
      -- change/add your code inside this loop
      -- Execute f1 and f2 in sequence
      f1;
      f2;
      
      -- Delay a period 
      delay until Next_Time;
      Next_Time := Next_Time + Period;
      
      -- Run f1 and f2 in sequence again, but save f1's start time
      F1_Start := Clock;
      f1;
      f2;
      
      -- Delay f3 start until exactly a half second after f1 start.
      -- Even though we know f1 and f2 won't execute for more than a half 
      -- second, this is not the same as executing f3 a half second after 
      -- Next_Time. Next_Time keeps track of (almost) exact seconds after 
      -- start, while F1_Start is set to exact time of f1's start and takes 
      -- jitter into account.
      delay until F1_Start + 0.5; 
      f3;
      
      -- Finally, delay until next period. We know that f1 and f2 executes 
      -- for less than 0.5 seconds and that f3 won't execute for more than 
      -- 0.5 seconds, meaning we don't have to worry about f3 exceeding 
      -- the time for the next period.
      delay until Next_Time;
      Next_Time := Next_Time + Period;
   end loop;
end cyclic;

