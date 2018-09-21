--Cyclic scheduler with a watchdog: 

with Ada.Calendar;
with Ada.Text_IO;

use Ada.Calendar;
use Ada.Text_IO;

-- add packages to use randam number generator
with Ada.Numerics.Float_Random;
use Ada.Numerics.Float_Random;


procedure cyclic_wd is
   Message: constant String := "Cyclic scheduler with watchdog";
   -- change/add your declarations here
   -- Keep track of start time
   Start_Time: Time := Clock;
   -- Period of task(s) with shortest period
   Period: Duration := 1.0;
   -- Next time for action, from start time 
   Next_Time: Time := Start_Time + Period;
   -- Variable to keep track of start time of f1 for each period
   F1_Start: Time;
   F3_Run_Time: Duration;
   -- Generator for random floats within interval [0.0, 1.0]
   G: Generator;
   
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
      -- add a random delay here
      -- Random(G) generates a random float within interval [0.0, 1.0]
      delay Duration(Random(G));
   end f3;
   
   -- Watchdog accepts calls for
   -- - Start, activates the watchdog, starting a timer for f3 deadline
   -- - Finish, deactivates the watchdog and outputs total elapsed time
   --
   -- If more than 0.5 seconds passes between activation and deactivation, 
   -- the watchdog outputs a warning message indicating that f3 exceeded 
   -- its deadline, but still waits for f3 to finish.
   task Watchdog is
      -- add your task entries for communication
      entry Start;
      entry Finish(Elapsed: out Duration);
   end Watchdog;
   
   task body Watchdog is
      -- Watch_Start_Time, keeps track of start time of activation
      Watch_Start_Time: Time;
      -- Active, keeps track of whether the watchdog is currently active
      Active: Boolean := False;
      -- Warning, keeps track of whether f3 has exceeded its deadline
      Warning: Boolean := False;
   begin
      loop
	 -- add your task code inside this loop
	 select
	    -- Activate watchdog
	    accept Start do
	       -- Save start time
	       Watch_Start_Time := Clock;
	       -- Indicate active
	       Active := True;
	    end;
	 or
	    -- Deactivate watchdog
	    accept Finish(Elapsed: out Duration) do
	       -- Output total elapsed time of f3
	       Elapsed := Clock - Watch_Start_Time;
	       -- Indicate inactive
	       Active := False;
	       -- Reset possible warning
	       Warning := False;
	    end;
	 or
	    -- Watchdog timer should only start when the watchdog is active 
	    -- and when f3 has not yet exceeded its deadline.
	    when Active and then not Warning =>
	       -- f3 shouldn't run for more than 0.5 seconds
	       delay 0.5;
	       -- If no call to Finish is made before the delay is finished, 
	       -- put warning message indicate warning
	       Put_Line("Warning, f3 running for more than 0.5 seconds");
	       Warning := True;
	 end select;
      end loop;
   end Watchdog;
   
begin
   -- Feed the random generator a fresh seed
   Reset(G);
   
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
      
      -- Activate the watchdog, run f3 and deactivate the watchdog
      Watchdog.Start;
      f3;
      Watchdog.Finish(F3_Run_Time);
      
      -- Use the elapsed time of f3 output by the watchdog to calculate if a 
      -- delay of the Next_Time is needed.
      -- Adding 0.5 to the total elapsed time of f3 and rounding that down to 
      -- the closest second will show how much the Next_Time must be delayed, 
      -- to get to the closest whole second after f3 finishes.
      Next_Time := Next_Time + Duration(Float'Floor(Float(F3_Run_Time) + 0.5));
      
      -- Delay until the closest second after f3 finishes and update 
      -- Next_Time variable to next period
      delay until Next_Time;
      Next_Time := Next_Time + Period;
   end loop;
end cyclic_wd;

