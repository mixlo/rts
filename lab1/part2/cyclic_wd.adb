--Cyclic scheduler with a watchdog: 

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Float_Random;

use Ada.Calendar;
use Ada.Text_IO;
use Ada.Numerics.Float_Random;

-- add packages to use randam number generator


procedure cyclic_wd is
   Message: constant String := "Cyclic scheduler with watchdog";
   -- change/add your declarations here
   Start_Time: Time := Clock;
   Period: Duration := 1.0;
   Next_Time: Time := Start_Time + Period;
   F1_Start: Time;
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
   
   task Watchdog is
      -- add your task entries for communication
      entry Start;
      entry Finish;
   end Watchdog;
   
   task body Watchdog is
      Watch_Start_Time: Time;
      Elapsed: Duration;
      Active: Boolean := False;
      Warning: Boolean := False;
   begin
      loop
	 -- add your task code inside this loop
	 select
	    accept Start do
	       Watch_Start_Time := Clock;
	       Active := True;
	    end;
	 or
	    accept Finish do
	       Elapsed := Clock - Watch_Start_Time;
	       Active := False;
	       if Warning then
		  --Put("f3 elapsed time:");
		  --Put_Line(Duration'Image(Elapsed));
		  Next_Time := Next_Time + Duration(Float'Ceiling(Float(Elapsed)));
		  Warning := False;
	       end if;
	    end;
	 or
	    when Active and then not Warning =>
	       delay 0.5;
	       Put_Line("Warning, f3 running for more than 0.5 seconds");
	       Warning := True;
	 end select;
      end loop;
   end Watchdog;
   
begin
   Reset(G);
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
      
      Watchdog.Start;
      f3;
      Watchdog.Finish;
      
      delay until Next_Time;
      Next_Time := Next_Time + Period;
   end loop;
end cyclic_wd;

