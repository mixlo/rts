--Process commnication: Ada lab part 3

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

use Ada.Calendar;
use Ada.Text_IO;

procedure comm1 is
   subtype Element_Range is Integer range 0..25;
   package Rand_Element is new Ada.Numerics.Discrete_Random(Element_Range);
   
   Message: constant String := "Process communication";
   Element_Gen: Rand_Element.Generator;
   Delay_Gen: Ada.Numerics.Float_Random.Generator;
   
   task buffer is
      -- add your task entries for communication
      entry Push(Element: in Integer; Blocked: out Boolean);
      entry Pop(Element: out Integer; Blocked: out Boolean);
      entry Stop;
   end buffer;
   
   task producer is
      -- add your task entries for communication
      entry Stop;
   end producer;
   
   task consumer is
      -- add your task entries for communication 
   end consumer;
   
   task body buffer is 
      Message: constant String := "Buffer: Executing";
      -- change/add your local declarations here   
      Queue_Capacity: Integer := 10;
      type FIFO is array (0..Queue_Capacity-1) of Integer;
      Queue: FIFO;
      Head: Integer := 0;
      Tail: Integer := 0;
      Queue_Length: Integer := 0;
      Should_Stop: Boolean := False;
   begin
      Put_Line(Message);
      loop
	 -- add your task code inside this loop
	 select 
	    accept Push(Element: in Integer; Blocked: out Boolean) do
	       if Queue_Length = Queue_Capacity then
		  Blocked := True;
	       else
		  Queue(Tail) := Element;
		  Tail := (Tail + 1) mod Queue_Capacity;
		  Queue_Length := Queue_Length + 1;
		  Blocked := False;
	       end if;
	    end;
	 or
	    accept Pop(Element: out Integer; Blocked: out Boolean) do
	       if Queue_Length = 0 then
		  Element := -1;
		  Blocked := True;
	       else
		  Element := Queue(Head);
		  Head := (Head + 1) mod Queue_Capacity;
		  Queue_Length := Queue_Length - 1;
		  Blocked := False;
	       end if;
	    end;
	 or
	    accept Stop do
	       Put_Line("Buffer: Received signal to stop, stopping producer first.");
	       Producer.Stop;
	       Should_Stop := True;
	    end;
	 end select;
	 
	 exit when Should_Stop;
      end loop;
      
      Put_Line("Buffer: Producer stopped. Stopping.");
   end buffer;
   
   task body producer is 
      Message: constant String := "Producer: Executing";
      -- change/add your local declarations here
      Value: Integer;
      Blocked: Boolean := False;
      Should_Stop: Boolean := False;
   begin
      -- Wait a bit for random generators to be reset
      delay 0.5;
      Value := Rand_Element.Random(Element_Gen);
      Put_Line(Message);
      loop
	 -- add your task code inside this loop
	 select 
	    accept Stop do
	       Should_Stop := True;
	    end;
	 else
	    Put_Line("Producer: Pushing...");
	    buffer.Push(Value, Blocked);
	 
	    if not Blocked then
	       Put_Line("Producer: Pushed value:" & Integer'Image(Value));
	       Value := Rand_Element.Random(Element_Gen);
	    else
	       Put_Line("Producer: Blocked, capacity reached");
	    end if;
	    
	    delay Duration(Ada.Numerics.Float_Random.Random(Delay_Gen) * 2.0);
	 end select;
	 
	 exit when Should_Stop;
      end loop;
      
      Put_Line("Producer: Received signal to stop, stopping.");
   end producer;
   
   task body consumer is 
      Message: constant String := "Consumer: Executing";
      -- change/add your local declarations here
      Value: Integer;
      Blocked: Boolean := False;
      Sum: Integer := 0;
   begin
      -- Wait a bit for random generators to reset
      delay 0.5;
      Put_Line(Message);
  Main_Cycle:
      loop 
	 -- add your task code inside this loop 
	 Put_Line("Consumer: Popping...");
	 buffer.Pop(Value, Blocked);
	 
	 if not Blocked then
	    Sum := Sum + Value;
	    Put_Line("Consumer: Popped value:" & Integer'Image(Value) & ", Sum:" & Integer'Image(Sum));
	    exit Main_Cycle when Sum > 100;
	 else
	    Put_Line("Consumer: Blocked, queue empty");
	 end if;
	 
	 delay Duration(Ada.Numerics.Float_Random.Random(Delay_Gen) * 5.0);
      end loop Main_Cycle;
      
      -- add your code to stop executions of other tasks
      Put_Line("Consumer: Total sum:" & Integer'Image(Sum) & ". Stopping buffer.");
      buffer.Stop;
      Put_Line("Consumer: Buffer stopped. Stopping.");
   exception
      when TASKING_ERROR =>
	 Put_Line("Buffer finished before producer");
	 Put_Line("Ending the consumer");
   end consumer;
begin
   Put_Line(Message);
   
   Rand_Element.Reset(Element_Gen);
   Ada.Numerics.Float_Random.Reset(Delay_Gen);
end comm1;
