--Protected types: Ada lab part 4

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

use Ada.Calendar;
use Ada.Text_IO;

procedure comm2 is
   Message: constant String := "Protected Object";
   
   -- Creating queue type with queue capacity 10
   -- Can't declare Queue_Capacity variable globally when using protected object
   type FIFO is array (0..9) of Integer;
   
   -- Creating type for random integers in range 0..25
   -- Used to generate elements to push into queue
   subtype Element_Range is Integer range 0..25;
   package Rand_Element is new Ada.Numerics.Discrete_Random(Element_Range);
   
   -- Creating generator objects
   Element_Gen: Rand_Element.Generator;
   Delay_Gen: Ada.Numerics.Float_Random.Generator;
   
   -- protected object declaration
   -- The buffer works pretty much the same as in Part 3, except that it 
   -- doesn't need an init call since it's not a task anymore.
   -- The Push entry guard will now block producer when trying to push above 
   -- queue capacity, but will accept when the Should_Stop variable is set. 
   -- This makes it possible for the consumer to call buffer.Stop to set the 
   -- Should_Stop variable to True, which will unblock the potentially blocked 
   -- producer, avoiding the potential deadlock displayed in Part 3.
   --
   -- NOTE: We realise that this solution could be used in to avoid the 
   -- deadlock in Part 3 as well. However, we wanted to display both a 
   -- concrete scenario of a deadlock, and a way to avoid it. It seems like 
   -- the assignment strongly suggests that we should display another easy 
   -- way of causing a deadlock in Part 3, and then use a method/construct only 
   -- avaiable for protected objects to avoid the same kind of deadlock in 
   -- Part 4. We feel unfortunate not to be able to come up with such a 
   -- scenario, but this is the best we could do instead.
   protected  buffer is
      -- add entries of protected object here 
      entry Push(Element: in Integer; Stop: out Boolean);
      entry Pop(Element: out Integer);
      entry Stop;
   private
      -- add local declarations
      -- Queue_Capacity, can't be global in this case
      Queue_Capacity: Integer := 10;
      -- Queue, the FIFO queue implemented as an array of integers
      Queue: FIFO;
      -- Head, keeping track of the first position of the queue, 
      -- from which the next element should be popped
      Head: Integer := 0;
      -- Tail, keeping track of the end of the queue, to which new 
      -- elements should be pushed
      Tail: Integer := 0;
      -- Queue_Length, keeping track of the length of queue, used 
      -- by push and pop operations to make sure number of elements 
      -- stay in interval 0..Queue_Capacity
      Queue_Length: Integer := 0;
      -- Should_Stop, checked after select in loop. Buffer task will 
      -- finish when set to true
      Should_Stop: Boolean := False;
   end buffer;
   
   -- Producer works the same as in Part 3, except that it doesn't need a 
   -- Stop entry, since it will check that Stop output variable of the 
   -- buffer.Push call.
   task producer is
      -- add task entries
      entry Init;
   end producer;
   
   -- The consumer works pretty much the same as in Part 3.
   task consumer is
      -- add task entries
      entry Init;
   end consumer;
   
   protected body buffer is 
      -- add definitions of protected entries here 
      -- Accept the push operation, when queue not full or when signal to Stop
      entry Push(Element: in Integer; Stop: out Boolean) when 
	Queue_Length < Queue_Capacity or else Should_Stop is
      begin
	 if Queue_Length < Queue_Capacity then
	    -- Put the new element at end of queue
	    Queue(Tail) := Element;
	    -- Set new position of end of queue, in a cyclic array manner
	    Tail := (Tail + 1) mod Queue_Capacity;
	    -- Increment queue length variable
	    Queue_Length := Queue_Length + 1;
	 end if;
	 Stop := Should_Stop;
      end Push;
      
      -- Perform the pop operation, when queue not empty
      entry Pop(Element: out Integer) when
	Queue_Length > 0 is
      begin
	 -- Output/pop the element at the start of the queue
	 Element := Queue(Head);
	 -- Set the second-in-line element to the new head of the queue,
	 -- in a cyclic array manner
	 Head := (Head + 1) mod Queue_Capacity;
	 -- Increment queue length variable
	 Queue_Length := Queue_Length - 1;
      end Pop;
      
      -- Signal to stop comes from the consumer. The consumer will then finish 
      -- immediately after, not waiting for the producer to finish. The 
      -- producer will eventually read the Should_Stop variable when trying to 
      -- Push and then also stop. The buffer is now a protected object and not 
      -- a task, and is therefore nothing to stop.
      entry Stop when
	 True is
      begin
	 Should_Stop := True;
      end;
   end buffer;
   
   task body producer is 
      Message: constant String := "Producer: Executing";
      -- add local declrations of task here  
      -- Value, generated random element to push
      Value: Integer;
      -- Should_Stop, checked after select in loop. Producer task will 
      -- finish when set to true
      Should_Stop: Boolean := False;
   begin
      -- Wait for random generators to be reset
      accept Init;
      
      Put_Line(Message);
      loop
	 -- add your task code inside this loop
	 -- Get a random value in range 0..25
	 Value := Rand_Element.Random(Element_Gen);
	 
	 -- Push the value into the queue.
	 -- Here the potential for a deadlock in Part 3 is avoided using the 
	 -- Should_Stop variable.
	 Put_Line("Producer: Pushing value:" & Integer'Image(Value));
	 buffer.Push(Value, Should_Stop);
	 
	 -- Exit loop when buffer.Push output Should_Stop = True
	 exit when Should_Stop;
	 
	 Put_Line("Producer: Finished pushing");
	 
	 -- Delay a random time
	 -- Float_Random.Random will generate value in range [0.0, 1.0].
	 -- Can be multiplied by float scalar to increase max range value.
	 -- NOTE: Increasing the scalar will give the consumer longer time 
	 -- to pop elements, increasing the probability of the consumer 
	 -- getting blocked by the guard at the "accept Pop" alternative 
	 -- in the buffer.
	 delay Duration(Ada.Numerics.Float_Random.Random(Delay_Gen) * 1.0);
      end loop;
      
      Put_Line("Producer: Received signal to stop, stopping.");
   end producer;
   
   task body consumer is 
      Message: constant String := "Consumer: Executing";
      -- add local declrations of task here 
      -- Value, keeping track of popped value
      Value: Integer;
      -- Sum, keeping track of total sum, to know when to finish
      Sum: Integer := 0;
   begin
      -- Wait for random generators to be reset
      accept Init;
      
      Put_Line(Message);
  Main_Cycle:
      loop 
	 -- add your task code inside this loop
	 -- Pop a value from the queue
	 Put_Line("Consumer: Popping...");
	 buffer.Pop(Value);
	 
	 -- Increment sum
	 Sum := Sum + Value;
	 Put_Line("Consumer: Popped value:" & Integer'Image(Value) & ", Sum:" & Integer'Image(Sum));
	 
	 -- Delay a random time
	 -- Float_Random.Random will generate value in range [0.0, 1.0].
	 -- Can be multiplied by float scalar to increase max range value.
	 -- NOTE: Increasing the scalar will give the producer longer time 
	 -- to push elements, increasing the probability of the producer 
	 -- getting blocked by the guard at the "accept Push" alternative 
	 -- in the buffer.
	 delay Duration(Ada.Numerics.Float_Random.Random(Delay_Gen) * 1.0);
	 
	 -- Exit the loop when sum reaches above 100
	 exit Main_Cycle when Sum > 100;
      end loop Main_Cycle; 
      
      -- add your code to stop executions of other tasks     
      --Put_Line("Ending the consumer");
      -- When consumer is finished, print the total sum and signal buffer that 
      -- producer should be stopped, then stop consumer without waiting for 
      -- producer to stop.
      Put_Line("Consumer: Total sum:" & Integer'Image(Sum) & ".");
      Put_Line("Consumer: Signaling buffer to stop producer, then stopping.");
      buffer.Stop;
   end consumer;
   
begin
   Put_Line(Message);
   
   -- Reset random generators with fresh seed
   Rand_Element.Reset(Element_Gen);
   Ada.Numerics.Float_Random.Reset(Delay_Gen);
   
   -- Start producer and consumer when random generators are reset.
   producer.Init;
   consumer.Init;
end comm2;
