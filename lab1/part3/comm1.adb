--Process commnication: Ada lab part 3

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

use Ada.Calendar;
use Ada.Text_IO;

procedure comm1 is
   Message: constant String := "Process communication";
   
   -- Setting queue capacity and creating queue type
   Queue_Capacity: Integer := 10;
   type FIFO is array (0..Queue_Capacity-1) of Integer;
   
   -- Creating type for random integers in range 0..25
   -- Used to generate elements to push into queue
   subtype Element_Range is Integer range 0..25;
   package Rand_Element is new Ada.Numerics.Discrete_Random(Element_Range);
   
   -- Creating generator objects
   Element_Gen: Rand_Element.Generator;
   Delay_Gen: Ada.Numerics.Float_Random.Generator;
   
   -- The buffer queue is implemented as a cyclic array, keeping track of the 
   -- indices for the head and tail of the queue. 
   -- - When a new element is pushed, the tail index is incremented by 1. 
   --   If the tail index points to the end of the array, the tail cycles back 
   --   to the start of the array, using the modulo operation.
   -- - When an element is popped, the head index is incremented by 1, 
   --   essentially "freeing"/"unallocating" that position in the queue.
   --   If the head index ṕoints to the end of the array, the head cycles back 
   --   to the start of the array, using the modulo operation.
   --   
   -- Buffer will accept calls for 
   -- - Push when Queue_Length < Queue_Capacity
   -- - Pop when Queue_Length > 0
   -- When Push call causes Queue_Length = Queue_Capacity, producer is blocked
   -- When Pop call causes Queue_Length = 0, consumer is blocked
   -- 
   -- Producer and consumer will continue pushing and popping until they are 
   -- blocked by guards enforcing the above conditions. Producer will be 
   -- unblocked as soon as the length goes a step below full capacity, and 
   -- consumer will be unblocked as soon as the length goes a step above 0.
   --
   -- Buffer will also accept a Stop call.
   -- When the sum reaches above 100, the consumer will tell the buffer to 
   -- stop, waiting for the buffer to finish before stopping itself. The 
   -- buffer in turn will send a stop-signal to the producer, waiting for 
   -- the producer to stop before stopping itself.
   --
   -- This is what causes a potential deadlock.
   -- Let's say that the consumer pops an value that will cause the 
   -- sum to reach above 100, but before it calculates the sum, the 
   -- producer quickly pushes elements until it gets blocked by the 
   -- guard at the "accept Push" alternative in the buffer. 
   -- Then, when the consumer calculates the sum that reaches above 
   -- 100, it will tell the buffer to finish, which in turn will tell 
   -- the producer to finish. 
   -- So the producer waits for the buffer to accept the push and the 
   -- buffer waits for the producer to accept the stop-signal, hence 
   -- we have a deadlock.
   task buffer is
      -- add your task entries for communication 
      entry Init;
      entry Push(Element: in Integer);
      entry Pop(Element: out Integer);
      entry Stop;
   end buffer;
   
   -- Producer accepts calls for
   -- - Init, used to wait for the random generators to get a seed
   -- - Stop, used to stop the producer task (will be called from the buffer)
   task producer is
      -- add your task entries for communication
      entry Init;
      entry Stop;
   end producer;
   
   -- Consumer accepts call for
   -- - Init, used to wait for the random generators to get a seed
   task consumer is
      -- add your task entries for communication 
      entry Init;
   end consumer;

   task body buffer is 
      Message: constant String := "buffer executing";
      -- change/add your local declarations here   
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
   begin
      -- Wait for random generators to be reset
      accept Init;
      
      Put_Line(Message);
      loop
	 -- add your task code inside this loop    
	 select
	    -- Perform the push operation, when queue not full
	    when Queue_Length < Queue_Capacity =>
	       accept Push(Element: in Integer) do
		  -- Put the new element at end of queue
		  Queue(Tail) := Element;
		  -- Set new position of end of queue, in a cyclic array manner
		  Tail := (Tail + 1) mod Queue_Capacity;
		  -- Increment queue length variable
		  Queue_Length := Queue_Length + 1;
	       end;
	 or
	    -- Perform the pop operation, when queue not empty
	    when Queue_Length > 0 =>
	       accept Pop(Element: out Integer) do
		  -- Output/pop the element at the start of the queue
		  Element := Queue(Head);
		  -- Set the second-in-line element to the new head of the queue,
		  -- in a cyclic array manner
		  Head := (Head + 1) mod Queue_Capacity;
		  -- Increment queue length variable
		  Queue_Length := Queue_Length - 1;
	       end;
	 or
	    -- Signal to stop comes from the consumer. The buffer will then first 
	    -- send a stop signal to the producer, waiting for it to stop, then 
	    -- it will stop itself.
	    accept Stop do
	       -- Here there's potential for a deadlock (explanation above).
	       Put_Line("Buffer: Received signal to stop, stopping producer first.");
	       producer.Stop;
	       Should_Stop := True;
	    end;
	 end select;
	 
	 -- When stop variable is set, exit loop.
	 exit when Should_Stop;
      end loop;
      
      Put_Line("Buffer: Producer stopped. Stopping.");
   end buffer;

   task body producer is 
      Message: constant String := "producer executing";
      -- change/add your local declarations here
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
	 select 
	    -- Should exit loop and stop task when Stop is called
	    -- (will be called from buffer task)
	    accept Stop do
	       Should_Stop := True;
	    end;
	 else
	    -- Get a random value in range 0..25
	    Value := Rand_Element.Random(Element_Gen);
	    
	    -- Push the value into the queue.
	    -- Here there's potential for a deadlock (explanation above).
	    Put_Line("Producer: Pushing value:" & Integer'Image(Value));
	    buffer.Push(Value);
	    Put_Line("Producer: Finished pushing");
	    
	    -- Delay a random time
	    -- Float_Random.Random will generate value in range [0.0, 1.0].
	    -- Can be multiplied by float scalar to increase max range value.
	    -- NOTE: Increasing the scalar will give the consumer longer time 
	    -- to pop elements, increasing the probability of the consumer 
	    -- getting blocked by the guard at the "accept Pop" alternative 
	    -- in the buffer, which in turn will decrease the probability of a 
	    -- deadlock.
	    delay Duration(Ada.Numerics.Float_Random.Random(Delay_Gen) * 1.0);
	 end select;
	 
	 -- When stop variable is set, exit loop.
	 exit when Should_Stop;
      end loop;
      
      Put_Line("Producer: Received signal to stop, stopping.");
   end producer;

   task body consumer is 
      Message: constant String := "consumer executing";
      -- change/add your local declarations here
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
	 -- in the buffer, which in turn will increase the probability of a 
	 -- deadlock.
	 delay Duration(Ada.Numerics.Float_Random.Random(Delay_Gen) * 1.0);
	 
	 -- Exit the loop when sum reaches above 100
	 exit Main_Cycle when Sum > 100;
      end loop Main_Cycle; 

      -- add your code to stop executions of other tasks
      -- When consumer is finished, print the total sum and signal the buffer 
      -- task to stop. Waits for buffer task to stop and then stops consumer task.
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
   
   -- Reset random generators with fresh seed
   Rand_Element.Reset(Element_Gen);
   Ada.Numerics.Float_Random.Reset(Delay_Gen);
   
   -- Start producer and consumer when random generators are reset.
   buffer.Init;
   producer.Init;
   consumer.Init;
end comm1;
