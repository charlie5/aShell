with
     Shell.Commands.Unsafe,
     Ada.Calendar,
     Ada.Text_IO,
     Interfaces.C,
     System;


procedure Test_Unsafe_Wait_On
--
-- 'Wait_On' must read the output of a command, or a program which fills its output pipe would never end.
-- Any failure raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Unsafe;


   procedure Check (Test : in String;
                    Ok   : in Boolean)
   is
   begin
      if not Ok
      then
         raise Program_Error with Test & " failed.";
      end if;

      Ada.Text_IO.Put_Line (Test & " passed.");
   end Check;


   function Times_This_Task_Has_Slept return Natural
   is
      use Ada.Text_IO;

      Label  : constant String := "voluntary_ctxt_switches:";
      Status : File_Type;
      Result : Natural := 0;
   begin
      Open (Status, In_File, "/proc/thread-self/status");

      while not End_Of_File (Status)
      loop
         declare
            Line : constant String := Get_Line (Status);
         begin
            if         Line'Length > Label'Length
              and then Line (Line'First .. Line'First + Label'Length - 1) = Label
            then
               Result := Natural'Value (Line (Line'First + Label'Length + 1 .. Line'Last));     -- After the tab.
            end if;
         end;
      end loop;

      Close (Status);
      return Result;
   end Times_This_Task_Has_Slept;


   -- Once a program has closed its pipes, only its end is left to wait for, so a wait should rarely check. It checks every
   -- 2 ms until then, so this leaves room for a busy machine to take a while to get the program to close its pipes.
   --
   Most_Wake_Ups : constant := 150;

begin
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("bash closes_output.sh");
   begin
      The_Command.Start;

      declare
         Before : constant Natural := Times_This_Task_Has_Slept;
      begin
         The_Command.Wait_On;

         Ada.Text_IO.Put_Line ("Wake ups in 'Wait_On':" & Natural'Image (Times_This_Task_Has_Slept - Before));
         Check ("'Wait_On' sleeps once a program has closed its pipes", Times_This_Task_Has_Slept - Before < Most_Wake_Ups);
         Check ("The program which closed its pipes ends normally",     The_Command.Normal_Exit);
      end;
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash closes_output.sh | bash closes_output.sh");
      Before       : constant Natural := Times_This_Task_Has_Slept;
   begin
      Run (The_Pipeline);

      Ada.Text_IO.Put_Line ("Wake ups in pipeline 'Run':" & Natural'Image (Times_This_Task_Has_Slept - Before));
      Check ("Pipeline 'Run' sleeps once its programs have closed their pipes", Times_This_Task_Has_Slept - Before < Most_Wake_Ups);
      Check ("The pipeline which closed its pipes didn't fail",                 Which_Failed (The_Pipeline) = 0);
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash closes_output.sh | bash closes_output.sh");
   begin
      Start (The_Pipeline);

      declare
         Before : constant Natural := Times_This_Task_Has_Slept;
      begin
         The_Pipeline (2).Wait_On;
         The_Pipeline (1).Wait_On;

         Ada.Text_IO.Put_Line ("Wake ups waiting on a pipeline:" & Natural'Image (Times_This_Task_Has_Slept - Before));
         Check ("Waiting on a pipeline sleeps once its programs have closed their pipes", Times_This_Task_Has_Slept - Before < Most_Wake_Ups);
         Check ("Each command of that pipeline ends normally", The_Pipeline (1).Normal_Exit and The_Pipeline (2).Normal_Exit);
      end;
   end;

   -- A program which closes its pipes, stops, and is continued from outside, is still waited on until it ends.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("bash stops_after_closing.sh");
   begin
      The_Command.Run;

      Check ("A stopped program which closed its pipes is waited on", The_Command.Normal_Exit);
   end;

   -- A wait must remain open to being given up on, including once a program has closed its pipes.
   --
   declare
      use Ada.Calendar;

      The_Command : Unsafe.Command := Forge.To_Command ("bash closes_output.sh 10");
      The_Start   : constant Time  := Clock;
   begin
      The_Command.Start;

      select
         delay 0.3;
      then abort
         The_Command.Wait_On;
      end select;

      Ada.Text_IO.Put_Line ("Took" & Duration'Image (Clock - The_Start) & " to give up on a wait.");
      Check ("A wait can be aborted, once a program has closed its pipes", Clock - The_Start < 5.0);

      The_Command.Kill;
      The_Command.Wait_On;
   end;

   declare
      use Ada.Calendar;

      The_Pipeline : Command_Array := Forge.To_Commands ("bash closes_output.sh 10 | bash closes_output.sh 10");
      The_Start    : constant Time := Clock;
   begin
      select
         delay 0.3;
      then abort
         Run (The_Pipeline);
      end select;

      Check ("A pipeline 'Run' can be aborted, once its programs have closed their pipes", Clock - The_Start < 5.0);

      for Each of The_Pipeline
      loop
         Each.Kill;
         Each.Wait_On;
      end loop;
   end;

   -- A signal must not end a wait, which also leaves the program running. Most signals are blocked in an Ada
   -- program's main thread, but not an interrupt, and this handler doesn't ask for the system call to be restarted.
   --
   declare
      use Interfaces.C;

      SIGINT : constant int := 2;

      Handled : int := 0
        with Volatile;

      procedure Handler (Signal : in int)
        with Convention => C;

      procedure Handler (Signal : in int)
      is
         pragma Unreferenced (Signal);
      begin
         Handled := Handled + 1;
      end Handler;

      function Set_Handler (Signal  : in int;
                            Handler : in System.Address) return System.Address
        with Import, Convention => C, External_Name => "signal";

      function Allow_Interruption (Signal : in int;
                                   Flag   : in int) return int
        with Import, Convention => C, External_Name => "siginterrupt";

      The_Command : Unsafe.Command := Forge.To_Command ("bash interrupts_parent.sh");

      Old_Handler : constant System.Address := Set_Handler (SIGINT, Handler'Address);
      Unused      : constant int            := Allow_Interruption (SIGINT, 1);
   begin
      The_Command.Run;

      declare
         Ignored : constant System.Address := Set_Handler (SIGINT, Old_Handler);
         pragma Unreferenced (Ignored);
      begin
         Check ("The interrupt reached this program",       Handled = 1);
         Check ("A wait carries on after a signal",         The_Command.Normal_Exit);
      end;
   end;


   -- Large output.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("head -c 3000000 /dev/zero");
   begin
      The_Command.Start;
      The_Command.Wait_On;

      Check ("Command with large output ends normally", The_Command.Normal_Exit);
      Check ("All of the large output is kept",         Output_Of (Results_Of (The_Command))'Length = 3_000_000);
   end;


   -- Large error output.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("bash large_errors.sh");
   begin
      The_Command.Start;
      The_Command.Wait_On;

      Check ("All of the large error output is kept", Errors_Of (Results_Of (The_Command))'Length = 300_000);
   end;


   -- A command which fails.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("ls /non_existent_file");
   begin
      The_Command.Start;
      The_Command.Wait_On;

      Check ("Failed command is reported", The_Command.Failed);
      Check ("Its error output is kept",   Errors_Of (Results_Of (The_Command))'Length > 0);
   end;


   -- Waiting again.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("echo Hello");
   begin
      The_Command.Start;
      The_Command.Wait_On;
      The_Command.Wait_On;

      Check ("Waiting twice keeps the output", +Output_Of (Results_Of (The_Command)) = "Hello" & ASCII.LF);
   end;


   -- A program with no output.
   --
   declare
      use Ada.Calendar;

      The_Start   : constant Time := Clock;
      The_Command : Unsafe.Command := Forge.To_Command ("sleep 1");
   begin
      The_Command.Start;
      The_Command.Wait_On;

      Check ("Waits until the program ends", Clock - The_Start >= 1.0);
      Check ("Sees the end promptly",        Clock - The_Start <  10.0);
   end;


   -- A command which hasn't started.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("true");
   begin
      The_Command.Wait_On;
      Check ("Waiting on an unstarted command raises", False);
   exception
      when Command_Error =>
         Check ("Waiting on an unstarted command raises", True);
   end;


   -- A killed command.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("sleep 30");
   begin
      The_Command.Start;
      The_Command.Kill;
      The_Command.Wait_On;

      Check ("Waiting on a killed command returns", The_Command.Has_Terminated);
      Check ("It remains killed",                   Shell.Commands.State'(The_Command.Status) = Killed);
   end;


   -- Waiting on each command of a pipeline, in either order. The large output of the last command
   -- must be read while waiting on the first, or both stay blocked. It must still reach the next command.
   --
   for In_Reverse in Boolean
   loop
      declare
         The_Pipeline : Command_Array := Forge.To_Commands ("head -c 3000000 /dev/zero | cat | cat");
         Last         : Unsafe.Command renames The_Pipeline (The_Pipeline'Last);
      begin
         Start (The_Pipeline);

         if In_Reverse
         then
            for Each of reverse The_Pipeline
            loop
               Each.Wait_On;
            end loop;
         else
            for Each of The_Pipeline
            loop
               Each.Wait_On;
            end loop;
         end if;

         Check ("Waiting on a pipeline, in reverse is " & In_Reverse'Image,
                Output_Of (Results_Of (Last))'Length = 3_000_000);
      end;
   end loop;


   -- The large error output of a middle command must also be read while waiting on the first.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("head -c 3000000 /dev/zero | bash noisy_stage.sh | wc -c");
   begin
      Start (The_Pipeline);

      for Each of The_Pipeline
      loop
         Each.Wait_On;
      end loop;

      Check ("Pipeline's errors are kept",  Errors_Of (Results_Of (The_Pipeline (2)))'Length = 300_000);
      Check ("Pipeline's output is intact", +Output_Of (Results_Of (The_Pipeline (3))) = "3000000" & ASCII.LF);
   end;


   -- The results of a pipeline can be gathered while it runs, without waiting.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("echo Hello | cat");
   begin
      Start (The_Pipeline);

      The_Pipeline (1).Wait_On;
      The_Pipeline (2).Wait_On;

      Check ("Waiting on a small pipeline", +Output_Of (Results_Of (The_Pipeline (2))) = "Hello" & ASCII.LF);
   end;
end Test_Unsafe_Wait_On;
