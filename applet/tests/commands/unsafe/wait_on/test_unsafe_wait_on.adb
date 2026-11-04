with
     Shell.Commands.Unsafe,
     Ada.Calendar,
     Ada.Text_IO;


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

begin
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
