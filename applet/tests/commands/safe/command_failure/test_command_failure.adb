with
     Shell.Commands.Safe,
     Ada.Exceptions,
     Ada.Execution_Time,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Test_Command_Failure
--
-- A failure which belongs to one command must not stop the spawn server, or leave the caller waiting.
-- Any failure of this test raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;


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


   function Contains (Source  : in String;
                      Pattern : in String) return Boolean
   is (Ada.Strings.Fixed.Index (Source, Pattern) /= 0);


   Missing : constant String := "no_such_program_for_ashell";

begin
   -- A program which doesn't exist.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command (Missing);
      The_Results : constant Command_Results := Run (The_Command);
   begin
      Check ("Missing program fails",          The_Command.Failed);
      Check ("Missing program gives an error", Contains (+Errors_Of (The_Results), Missing));
   end;

   declare
      Raised : Boolean := False;
   begin
      begin
         Run (Missing);
      exception
         when Command_Error =>
            Raised := True;
      end;

      Check ("Missing program raises in 'Run' procedure", Raised);
   end;


   -- A pipeline containing a program which doesn't exist.
   --
   for Position in 1 .. 3
   loop
      declare
         Line : constant String := (case Position is
                                       when 1      => Missing & " | cat | cat",
                                       when 2      => "sleep 20 | " & Missing & " | cat",
                                       when others => "sleep 20 | cat | " & Missing);

         The_Pipeline : Command_Array := Forge.To_Commands (Line);
      begin
         Run (The_Pipeline);

         Check ("Missing program in pipeline position" & Position'Image & " is the one which failed",
                Which_Failed (The_Pipeline) = Position);
      end;
   end loop;


   -- Sending to a command which doesn't accept input.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("sleep 1");
   begin
      The_Command.Start;
      The_Command.Send (+"Unwanted.");
      The_Command.Wait_On;

      Check ("Unwanted input is ignored", The_Command.Normal_Exit);
   end;


   -- A program which catches the interrupt, then exits. The server must not wait for it to be 'Interrupted'.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("bash catches_interrupt.sh");
   begin
      The_Command.Start;
      delay 0.5;

      Check ("Program is running when interrupted", not The_Command.Has_Terminated);

      The_Command.Interrupt;
      The_Command.Wait_On;

      Check ("Program which catches an interrupt fails", not The_Command.Normal_Exit);
   end;


   -- An early command with large error output.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash large_errors.sh | cat");
   begin
      Run (The_Pipeline);

      Check ("Early command's errors don't block 'Run'", Errors_Of (Results_Of (The_Pipeline (1)))'Length = 300_000);
   end;


   -- A failed pipeline's exception must include the error text of the command which failed.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("ls /non_existent_file | cat");
   begin
      Run (The_Pipeline, Raise_Error => True);
      Check ("Failed pipeline raises", False);
   exception
      when E : Command_Error =>
         Check ("Failed pipeline's exception has the error text",
                Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), "non_existent_file': ") /= 0);
   end;


   -- A missing program in a pipeline.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands (Missing & " | cat");
   begin
      Run (The_Pipeline, Raise_Error => True);
      Check ("Missing program in a pipeline raises", False);
   exception
      when E : Command_Error =>
         Check ("Missing program's exception has the error text",
                Contains (Ada.Exceptions.Exception_Message (E), "not found on PATH"));
   end;


   -- Interrupting a program which carries on must not hold up the server's other commands.
   --
   declare
      use Ada.Real_Time;

      Stubborn : Safe.Command := Forge.To_Command ("bash ignores_interrupt.sh");
   begin
      Stubborn.Start;
      delay 0.5;

      Stubborn.Interrupt;

      declare
         Before : constant Time := Clock;
         Output : constant String := +Output_Of (Run ("echo Prompt"));
         Taken  : constant Duration := To_Duration (Clock - Before);
      begin
         Ada.Text_IO.Put_Line ("Command took" & Taken'Image & " after an interrupt.");

         Check ("Interrupting a program doesn't hold up other commands",     Output = "Prompt" & ASCII.LF
                                                                          and Taken < 0.9);
      end;

      Check ("Program which ignores an interrupt keeps running", not Stubborn.Has_Terminated);

      Stubborn.Kill;
      Stubborn.Wait_On;
   end;


   -- A pipeline which the server cannot parse must not leave the caller waiting.
   --
   declare
      Ends_In_Bar   : Command_Array := [Forge.To_Command ("cat"), Forge.To_Command ("echo end|")];
      Contains_Bar  : Command_Array := [Forge.To_Command ("cat"), Forge.To_Command ("echo a|b")];
      Too_Many      : Command_Array := [for Each in 1 .. 51 => Forge.To_Command ("true")];
   begin
      Run (Ends_In_Bar);
      Check ("Pipeline ending in a bar fails", Which_Failed (Ends_In_Bar) = 1);
      Check ("Pipeline ending in a bar gives an error", Errors_Of (Results_Of (Ends_In_Bar (1)))'Length /= 0);

      Run (Contains_Bar);
      Check ("Argument containing a bar fails", Which_Failed (Contains_Bar) = 1);
      Check ("Argument containing a bar gives its reason", Contains (+Errors_Of (Results_Of (Contains_Bar (1))), "may contain a '|'"));

      Run (Too_Many);
      Check ("Pipeline with too many commands fails", Which_Failed (Too_Many) = 1);

      -- Failing a long pipeline must leave no backlog of actions, which would hold up the next command.
      --
      declare
         use Ada.Real_Time;

         Before : constant Time   := Clock;
         Output : constant String := +Output_Of (Run ("echo Next"));
         Taken  : constant Duration := To_Duration (Clock - Before);
      begin
         Ada.Text_IO.Put_Line ("Next command took" & Taken'Image);
         Check ("A failed long pipeline doesn't hold up the next command",     Output = "Next" & ASCII.LF
                                                                           and Taken < 1.5);
      end;
   end;


   -- A burst of actions must be handled together, rather than one for each pass over the running commands.
   --
   declare
      use Ada.Real_Time;

      Sleepers : Command_Array := [for Each in 1 .. 12 => Forge.To_Command ("sleep 20")];
      Before   : Time;
   begin
      for Each of Sleepers
      loop
         Each.Start;
      end loop;

      delay 2.0;     -- Let them all be started.
      Before := Clock;

      for Each of Sleepers
      loop
         Each.Kill;
      end loop;

      for Each of Sleepers
      loop
         Each.Wait_On;
      end loop;

      Ada.Text_IO.Put_Line ("Killing all took" & Duration'Image (To_Duration (Clock - Before)));
      Check ("A burst of actions is handled together", To_Duration (Clock - Before) < 8.0);
   end;


   -- Waiting on a command which was never started cannot end.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("echo Unstarted");
   begin
      The_Command.Wait_On;
      Check ("Waiting on an unstarted command raises", False);
   exception
      when Command_Error =>
         Check ("Waiting on an unstarted command raises", True);
   end;


   -- A command which isn't made by 'Forge'.
   --
   declare
      Unmade : Safe.Command;
      Many   : Command_Array (1 .. 3);
   begin
      Unmade   := Forge.To_Command ("echo Assigned");
      Many (2) := Forge.To_Command ("echo Assigned");

      Check ("Command made by assignment runs", +Output_Of (Run (Unmade)) = "Assigned" & ASCII.LF);
   end;


   -- A pipeline must not use a whole core while it waits.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("sleep 2 | cat");
      Before       : constant Ada.Execution_Time.CPU_Time := Ada.Execution_Time.Clock;
   begin
      Run (The_Pipeline);

      declare
         use Ada.Execution_Time,
             Ada.Real_Time;
      begin
         Check ("Waiting on a pipeline doesn't spin", To_Duration (Clock - Before) < 1.0);
      end;
   end;


   -- The server must still be running.
   --
   Check ("Server still runs commands", +Output_Of (Run ("echo Alive")) = "Alive" & ASCII.LF);
end Test_Command_Failure;
