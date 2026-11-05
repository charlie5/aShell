with
     Shell.Commands.Safe,
     Ada.Exceptions,
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


   -- The errors of every command in a pipeline reach its results.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash two_errors.sh | bash two_errors.sh Last | cat");
      The_Results  : constant Command_Results := Run (The_Pipeline);
   begin
      Check ("Results have the errors of all commands, in order",
             +Errors_Of (The_Results) = "First" & ASCII.LF & "Last" & ASCII.LF);
      Check ("A command still gives its own errors", +Errors_Of (Results_Of (The_Pipeline (1))) = "First" & ASCII.LF);
   end;

   declare
      Results : constant Command_Results := Run ("ls /non_existent_file | cat");
   begin
      Check ("Results of a failed line have the early command's errors",
             Contains (+Errors_Of (Results), "non_existent_file"));
   end;


   -- A failed pipeline's results must include what its later commands had yet to send.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("false | bash errors_then_sleeps.sh");
      The_Results  : constant Command_Results := Run (The_Pipeline);
   begin
      Check ("Failed pipeline has the errors of its later commands", Errors_Of (The_Results)'Length = 1_000_000);
      Check ("Failed pipeline's later command has ended",            The_Pipeline (2).Has_Terminated);
   end;


   -- A pipeline holding a command which has already run must not be left half started.
   --
   declare
      The_First : Safe.Command := Forge.To_Command ("echo First");
      Started   : Safe.Command := Forge.To_Command ("true");
   begin
      Run (Started);

      declare
         The_Pipeline : Command_Array := [The_First, Started];
      begin
         Run (The_Pipeline);
         Check ("Pipeline with a started command raises", False);
      exception
         when Command_Error =>
            Check ("Pipeline with a started command leaves the rest not started", State'(Status (The_Pipeline (1))) = Not_Started);
      end;
   end;


   -- A pipeline need not start from an index of 1, and a mistake must not stop the spawn client.
   --
   declare
      All_Of_Them : Command_Array := Forge.To_Commands ("echo Unused | echo Slice | cat");
      The_Results : constant Command_Results := Run (All_Of_Them (2 .. 3));
   begin
      Check ("A slice of a pipeline runs", +Output_Of (The_Results) = "Slice" & ASCII.LF);
      Check ("A slice of a pipeline didn't fail", Which_Failed (All_Of_Them (2 .. 3)) = 0);
   end;

   declare
      All_Of_Them : Command_Array := Forge.To_Commands ("echo Unused | ls /non_existent_file | cat");
   begin
      Run (All_Of_Them (2 .. 3));
      Check ("The command which failed in a slice is found", Which_Failed (All_Of_Them (2 .. 3)) = 2);
   end;

   declare
      None : Command_Array (1 .. 0);
   begin
      Start (None);
      Check ("A pipeline of no commands raises", False);
   exception
      when Command_Error =>
         Check ("A pipeline of no commands raises", True);
   end;

   declare
      None : Command_Array (1 .. 0);
   begin
      Run (None);
      Check ("Running a pipeline of no commands raises", False);
   exception
      when Command_Error =>
         Check ("Running a pipeline of no commands raises", True);
   end;

   declare
      None : Command_Array (1 .. 0);
   begin
      declare
         Unused : constant Command_Results := Run (None);
      begin
         Check ("The function running a pipeline of no commands raises", False);
      end;
   exception
      when Command_Error =>
         Check ("The function running a pipeline of no commands raises", True);
   end;


   -- A program must not inherit the pipes of other commands, or a reader
   -- which ends early would not be seen by the commands feeding it.
   --
   Check ("A long pipeline ends when its last command does", +Output_Of (Run ("yes | cat | head -1")) = "y" & ASCII.LF);


   -- A program which closes its output and carries on, must be seen to end.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("bash closes_output.sh");
   begin
      The_Command.Run;
      Check ("A program which closes its output ends normally", The_Command.Normal_Exit);
   end;


   -- An action which arrives as a program ends must not lose its exit status.
   --
   declare
      Failed_Count : Natural := 0;
   begin
      for Each in 1 .. 300
      loop
         declare
            The_Command : Safe.Command := Forge.To_Command ("true");
         begin
            The_Command.Start;

            if Each mod 2 = 0
            then
               The_Command.Pause;     -- This might really pause it, so it needs resuming.
            end if;

            The_Command.Resume;
            The_Command.Wait_On;

            if not The_Command.Normal_Exit
            then
               Failed_Count := Failed_Count + 1;
            end if;
         end;
      end loop;

      Ada.Text_IO.Put_Line ("Lost the status of" & Failed_Count'Image & " of 300 commands.");
      Check ("An action as a program ends doesn't lose its status", Failed_Count = 0);
   end;


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


   -- Arguments are sent as they are, rather than in one line for the server to split.
   --
   declare
      Ends_In_Bar  : Command_Array := [Forge.To_Command ("true"), Forge.To_Command ("echo end|")];
      Contains_Bar : Command_Array := [Forge.To_Command ("true"), Forge.To_Command ("echo a|b")];
      Many         : Command_Array := [for Each in 1 .. 51 => Forge.To_Command ("true")];
   begin
      Check ("An argument may end in a bar",  +Output_Of (Run (Ends_In_Bar))  = "end|" & ASCII.LF);
      Check ("An argument may contain a bar", +Output_Of (Run (Contains_Bar)) = "a|b"  & ASCII.LF);

      Run (Many);
      Check ("A pipeline may have many commands", Which_Failed (Many) = 0);
   end;


   -- A pipeline's failure must leave no backlog of actions, which would hold up the next command.
   --
   declare
      use Ada.Real_Time;

      The_Pipeline : Command_Array := [for Each in 1 .. 51 => Forge.To_Command ("false")];
   begin
      Run (The_Pipeline);

      declare
         Before : constant Time   := Clock;
         Output : constant String := +Output_Of (Run ("echo Next"));
         Taken  : constant Duration := To_Duration (Clock - Before);
      begin
         Ada.Text_IO.Put_Line ("Next command took" & Taken'Image);
         Check ("A failed long pipeline doesn't hold up the next command",     Output = "Next" & ASCII.LF
                                                                           and Taken < 1.5);
      end;
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


   -- A pipeline must sleep while it waits, rather than waking up to check.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("sleep 2 | cat");
      Before       : constant Natural := Times_This_Task_Has_Slept;
   begin
      Run (The_Pipeline);

      declare
         Woken : constant Natural := Times_This_Task_Has_Slept - Before;
      begin
         Ada.Text_IO.Put_Line ("Woke" & Woken'Image & " times while waiting on a pipeline.");
         Check ("Waiting on a pipeline doesn't poll", Woken < 200);
      end;
   end;


   -- The server must still be running.
   --
   Check ("Server still runs commands", +Output_Of (Run ("echo Alive")) = "Alive" & ASCII.LF);
end Test_Command_Failure;
