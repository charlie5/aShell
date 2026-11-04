with
     Shell.Commands.Unsafe,
     Ada.Directories,
     Ada.Execution_Time,
     Ada.Real_Time,
     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Test_Unsafe_Pipeline_Failure
--
-- Failing to start a pipeline must raise cleanly, stop any commands which did start, and leak no descriptors.
-- 'Run' must also read the error output of every command. Any failure raises an exception, or hangs.
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


   function Open_Descriptors return Natural
   is
      use Ada.Directories;

      Search : Search_Type;
      Item   : Directory_Entry_Type;
      Count  : Natural := 0;
   begin
      Start_Search (Search, "/proc/self/fd", "");

      while More_Entries (Search)
      loop
         Get_Next_Entry (Search, Item);
         Count := Count + 1;
      end loop;

      End_Search (Search);
      return Count;
   end Open_Descriptors;


   function Contains (Source  : in String;
                      Pattern : in String) return Boolean
   is (Ada.Strings.Fixed.Index (Source, Pattern) /= 0);


   Missing : constant String := "no_such_program_for_ashell";

begin
   -- An early command with large error output.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash large_errors.sh | cat");
   begin
      Run (The_Pipeline);

      Check ("Early command's errors don't block 'Run'", Errors_Of (Results_Of (The_Pipeline (1)))'Length = 300_000);
   end;


   -- A missing program, in each position.
   --
   for Position in 1 .. 2
   loop
      declare
         Line : constant String := (if Position = 1 then Missing & " | cat"
                                                    else "sleep 30 | " & Missing);
      begin
         declare
            The_Pipeline : Command_Array := Forge.To_Commands (Line);
         begin
            begin
               Start (The_Pipeline);
               Check ("Missing program raises", False);
            exception
               when Process_Error =>
                  Check ("Missing program raises", True);
            end;

            if Position = 2
            then
               Check ("Command which started is stopped", The_Pipeline (1).Has_Terminated);
            end if;
         end;

         Check ("Leaving the scope doesn't raise", True);
      end;
   end loop;


   -- 'Run' treats a missing program as a failure, as safe commands do.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command (Missing);
      The_Results : constant Command_Results := Run (The_Command);
   begin
      Check ("Missing program fails 'Run' function",       The_Command.Failed);
      Check ("Missing program has ended",                  The_Command.Has_Terminated);
      Check ("Missing program gives an error",             Contains (+Errors_Of (The_Results), Missing));
   end;

   declare
      Results : constant Command_Results := Run (Missing);
   begin
      Check ("Missing program in a line gives an error",   Contains (+Errors_Of (Results), "not found on PATH"));
   end;

   declare
      Raised : Boolean := False;
   begin
      begin
         Run (Missing);
      exception
         when E : Command_Error =>
            Raised := Contains (Ada.Exceptions.Exception_Message (E), "not found on PATH");
      end;

      Check ("Missing program raises in 'Run' procedure", Raised);
   end;

   for Position in 1 .. 3
   loop
      declare
         Line : constant String := (case Position is
                                       when 1      => Missing & " | cat | cat",
                                       when 2      => "sleep 20 | " & Missing & " | cat",
                                       when others => "sleep 20 | cat | " & Missing);

         The_Pipeline : Command_Array := Forge.To_Commands (Line);
         The_Results  : constant Command_Results := Run (The_Pipeline);
      begin
         Check ("Missing program in pipeline position" & Position'Image & " is the one which failed",
                Which_Failed (The_Pipeline) = Position);
         Check ("Pipeline with a missing program gives an error", Contains (+Errors_Of (The_Results), "not found on PATH"));
         Check ("Pipeline with a missing program has ended",      The_Pipeline (The_Pipeline'Last).Has_Terminated);
      end;
   end loop;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("sleep 20 | " & Missing);
      Raised       : Boolean       := False;
   begin
      begin
         Run (The_Pipeline, Raise_Error => True);
      exception
         when E : Command_Error =>
            Raised := Contains (Ada.Exceptions.Exception_Message (E), "Pipeline command 2: ");
      end;

      Check ("Missing program in a pipeline raises, naming the command", Raised);
   end;


   -- The errors of every command in a pipeline reach its results.
   --
   declare
      Results : constant Command_Results := Run ("ls /non_existent_file | cat");
   begin
      Check ("Results of a failed line have the early command's errors",
             Contains (+Errors_Of (Results), "non_existent_file"));
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash two_errors.sh | bash two_errors.sh Last | cat");
      The_Results  : constant Command_Results := Run (The_Pipeline);
      All_Errors   : constant String := +Errors_Of (The_Results);
   begin
      Check ("Results have the errors of all commands, in order",
             All_Errors = "First" & ASCII.LF & "Last" & ASCII.LF);
      Check ("Output is only that of the last command", Output_Of (The_Results)'Length = 0);
      Check ("A command still gives its own errors",    +Errors_Of (Results_Of (The_Pipeline (1))) = "First" & ASCII.LF);
      Check ("The last command's own errors are its own", Errors_Of (Results_Of (The_Pipeline (3)))'Length = 0);
   end;


   -- Starting a command twice must not disturb the one which is running.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("sleep 0.5");
      Raised      : Boolean        := False;
   begin
      The_Command.Start;

      begin
         The_Command.Start;
      exception
         when Command_Error =>
            Raised := True;
      end;

      Check ("Starting a command twice raises",            Raised);
      Check ("Starting a command twice leaves it running", State'(The_Command.Status) = Running);

      The_Command.Wait_On;
      Check ("A command started twice still ends normally", The_Command.Normal_Exit);

      begin
         Run (The_Command);
         Check ("Running a command which has ended raises", False);
      exception
         when Command_Error =>
            Check ("Running a command which has ended leaves its status",     The_Command.Normal_Exit
                                                                          and State'(The_Command.Status) = Normal_Exit);
      end;
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash two_errors.sh | sleep 0.5");
   begin
      Start (The_Pipeline);

      for Attempt in 1 .. 2
      loop
         begin
            if Attempt = 1
            then
               Start (The_Pipeline);
            else
               Run (The_Pipeline);
            end if;

            Check ("Starting a pipeline twice raises", False);
         exception
            when Command_Error =>
               Check ("Starting a pipeline twice leaves it running", not The_Pipeline (2).Has_Terminated);
         end;
      end loop;

      The_Pipeline (2).Wait_On;
      Check ("A pipeline started twice still ends normally", The_Pipeline (2).Normal_Exit);
      Check ("A pipeline started twice keeps its pipes", +Errors_Of (Results_Of (The_Pipeline (1))) = "First" & ASCII.LF);
   end;


   -- A program must not inherit the pipes of other commands, or a reader
   -- which ends early would not be seen by the commands feeding it.
   --
   Check ("A long pipeline ends when its last command does", +Output_Of (Run ("yes | cat | head -1")) = "y" & ASCII.LF);


   -- A program which stops itself, as if by Ctrl-Z.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("bash stops_itself.sh");
   begin
      The_Command.Start;
      delay 0.3;

      Check ("A stopped program has not terminated", not The_Command.Has_Terminated);
      Check ("A stopped program is paused",          State'(The_Command.Status) = Paused);

      The_Command.Resume;
      The_Command.Wait_On;

      Check ("A stopped program can be resumed", +Output_Of (Results_Of (The_Command)) = "Resumed" & ASCII.LF);
   end;


   -- Closing the input must reach the program straight away, rather than when its idle writer next checks.
   --
   declare
      use Ada.Real_Time;

      Taken : Duration := 0.0;
   begin
      for Each in 1 .. 10
      loop
         declare
            The_Command : Unsafe.Command := Forge.To_Command ("cat");
         begin
            The_Command.Start (Accepts_Input => True);
            The_Command.Send  (+"Sent");

            delay 0.02;     -- Let the writer finish writing, and become idle.

            declare
               Before : constant Time := Clock;
            begin
               The_Command.Close_Input;
               The_Command.Wait_On;
               Taken := Taken + To_Duration (Clock - Before);
            end;

            Check ("A command gets its input", +Output_Of (Results_Of (The_Command)) = "Sent");
         end;
      end loop;

      Check ("Closing the input reaches the program straight away", Taken / 10 < 0.05);
   end;


   -- A program which closes its output and carries on must not use a whole core, nor end early.
   --
   declare
      use Ada.Execution_Time,
          Ada.Real_Time;

      The_Command : Unsafe.Command := Forge.To_Command ("bash closes_output.sh");
      Before      : constant CPU_Time := Ada.Execution_Time.Clock;
   begin
      The_Command.Run;

      Check ("A program which closes its output ends normally", The_Command.Normal_Exit);
      Check ("Waiting on it doesn't spin", To_Duration (Ada.Execution_Time.Clock - Before) < 0.1);
   end;


   -- Waiting on a connected command must leave its output for the command it is connected to.
   --
   declare
      Writer : Unsafe.Command := Forge.To_Command ("echo Connected");
      Reader : Unsafe.Command := Forge.To_Command ("cat");
   begin
      Connect (Writer, Reader);

      Writer.Start (Pipeline => True);
      Writer.Wait_On;
      Check ("A connected command gives its output to no one else", Output_Of (Results_Of (Writer))'Length = 0);

      Reader.Start;
      Close_Write_End (Output_Pipe (Writer));
      Reader.Wait_On;

      Check ("Output of a connected command reaches its reader", +Output_Of (Results_Of (Reader)) = "Connected" & ASCII.LF);
   end;


   -- The status of a command in a pipeline must be current after 'Run'.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("echo Status | cat");
   begin
      Run (The_Pipeline);

      Check ("Commands which ended normally have that status",     State'(The_Pipeline (1).Status) = Normal_Exit
                                                               and State'(The_Pipeline (2).Status) = Normal_Exit);
      begin
         The_Pipeline (1).Kill;
         Check ("Killing a command which has ended raises", False);
      exception
         when Command_Error =>
            Check ("Killing a command which has ended leaves its status", State'(The_Pipeline (1).Status) = Normal_Exit);
      end;
   end;


   -- A pipeline need not start from an index of 1.
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


   -- A pipeline of one command must leak nothing.
   --
   declare
      Before : constant Natural := Open_Descriptors;
   begin
      for Each in 1 .. 50
      loop
         declare
            The_Pipeline : Command_Array := Forge.To_Commands ("true");
         begin
            Run (The_Pipeline);
         end;
      end loop;

      Check ("A pipeline of one command leaks no descriptors", Open_Descriptors = Before);
   end;


   -- A failed single command returns to not having been started.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command (Missing);
   begin
      begin
         The_Command.Start;
      exception
         when Process_Error =>
            null;
      end;

      Check ("Failed command isn't running", State'(The_Command.Status) = Not_Started);
   end;


   -- Repeated failures.
   --
   declare
      Before : constant Natural := Open_Descriptors;
   begin
      for Each in 1 .. 50
      loop
         declare
            The_Command  : Unsafe.Command := Forge.To_Command (Missing);
            The_Pipeline : Command_Array  := Forge.To_Commands (Missing & " | cat");
         begin
            begin
               The_Command.Start (Accepts_Input => True);
            exception
               when Process_Error =>
                  null;
            end;

            begin
               Start (The_Pipeline);
            exception
               when Process_Error =>
                  null;
            end;
         end;
      end loop;

      Check ("Failed starts leak no descriptors", Open_Descriptors = Before);
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


   -- Writing directly to a pipe which also has a background writer must still block, rather than fail.
   --
   declare
      The_Input   : constant Data  := [1 .. 300_000 => 65];
      The_Command : Unsafe.Command := Forge.To_Command ("bash slow_reader.sh");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Send  (The_Input);

      Shell.Write_To (The_Command.Input_Pipe, The_Input);

      Check ("Direct write to a full pipe blocks", True);

      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("Both writes arrive", +Output_Of (Results_Of (The_Command)) = "600000" & ASCII.LF);
   end;


   -- A program which catches the interrupt, then exits.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("bash catches_interrupt.sh");
   begin
      The_Command.Start;
      delay 0.5;

      Check ("Program is running when interrupted", not The_Command.Has_Terminated);

      The_Command.Interrupt;

      Check ("'Interrupt' sees the program end", The_Command.Has_Terminated);
      Check ("Program which catches an interrupt fails", State'(The_Command.Status) = Failed_Exit);
   end;

   Ada.Text_IO.Put_Line ("End test.");
end Test_Unsafe_Pipeline_Failure;
