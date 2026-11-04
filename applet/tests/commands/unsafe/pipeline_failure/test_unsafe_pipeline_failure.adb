with
     Shell.Commands.Unsafe,
     Ada.Directories,
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
