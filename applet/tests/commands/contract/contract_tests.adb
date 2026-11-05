with
     Under_Test,
     Flavour,
     Shell.Commands,
     Ada.Exceptions,
     Ada.Streams,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Contract_Tests
is
   use Shell,
       Shell.Commands,
       Under_Test;

   use type Ada.Streams.Stream_Element_Array;
   use type Ada.Streams.Stream_Element_Offset;

   subtype Command is Under_Test.Command;     -- 'Shell.Commands' has one too.

   LF : constant Character := ASCII.LF;

   type Data_Offset_Array is array (Positive range <>) of Data_Offset;


   Failures : Natural := 0;     -- Carry on after a failure, to see them all.


   procedure Check (Test : in String;
                    Ok   : in Boolean)
   is
   begin
      if Ok
      then
         Ada.Text_IO.Put_Line (Test & " passed.");
      else
         Ada.Text_IO.Put_Line (Test & " FAILED.");
         Failures := Failures + 1;
      end if;
   end Check;


   function Contains (Source  : in String;
                      Pattern : in String) return Boolean
   is (Ada.Strings.Fixed.Index (Source, Pattern) /= 0);


   function Count_Of (Source  : in String;
                      Pattern : in String) return Natural
   is (Ada.Strings.Fixed.Count (Source, Pattern));


   -- Every value of a byte, so that any text handling of the data would show.
   --
   function Test_Data (Length : in Data_Offset) return Data
   is
      Result : Data (1 .. Length);
   begin
      for i in Result'Range
      loop
         Result (i) := Ada.Streams.Stream_Element (i mod 256);
      end loop;

      return Result;
   end Test_Data;


   -- Each of these expects 'Command_Error' from the call.
   --
   generic
      with procedure Call;
   procedure Check_Raises (Test : in String);

   procedure Check_Raises (Test : in String)
   is
      Raised : Boolean := False;
   begin
      begin
         Call;
      exception
         when Command_Error =>
            Raised := True;
      end;

      Check (Test, Raised);
   end Check_Raises;

begin
   --------------------
   --- Forming commands
   --
   declare
      With_Arguments : constant Command := Forge.To_Command ("ls -l /tmp");
      Without        : constant Command := Forge.To_Command ("ls");
      Extra_Spaces   : constant Command := Forge.To_Command ("ls    -l     /tmp");
   begin
      Check ("Name of a command",                     Name (With_Arguments)      = "ls");
      Check ("Arguments of a command",                Arguments (With_Arguments) = "-l /tmp");
      Check ("Name of a command without arguments",   Name (Without)             = "ls");
      Check ("Command without arguments has none",    Arguments (Without)        = "");
      Check ("Extra spaces don't make arguments",     Arguments (Extra_Spaces)   = "-l /tmp");

      Check ("Image has the name",                    Contains (Image (With_Arguments), "ls"));
      Check ("Image has the arguments",               Contains (Image (With_Arguments), "-l, /tmp"));
      Check ("Image of a command without arguments",  Contains (Image (Without), "ls"));
   end;

   Check ("Image shows repeated arguments as it does others",
          Contains (Image (Forge.To_Command ("echo a a a")), "(echo, (a, a, a)"));

   declare
      use Forge;

      One_Command : constant Command       := +"echo Hello";
      Pipeline    : constant Command_Array := +"ls -l | wc -l | cat";
   begin
      Check ("'+' makes a command",             Name (One_Command) = "echo");
      Check ("'+' makes a pipeline",            Pipeline'Length = 3);
      Check ("First command of a pipeline",     Name (Pipeline (1)) = "ls" and Arguments (Pipeline (1)) = "-l");
      Check ("Last command of a pipeline",      Name (Pipeline (3)) = "cat" and Arguments (Pipeline (3)) = "");
   end;

   declare
      The_Pipeline : constant Command_Array := Forge.To_Commands ("echo Only");
   begin
      Check ("A pipeline of one command", The_Pipeline'Length = 1);
   end;


   -----------------------
   --- Output and failures
   --
   declare
      The_Command : Command := Forge.To_Command ("bash ../both_streams.sh");
      The_Results : constant Command_Results := Run (The_Command);
   begin
      Check ("Output is kept apart from errors",  +Output_Of (The_Results) = "Out" & LF);
      Check ("Errors are kept apart from output", +Errors_Of (The_Results) = "Err" & LF);
      Check ("A command which succeeds didn't fail", not The_Command.Failed);
      Check ("Normal_Exit after success",             The_Command.Normal_Exit);
      Check ("The results are given only once",  Output_Of (Results_Of (The_Command))'Length = 0);
   end;

   declare
      The_Command : Command := Forge.To_Command ("bash ../exits_with.sh 3");
   begin
      The_Command.Run;

      Check ("A failing program fails",       The_Command.Failed);
      Check ("Failure isn't a normal exit",   not The_Command.Normal_Exit);
      Check ("A failing program's errors",    +Errors_Of (Results_Of (The_Command)) = "Exiting with 3" & LF);
   end;

   declare
      The_Command : Command := Forge.To_Command ("bash ../exits_with.sh 3");
      Raised      : Boolean := False;
   begin
      The_Command.Run (Raise_Error => True);
   exception
      when E : Command_Error =>
         Raised := True;
         Check ("'Raise_Error' names the command",  Contains (Ada.Exceptions.Exception_Message (E), "'bash'"));
         Check ("'Raise_Error' gives the errors",   Contains (Ada.Exceptions.Exception_Message (E), "Exiting with 3"));
         Check ("'Raise_Error' raises",             Raised);
   end;

   Check ("The 'Run' function doesn't raise for a failing line", Errors_Of (Run ("ls /non_existent_file"))'Length /= 0);

   declare
      Raised : Boolean := False;
   begin
      begin
         Run ("ls /non_existent_file");
      exception
         when Command_Error =>
            Raised := True;
      end;

      Check ("The 'Run' procedure raises for a failing line", Raised);

      Run ("true");
      Check ("The 'Run' procedure succeeds", True);
   end;

   declare
      The_Command : Command := Forge.To_Command ("bash -c kill${IFS}-TERM${IFS}$$");
   begin
      The_Command.Run;
      Check ("A program ended by a signal fails", The_Command.Failed);
   end;


   ----------
   --- Input
   --
   for Length of Data_Offset_Array'(1, 4_095, 4_096, 4_097, 65_536, 1_000_000)
   loop
      declare
         Input       : constant Data    := Test_Data (Length);
         The_Command : Command          := Forge.To_Command ("cat");
         The_Results : constant Command_Results := Run (The_Command, Input);
      begin
         Check ("Input of" & Length'Image & " bytes comes back unchanged", Output_Of (The_Results) = Input);
      end;
   end loop;

   declare
      The_Command : Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (Accepts_Input => True);

      The_Command.Send (+"One ");
      The_Command.Send (No_Data);
      The_Command.Send (+"Two ");
      The_Command.Send (Test_Data (10_000));
      The_Command.Send (+"Three");
      The_Command.Close_Input;
      The_Command.Close_Input;     -- Harmless.

      The_Command.Wait_On;

      Check ("Input sent arrives in order",     Output_Of (Results_Of (The_Command)) = To_Data ("One Two ") & Test_Data (10_000) & To_Data ("Three"));
      Check ("Closing the input ends a reader", The_Command.Normal_Exit);
   end;

   declare
      The_Command : Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (Input => +"First ", Accepts_Input => True);
      The_Command.Send  (+"Second");
      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("Input given to 'Start' comes before input sent", +Output_Of (Results_Of (The_Command)) = "First Second");
   end;

   declare
      The_Command : Command := Forge.To_Command ("true");
   begin
      The_Command.Run;
      The_Command.Close_Input;

      Check ("Closing the input of a command without any does nothing", True);
   end;


   -----------------------
   --- Status and control
   --
   declare
      The_Command : Command := Forge.To_Command ("sleep 0.3");

      procedure Wait is
      begin
         The_Command.Wait_On;
      end Wait;

      procedure Pause_It is
      begin
         The_Command.Pause;
      end Pause_It;

      procedure Resume_It is
      begin
         The_Command.Resume;
      end Resume_It;

      procedure Kill_It is
      begin
         The_Command.Kill;
      end Kill_It;

      procedure Interrupt_It is
      begin
         The_Command.Interrupt;
      end Interrupt_It;

      procedure Send_It is
      begin
         The_Command.Send (+"Input");
      end Send_It;

      procedure Start_It is
      begin
         The_Command.Start;
      end Start_It;

      procedure Run_It is
      begin
         The_Command.Run;
      end Run_It;

      procedure Check_Wait   is new Check_Raises (Wait);
      procedure Check_Pause  is new Check_Raises (Pause_It);
      procedure Check_Resume is new Check_Raises (Resume_It);
      procedure Check_Kill   is new Check_Raises (Kill_It);
      procedure Check_Interrupt is new Check_Raises (Interrupt_It);
      procedure Check_Send   is new Check_Raises (Send_It);
      procedure Check_Start  is new Check_Raises (Start_It);
      procedure Check_Run    is new Check_Raises (Run_It);
   begin
      Check ("A new command is not started",        State'(The_Command.Status) = Not_Started);
      Check ("A new command has not terminated",    not The_Command.Has_Terminated);

      Check_Wait   ("Waiting on a new command raises");
      Check_Pause  ("Pausing a new command raises");
      Check_Resume ("Resuming a new command raises");
      Check_Kill   ("Killing a new command raises");
      Check_Interrupt ("Interrupting a new command raises");
      Check_Send   ("Sending to a new command raises");

      The_Command.Start;

      Check ("A started command is running",        State'(The_Command.Status) = Running);
      Check ("A started command has not terminated", not The_Command.Has_Terminated);

      Check_Start  ("Starting a command twice raises");
      Check_Run    ("Running a started command raises");

      The_Command.Wait_On;
      The_Command.Wait_On;     -- Harmless.

      Check ("A command which has ended has terminated", The_Command.Has_Terminated);
      Check ("A command which has ended has a status",   State'(The_Command.Status) = Normal_Exit);

      Check_Start  ("Starting a command which has ended raises");
      Check_Run    ("Running a command which has ended raises");
      Check_Pause  ("Pausing a command which has ended raises");
      Check_Kill   ("Killing a command which has ended raises");

      Check ("A failed start leaves the status alone", State'(The_Command.Status) = Normal_Exit);
   end;

   -- These don't read the status first, as a read of it could hide a stale one.
   --
   declare
      Killed_Late  : Command := Forge.To_Command ("true");
      Paused_Late  : Command := Forge.To_Command ("true");
      Resumed_Late : Command := Forge.To_Command ("true");

      procedure Kill_It is
      begin
         Killed_Late.Kill;
      end Kill_It;

      procedure Pause_It is
      begin
         Paused_Late.Pause;
      end Pause_It;

      procedure Resume_It is
      begin
         Resumed_Late.Resume;
      end Resume_It;

      procedure Check_Kill   is new Check_Raises (Kill_It);
      procedure Check_Pause  is new Check_Raises (Pause_It);
      procedure Check_Resume is new Check_Raises (Resume_It);
   begin
      Killed_Late.Start;
      Killed_Late.Wait_On;
      Check_Kill ("Killing a command just seen to end raises");
      Check ("Killing a command which has ended leaves its status", State'(Killed_Late.Status) = Normal_Exit);

      Paused_Late.Start;
      Paused_Late.Wait_On;
      Check_Pause ("Pausing a command just seen to end raises");
      Check ("Pausing a command which has ended leaves its status", State'(Paused_Late.Status) = Normal_Exit);

      Resumed_Late.Start;
      Resumed_Late.Wait_On;
      Check_Resume ("Resuming a command just seen to end raises");
      Check ("Resuming a command which has ended leaves its status", State'(Resumed_Late.Status) = Normal_Exit);
   end;

   declare
      The_Command : Command := Forge.To_Command ("sleep 0.5");

      procedure Pause_It is
      begin
         The_Command.Pause;
      end Pause_It;

      procedure Check_Pause is new Check_Raises (Pause_It);
   begin
      The_Command.Start;
      The_Command.Pause;
      delay 0.1;

      Check ("A paused command reads as paused",     State'(The_Command.Status) = Paused);
      Check ("A paused command has not terminated",  not The_Command.Has_Terminated);

      Check_Pause ("Pausing a paused command raises");

      delay 0.6;     -- Longer than the program would otherwise have run.

      Check ("A paused program doesn't carry on",    not The_Command.Has_Terminated);

      The_Command.Resume;
      Check ("A resumed command reads as running",   State'(The_Command.Status) = Running);

      The_Command.Wait_On;
      Check ("A resumed command ends normally",      The_Command.Normal_Exit);
   end;

   declare
      The_Command : Command := Forge.To_Command ("sleep 20");

      procedure Kill_It is
      begin
         The_Command.Kill;
      end Kill_It;

      procedure Check_Kill is new Check_Raises (Kill_It);
   begin
      The_Command.Start;
      The_Command.Kill;
      The_Command.Wait_On;

      Check ("A killed command reads as killed",  State'(The_Command.Status) = Killed);
      Check ("A killed command has failed",       The_Command.Failed);
      Check ("A killed command has terminated",   The_Command.Has_Terminated);

      Check_Kill ("Killing a killed command raises");
   end;

   declare
      The_Command : Command := Forge.To_Command ("sleep 20");
   begin
      The_Command.Start;
      The_Command.Pause;
      The_Command.Kill;
      The_Command.Wait_On;

      Check ("A paused command can be killed", The_Command.Failed);
   end;

   declare
      The_Command : Command := Forge.To_Command ("sleep 20");
   begin
      The_Command.Start;
      delay 0.1;
      The_Command.Interrupt;
      The_Command.Wait_On;

      Check ("An interrupted command has failed", The_Command.Failed);
   end;


   -- A difference between the two, which is documented.
   --
   declare
      The_Command : Command := Forge.To_Command ("sleep 0.2");
   begin
      The_Command.Start;

      begin
         The_Command.Resume;
         Check ("Resuming a running command does nothing on 'Safe', and raises on 'Unsafe'", Flavour.Is_Safe);
      exception
         when Command_Error =>
            Check ("Resuming a running command does nothing on 'Safe', and raises on 'Unsafe'", not Flavour.Is_Safe);
      end;

      The_Command.Wait_On;
   end;


   -------------
   --- Pipelines
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("echo Hello | tr l L | cat");
      The_Results  : constant Command_Results := Run (The_Pipeline);
   begin
      Check ("A pipeline gives the last command's output", +Output_Of (The_Results) = "HeLLo" & LF);
      Check ("A pipeline which succeeds has no errors",    Errors_Of (The_Results)'Length = 0);
      Check ("A pipeline which succeeds didn't fail",      not Failed (The_Pipeline));
      Check ("Which_Failed is 0 when none failed",         Which_Failed (The_Pipeline) = 0);
      Check ("Commands in a finished pipeline have ended",     State'(The_Pipeline (1).Status) = Normal_Exit
                                                           and State'(The_Pipeline (3).Status) = Normal_Exit);
   end;

   declare
      Input        : constant Data := Test_Data (1_000_000);
      The_Pipeline : Command_Array := Forge.To_Commands ("cat | cat | cat");
      The_Results  : constant Command_Results := Run (The_Pipeline, Input);
   begin
      Check ("Input goes through a pipeline unchanged", Output_Of (The_Results) = Input);
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("true | false | true");
   begin
      Run (The_Pipeline);

      Check ("A pipeline with a failing command fails", Failed (The_Pipeline));
      Check ("Which_Failed finds the failing command",  Which_Failed (The_Pipeline) = 2);
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("true | bash ../exits_with.sh 4 | cat");
      Raised       : Boolean       := False;
   begin
      begin
         Run (The_Pipeline, Raise_Error => True);
      exception
         when E : Command_Error =>
            Raised :=     Contains (Ada.Exceptions.Exception_Message (E), "Pipeline command 2")
                      and Contains (Ada.Exceptions.Exception_Message (E), "Exiting with 4");
      end;

      Check ("A failing pipeline's exception gives the command and its errors", Raised);
   end;

   Check ("A line with a pipeline",  +Output_Of (Run ("echo Line | cat")) = "Line" & LF);
   Check ("A line with input",       +Output_Of (Run ("cat | cat", +"Given")) = "Given");
   Check ("A long pipeline",         +Output_Of (Run ("echo Long | cat | cat | cat | cat | cat | cat | cat | cat | cat")) = "Long" & LF);
   Check ("A reader ending early ends the pipeline", +Output_Of (Run ("yes | cat | head -1")) = "y" & LF);

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("echo Started | cat");
   begin
      Start (The_Pipeline);
      The_Pipeline (2).Wait_On;

      Check ("A pipeline which is started then waited on", +Output_Of (Results_Of (The_Pipeline (2))) = "Started" & LF);
   end;

   declare
      Separate_Commands : Command_Array := Forge.To_Commands ("cat | cat");
   begin
      Start (Separate_Commands, Input => +"Each", Pipeline => False);

      for Each of Separate_Commands
      loop
         Each.Wait_On;
         Check ("Commands which aren't a pipeline each get the input", +Output_Of (Results_Of (Each)) = "Each");
      end loop;
   end;


   -- Only the command which can't start is at fault. Those before it are stopped, which isn't a normal exit.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("sleep 20 | no_such_program_for_ashell | cat");
   begin
      Run (The_Pipeline);

      Check ("Which_Failed finds the command which can't start",       Which_Failed (The_Pipeline) = 2);
      Check ("A command stopped by another not starting has failed",   The_Pipeline (1).Failed);
      Check ("A command stopped by another isn't a normal exit",       not The_Pipeline (1).Normal_Exit);
      Check ("A command which can't start isn't a normal exit",        not The_Pipeline (2).Normal_Exit);
      Check ("A command after one which can't start isn't either",     not The_Pipeline (3).Normal_Exit);
      Check ("Which_Failed finds a stopped command, if all it is given", Which_Failed (The_Pipeline (1 .. 1)) = 1);
   end;

   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("sleep 20 | no_such_program_for_ashell | cat");
      Named        : Boolean       := False;
   begin
      begin
         Run (The_Pipeline, Raise_Error => True);
      exception
         when E : Command_Error =>
            Named := Contains (Ada.Exceptions.Exception_Message (E), "Pipeline command 2: ");
      end;

      Check ("The error names the command which can't start", Named);
   end;


   -- What a command left in its pipes before being stopped is kept, when a later command can't start.
   -- It may not have written anything yet, so allow a few tries. Starting the many commands gives it time.
   --
   declare
      Is_Kept : Boolean := False;
   begin
      for Each in 1 .. 5
      loop
         declare
            The_Pipeline : Command_Array := Forge.To_Commands ("bash ../writes_then_sleeps.sh | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | cat | no_such_program_for_ashell");
         begin
            Run (The_Pipeline);

            Is_Kept := +Errors_Of (Results_Of (The_Pipeline (1))) = "Early" & LF;
         end;

         exit when Is_Kept;
      end loop;

      Check ("A stopped command's results are kept, when a later command can't start", Is_Kept);
   end;


   ----------
   --- Copies
   --
   declare
      The_Command : Command := Forge.To_Command ("echo Original");
   begin
      The_Command.Start;

      declare
         Copy : constant Command := The_Command with Unreferenced;
      begin
         null;
      end;

      The_Command.Wait_On;

      Check ("Finalizing a copy leaves the command working", +Output_Of (Results_Of (The_Command)) = "Original" & LF);
   end;

   declare
      Assigned : Command := Forge.To_Command ("echo First");
      Other    : constant Command := Forge.To_Command ("echo Second");
   begin
      Assigned := Other;
      Check ("Assigning replaces the command", +Output_Of (Run (Assigned)) = "Second" & LF);
   end;

   declare
      Unmade : Command with Warnings => Off;
      Many   : Command_Array (1 .. 3);
   begin
      Many (2) := Forge.To_Command ("echo Made");
      Unmade   := Many (2);

      Check ("A command which isn't made by 'Forge' can be assigned", +Output_Of (Run (Many (2))) = "Made" & LF);
   end;

   declare
      Unmade : Command;
   begin
      Unmade.Run;
      Check ("A command with no program fails", Unmade.Failed);
   exception
      when Process_Error =>
         Check ("A command with no program fails", False);
   end;


   -----------------------
   --- What programs see
   --
   declare
      Listing : constant String := +Output_Of (Run ("bash ../show_descriptors.sh"));
   begin
      Ada.Text_IO.Put_Line (Listing);
      Check ("A program inherits only the pipes it is given", Count_Of (Listing, "pipe:") <= 2);
   end;

   -- What a command line does with quotes, which isn't what a shell does.
   --
   Check ("Double quotes group an argument, and remain part of it",
          +Output_Of (Run ("printf [%s] ""one two"" three")) = "[""one two""][three]");

   Check ("Single quotes aren't understood",
          +Output_Of (Run ("printf [%s] 'one two' three")) = "['one][two'][three]");

   Check ("A backslash keeps a space within an argument",
          +Output_Of (Run ("printf [%s] one\ two three")) = "[one two][three]");

   Check ("A backslash is kept with what it escapes in a pipeline",
          +Output_Of (Run ("printf [%s] one\ two | cat")) = "[one two]");

   if Failures /= 0
   then
      raise Program_Error with Failures'Image & " tests failed.";
   end if;

   Ada.Text_IO.Put_Line ("End contract tests.");
end Contract_Tests;
