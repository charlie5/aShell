with Ada.Text_IO; use Ada.Text_IO;
with
     Shell;


package body Process_Tests
is
   use Ahven,
       Ahven.Framework;


   procedure Test_Status
   is
      use Shell;
      Process : Shell.Process;
   begin
      Assert (Status (Process) = Not_Started,
              "Process status should be 'Not_Started' not '" & Status (Process)'Image & "'");

      Start (Process, "sleep 5");
      --  Start (Process, "ls -R /");
      Assert (Status (Process) = Running,
              "Process status should be 'Running' not '"     & Status (Process)'Image & "'");

      delay 1.0;
      Pause (Process);
      Assert (Status (Process) = Paused,
              "Process status should be 'Paused' not '"      & Status (Process)'Image & "'");
      Put_Line (Status (Process)'Image);
      delay 3.0;

      Resume (Process);
      Assert (Status (Process) = Running,
              "Process status should be 'Running' not '"     & Status (Process)'Image & "'");

      Wait_On (Process);
      Assert (Status (Process) = Normal_Exit,
              "Process status should be 'Normal_Exit' not '" & Status (Process)'Image & "'");

      Process := Start ("ls /non_existant_file");
      Wait_On (Process);
      Assert (Status (Process) = Failed_Exit,
              "Process status should be 'Failed_Exit' not '" & Status (Process)'Image & "'");

      Process := Start ("sleep 2");
      Interrupt (Process);
      Assert (Status (Process) = Interrupted,
              "Process status should be 'Interrupted' not '" & Status (Process)'Image & "'");

      Process := Start ("sleep 2");
      Kill (Process);
      Assert (Status (Process) = Killed,
              "Process status should be 'Killed' not '" & Status (Process)'Image & "'");
      Assert (not Normal_Exit (Process),
              "A killed process should not have a normal exit.");
   end Test_Status;



   procedure Test_Ended_Process
   is
      use Shell;
      Process : Shell.Process;
   begin
      -- A process which is reaped by 'Has_Terminated' retains its final status.
      --
      Process := Start ("true");
      Wait_On (Process);
      Assert (Has_Terminated (Process),      "Process should have terminated.");
      Assert (Status (Process) = Normal_Exit, "Process status should be 'Normal_Exit' not '" & Status (Process)'Image & "'");

      Process := Start ("false");
      while not Has_Terminated (Process)
      loop
         delay 0.01;
      end loop;
      Assert (Status (Process) = Failed_Exit, "Process status should be 'Failed_Exit' not '" & Status (Process)'Image & "'");

      -- Killing and waiting on an ended process are harmless, and it retains its status.
      --
      Kill    (Process);
      Wait_On (Process);
      Assert (Status (Process) = Failed_Exit, "Process status should be 'Failed_Exit' not '" & Status (Process)'Image & "'");

      -- A process ended by a signal other than interrupt is 'Killed'.
      --
      Process := Start ("kill -TERM $$");
      Wait_On (Process);
      Assert (Status (Process) = Killed, "Process status should be 'Killed' not '" & Status (Process)'Image & "'");
      Assert (not Normal_Exit (Process), "A process ended by a signal should not have a normal exit.");
   end Test_Ended_Process;



   procedure Test_Start_With_Closed_Pipe
   is
      use Shell;
      The_Pipe : constant Pipe := To_Pipe;
      Writer   : Process       := Start (Program   => "true",     -- Not a 'Pipeline', so this closes the write end.
                                         Arguments => Nil_Strings,
                                         Output    => The_Pipe);
      Reader   : Process;
   begin
      begin
         Start (Reader,
                Program   => "cat",
                Arguments => Nil_Strings,
                Input     => The_Pipe);

         Fail ("Starting a process on a pipe with a closed write end should raise 'Process_Error'.");
      exception
         when Process_Error =>
            null;
      end;

      Assert (Status (Reader) = Not_Started,
              "Process status should be 'Not_Started' not '" & Status (Reader)'Image & "'");

      Close (The_Pipe);
      Wait_On (Writer);
   end Test_Start_With_Closed_Pipe;



   -- A program may catch an interrupt, or be resumed and exit, without aShell having caused it.
   --
   procedure Test_Unexpected_Endings
   is
      use Shell;

      procedure Wait_For (Process : in out Shell.Process;
                          State   : in     Process_State)
      is
      begin
         for Each in 1 .. 1000
         loop
            exit when Status (Process) = State;
            delay 0.01;
         end loop;

         Assert (Status (Process) = State,
                 "Process status should be '" & State'Image & "' not '" & Status (Process)'Image & "'");
      end Wait_For;

   begin
      declare     -- Catches the interrupt, then exits.
         Process : Shell.Process := Start (Program   => "bash",
                                           Arguments => [+"-c", +"trap 'exit 3' INT; sleep 20 & wait"]);
      begin
         delay 0.3;
         Interrupt (Process);
         Assert (Status (Process) = Failed_Exit, "Status should be 'Failed_Exit' not '" & Status (Process)'Image & "'");
      end;

      declare     -- Ignores the interrupt.
         Process : Shell.Process := Start (Program   => "bash",
                                           Arguments => [+"-c", +"trap '' INT; sleep 3"]);
      begin
         delay 0.3;
         Interrupt (Process);
         Assert (Status (Process) = Running, "Status should be 'Running' not '" & Status (Process)'Image & "'");
         Kill (Process);
      end;

      declare     -- Interrupting a process which has ended.
         Process : Shell.Process := Start (Program => "true", Arguments => Nil_Strings);
      begin
         Wait_On   (Process);
         Interrupt (Process);
         Assert (Status (Process) = Normal_Exit, "Status should be 'Normal_Exit' not '" & Status (Process)'Image & "'");

         begin
            Resume (Process);
            Fail ("Process_Has_Terminated expected.");
         exception
            when Process_Has_Terminated => null;
         end;
      end;

      declare     -- Stops itself, is resumed by another process, then exits.
         Process : Shell.Process := Start (Program   => "bash",
                                           Arguments => [+"-c", +"(sleep 0.5; kill -CONT $$) & kill -STOP $$; exit 7"]);
      begin
         Wait_For (Process, Paused);
         Wait_For (Process, Failed_Exit);
      end;
   end Test_Unexpected_Endings;



   -- The null id of a process which hasn't started must not wait on some other process, and take its status.
   --
   procedure Test_Unstarted_Process
   is
      use Shell;
      Unstarted : Shell.Process;
      Other     : Shell.Process := Start ("ls /non_existant_file");
   begin
      delay 0.5;     -- Let the other process end.

      Assert (not Has_Terminated (Unstarted), "An unstarted process has not terminated.");

      begin
         Wait_On (Unstarted);
         Fail ("Process_Not_Started expected.");
      exception
         when Process_Not_Started => null;
      end;

      Assert (Status (Unstarted) = Not_Started, "Process should remain not started.");

      Wait_On (Other);
      Assert (Status (Other) = Failed_Exit,
              "The other process's status should be 'Failed_Exit' not '" & Status (Other)'Image & "'");
   end Test_Unstarted_Process;



   function Output_Of_Process (Program           : in String;
                               Arguments         : in Shell.String_Array;
                               Working_Directory : in String := ".") return String
   is
      use Shell;

      The_Pipe : constant Pipe    := To_Pipe;
      Process  : Shell.Process    := Start (Program, Arguments, Working_Directory, Output => The_Pipe);
   begin
      Wait_On (Process);

      declare
         Result : constant String := To_String (Output_Of (The_Pipe));
      begin
         Close (The_Pipe);
         return Result;
      end;
   end Output_Of_Process;



   procedure Test_Start_Forms
   is
      use Shell;
   begin
      Assert (Output_Of_Process ("echo", [+"Hello"]) = "Hello" & ASCII.LF, "Function with arguments.");

      Assert (Output_Of_Process ("printf", [+"[%s]", +"one two", +"three"]) = "[one two][three]",
              "Arguments are passed exactly, including spaces.");

      Assert (Output_Of_Process ("pwd", Nil_Strings, Working_Directory => "/tmp") = "/tmp" & ASCII.LF,
              "The working directory is used.");

      declare
         The_Pipe : constant Pipe := To_Pipe;
         Process  : Shell.Process := Start (Command => "echo A command", Output => The_Pipe);
      begin
         Wait_On (Process);
         Assert (To_String (Output_Of (The_Pipe)) = "A command" & ASCII.LF, "Function with a command line.");
         Close (The_Pipe);
      end;

      declare
         The_Pipe : constant Pipe := To_Pipe;
         Process  : Shell.Process;
      begin
         Start (Process, Program => "echo", Arguments => [+"Procedure"], Output => The_Pipe);
         Wait_On (Process);
         Assert (To_String (Output_Of (The_Pipe)) = "Procedure" & ASCII.LF, "Procedure with arguments.");

         begin
            Start (Process, Program => "echo", Arguments => Nil_Strings);
            Fail ("Process_Already_Started expected.");
         exception
            when Process_Already_Started => null;
         end;

         begin
            Start (Process, Command => "echo Again");
            Fail ("Process_Already_Started expected.");
         exception
            when Process_Already_Started => null;
         end;

         Close (The_Pipe);
      end;

      declare
         The_Pipe : constant Pipe := To_Pipe;
         Process  : Shell.Process;
      begin
         Start (Process, Command => "pwd", Working_Directory => "/tmp", Output => The_Pipe);
         Wait_On (Process);
         Assert (To_String (Output_Of (The_Pipe)) = "/tmp" & ASCII.LF, "Procedure with a command line.");
         Close (The_Pipe);
      end;
   end Test_Start_Forms;



   procedure Test_Process_Pipes
   is
      use Shell;
   begin
      declare     -- Input, output and errors, all through pipes.
         Input  : constant Pipe := To_Pipe;
         Output : constant Pipe := To_Pipe;
         Errors : constant Pipe := To_Pipe;
      begin
         Write_To (Input, To_Data ("Through"));

         declare
            Process : Shell.Process := Start (Program   => "bash",
                                              Arguments => [+"-c", +"cat; echo Oops >&2"],
                                              Input     => Input,
                                              Output    => Output,
                                              Errors    => Errors);
         begin
            Close_Write_End (Input);     -- Which must be open for 'Start', and closed for the program to see the end.
            Wait_On (Process);
         end;

         Assert (To_String (Output_Of (Output)) = "Through",         "Input should reach the output.");
         Assert (To_String (Output_Of (Errors)) = "Oops" & ASCII.LF, "Errors should reach their pipe.");

         Close (Input);
         Close (Output);
         Close (Errors);
      end;

      declare     -- Without 'Pipeline', the process closes the parent's write ends.
         Output  : constant Pipe := To_Pipe;
         Process : Shell.Process := Start (Program => "true", Arguments => Nil_Strings, Output => Output);
      begin
         Wait_On (Process);
         Assert (not Is_Writeable (Output), "The write end should have been closed by 'Start'.");
         Close (Output);
      end;

      declare     -- With 'Pipeline', closing them is left to the caller.
         Output  : constant Pipe := To_Pipe;
         Process : Shell.Process := Start (Program => "true", Arguments => Nil_Strings, Output => Output, Pipeline => True);
      begin
         Wait_On (Process);
         Assert (Is_Writeable (Output), "The write end should be left open by 'Start'.");
         Close (Output);
      end;

      declare     -- A process inherits no pipe but those it is given.
         Unrelated : constant Pipe := To_Pipe;
         Output    : constant Pipe := To_Pipe;
         Process   : Shell.Process := Start (Program   => "bash",
                                             Arguments => [+"-c", +"ls -l /proc/self/fd | grep -c pipe:"],
                                             Output    => Output);
      begin
         Wait_On (Process);

         declare
            Result : constant String  := To_String (Output_Of (Output));
            Count  : constant Natural := Natural'Value (Result (Result'First .. Result'Last - 1));     -- Without the line feed.
         begin
            Assert (Count = 1, "'ls' should have only the one pipe to 'grep', not" & Count'Image & ".");
         end;

         Close (Unrelated);
         Close (Output);
      end;
   end Test_Process_Pipes;



   procedure Test_Process_Errors
   is
      use Shell;
   begin
      begin
         declare
            Unused : Shell.Process := Start (Program => "no_such_program_for_ashell", Arguments => Nil_Strings);
         begin
            Fail ("Process_Error expected, for a missing program.");
         end;
      exception
         when Process_Error => null;
      end;

      declare
         Unstarted : Shell.Process;
      begin
         Assert (Status (Unstarted) = Not_Started, "A new process should not be started.");

         begin
            Kill (Unstarted);
            Fail ("Process_Not_Started expected, from 'Kill'.");
         exception
            when Process_Not_Started => null;
         end;

         begin
            Interrupt (Unstarted);
            Fail ("Process_Not_Started expected, from 'Interrupt'.");
         exception
            when Process_Not_Started => null;
         end;

         begin
            Pause (Unstarted);
            Fail ("Process_Not_Started expected, from 'Pause'.");
         exception
            when Process_Not_Started => null;
         end;

         begin
            Resume (Unstarted);
            Fail ("Process_Not_Started expected, from 'Resume'.");
         exception
            when Process_Not_Started => null;
         end;
      end;

      declare
         Process : Shell.Process := Start (Program => "sleep", Arguments => [+"20"]);
      begin
         Pause (Process);

         begin
            Pause (Process);
            Fail ("Process_Already_Paused expected.");
         exception
            when Process_Already_Paused => null;
         end;

         Kill (Process);
         Assert (Status (Process) = Killed, "Should be killed, not " & Status (Process)'Image & ".");

         Kill (Process);     -- Harmless.

         begin
            Pause (Process);
            Fail ("Process_Has_Terminated expected.");
         exception
            when Process_Has_Terminated => null;
         end;

         Assert (not Normal_Exit (Process), "A killed process should not be a normal exit.");
         Assert (Image (Process) /= "",     "A process should have an image.");
      end;
   end Test_Process_Errors;



   -- A program which is stopped hasn't ended, so 'Wait_On' must carry on waiting.
   --
   procedure Test_Waiting_On_A_Stopped_Process
   is
      use Shell;
      Process : Shell.Process := Start (Program   => "bash",
                                        Arguments => [+"-c", +"(sleep 0.5; kill -CONT $$) & kill -STOP $$; exit 7"]);
   begin
      Wait_On (Process);

      Assert (Status (Process) = Failed_Exit,
              "'Wait_On' should return only once the process has ended, not when '" & Status (Process)'Image & "'.");
   end Test_Waiting_On_A_Stopped_Process;



   -- A process stopped then continued from outside is running again.
   --
   procedure Test_Continued_From_Outside
   is
      use Shell;
      Process : Shell.Process := Start (Program   => "bash",
                                        Arguments => [+"-c", +"(sleep 0.4; kill -CONT $$) & kill -STOP $$; sleep 0.5"]);
   begin
      delay 0.2;
      Assert (Status (Process) = Paused, "Should be paused, not '" & Status (Process)'Image & "'.");

      delay 0.4;     -- It has now been continued, and has yet to end.
      Assert (Status (Process) = Running, "Should be running again, not '" & Status (Process)'Image & "'.");

      Wait_On (Process);
      Assert (Status (Process) = Normal_Exit, "Should end normally, not '" & Status (Process)'Image & "'.");
   end Test_Continued_From_Outside;



   overriding
   procedure Initialize (T : in out Test)
   is
   begin
      T.Set_Name ("Process Tests");

      T.Add_Test_Routine (Test_Status'Access, "Process Status");
      T.Add_Test_Routine (Test_Start_Forms'Access, "Start Forms");
      T.Add_Test_Routine (Test_Waiting_On_A_Stopped_Process'Access, "Waiting On A Stopped Process");
      T.Add_Test_Routine (Test_Continued_From_Outside'Access, "Continued From Outside");
      T.Add_Test_Routine (Test_Process_Pipes'Access, "Process Pipes");
      T.Add_Test_Routine (Test_Process_Errors'Access, "Process Errors");
      T.Add_Test_Routine (Test_Unstarted_Process'Access, "Unstarted Process");
      T.Add_Test_Routine (Test_Ended_Process'Access, "Ended Process");
      T.Add_Test_Routine (Test_Unexpected_Endings'Access, "Unexpected Endings");
      T.Add_Test_Routine (Test_Start_With_Closed_Pipe'Access, "Start With Closed Pipe");

   end Initialize;



  function Get_Test_Suite return Ahven.Framework.Test_Suite
  is
     Suite : Test_Suite        := Ahven.Framework.Create_Suite ("Processes");
     Test  : Process_Tests.Test;
  begin
      Suite.Add_Static_Test (Test);
      return Suite;
   end Get_Test_Suite;


end Process_Tests;
