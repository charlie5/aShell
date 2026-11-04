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



   overriding
   procedure Initialize (T : in out Test)
   is
   begin
      T.Set_Name ("Process Tests");

      T.Add_Test_Routine (Test_Status'Access, "Process Status");
      T.Add_Test_Routine (Test_Ended_Process'Access, "Ended Process");
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
