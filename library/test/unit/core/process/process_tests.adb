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
   end Test_Status;





   overriding
   procedure Initialize (T : in out Test)
   is
   begin
      T.Set_Name ("Process Tests");

      T.Add_Test_Routine (Test_Status'Access, "Process Status");

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
