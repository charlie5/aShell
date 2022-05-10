with
     Shell.Commands.Safe,
     Ada.Text_IO;


procedure Test_Safe_Command_Status
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Test_Safe_Command_Status'.");


   declare
      use Shell.Commands,
          Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      The_Command : Safe.Command := To_Command ("sleep 5");
   begin
      if The_Command.Status /= Not_Started
      then
         raise Command_Error with "Command status should be 'Not_Started'.";
      end if;


      Put_Line ("Starting command.");
      The_Command.Start;

      if The_Command.Status /= Running
      then
         raise Command_Error with "Command status should be 'Running'.";
      end if;

delay 0.5;
      Put_Line ("Pausing command.");
      The_Command.Pause;

      if The_Command.Status /= Paused
      then
         raise Command_Error with "Command status should be 'Paused'.";
      end if;


      --  Put_Line ("Resuming command.");
      --  The_Command.Resume;
      --
      --  if The_Command.Status /= Running
      --  then
      --     raise Command_Error with "Command status should be 'Running'.";
      --  end if;
      --
      --
      --  Put_Line ("Waiting on command.");
      --  The_Command.Wait_On;
      --
      --  if The_Command.Status /= Normal_Exit
      --  then
      --     raise Command_Error with "Command status should be 'Normal_Exit'.";
      --  end if;
   end;


   Put_Line ("Run command.");
   declare
      use Shell.Commands,
          Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      The_Command : Safe.Command := To_Command ("ls /non_existant_file");
   begin
      Put_Line ("Running command.");
      The_Command.Run;

      if The_Command.Status /= Failed_Exit
      then
         raise Command_Error with "Command status should be 'Failed_Exit'.";
      end if;
   end;


   declare
      use Shell.Commands,
          Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      The_Command : Safe.Command := To_Command ("sleep 5");
   begin
      The_Command.Start;
      The_Command.Kill;

      if The_Command.Status /= Killed
      then
         raise Command_Error with "Command status should be 'Killed'.";
      end if;
   end;


   Shell.Commands.Safe.Stop_Spawn_Client;
   Put_Line ("End   'Test_Safe_Command_Status'.");
end Test_Safe_Command_Status;
