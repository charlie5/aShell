with
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_Unsafe_Command_Status
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Test_Unsafe_Command_Status'.");

   declare
      use Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Unsafe.Command := To_Command ("sleep 5");
   begin
      if The_Command.Status /= Not_Started
      then
         raise Command_Error with "Command status should be 'Not_Started'.";
      end if;


      The_Command.Start;

      if The_Command.Status /= Running
      then
         raise Command_Error with "Command status should be 'Running'.";
      end if;


      The_Command.Pause;

      if The_Command.Status /= Paused
      then
         raise Command_Error with "Command status should be 'Paused'.";
      end if;


      The_Command.Resume;

      if The_Command.Status /= Running
      then
         raise Command_Error with "Command status should be 'Running'.";
      end if;


      The_Command.Wait_On;

      if The_Command.Status /= Normal_Exit
      then
         raise Command_Error with "Command status should be 'Normal_Exit'.";
      end if;

   end;


   declare
      use Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Unsafe.Command := To_Command ("ls /non_existant_file");
   begin
      The_Command.Run;

      if The_Command.Status /= Failed_Exit
      then
         raise Command_Error with "Command status should be 'Failed_Exit'.";
      end if;
   end;


   declare
      use Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Unsafe.Command := To_Command ("sleep 5");
   begin
      The_Command.Start;
      The_Command.Kill;

      if The_Command.Status /= Killed
      then
         raise Command_Error with "Command status should be 'Killed'.";
      end if;
   end;

   Put_Line ("Status tests successful.");
   Put_Line ("End   'Test_Unsafe_Command_Status'.");
end Test_Unsafe_Command_Status;
