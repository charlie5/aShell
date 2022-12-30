with
     Shell.Commands.Safe,
     Ada.Text_IO;


procedure Test_Unused_Safe_Command
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Unused_Safe_Command' test.");

   declare
      use Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      The_Command : Command := To_Command ("ls /non_existent_file") with Unreferenced;
   begin
      Stop_Spawn_Client;
   end;

   Put_Line ("Success.");
   Put_Line ("End   'Unused_Safe_Command' test.");
end Test_Unused_Safe_Command;
