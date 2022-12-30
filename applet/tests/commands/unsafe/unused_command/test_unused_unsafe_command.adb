with
     Shell.Commands.Unsafe,
     Shell.Commands.Safe,
     Ada.Text_IO;


procedure test_unused_unsafe_Command
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'unused_unsafe_Command' test.");

   declare
      use Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Command := To_Command ("ls /non_existent_file") with Unreferenced;
   begin
      null;
   end;

   declare
      use Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      The_Command : Command := To_Command ("ls /non_existent_file") with Unreferenced;
   begin
      Stop_Spawn_Client;
   end;

   Put_Line ("End 'unused_unsafe_Command' test.");
end test_unused_unsafe_Command;
