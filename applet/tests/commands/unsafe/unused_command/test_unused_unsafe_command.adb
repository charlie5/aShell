with
     Shell.Commands.Unsafe,
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

   New_Line;
   Put_Line ("Success.");
   New_Line;

   Put_Line ("End 'unused_unsafe_Command' test.");
end test_unused_unsafe_Command;
