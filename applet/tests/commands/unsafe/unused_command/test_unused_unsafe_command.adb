with
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_Unused_Unsafe_Command
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Unused_Unsafe_Command' test.");

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

   Put_Line ("End 'Unused_Unsafe_Command' test.");
end Test_Unused_Unsafe_Command;
