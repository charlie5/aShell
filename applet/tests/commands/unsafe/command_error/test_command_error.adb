with
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_Command_Error
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Command_Error' test.");
   New_Line (2);

   declare
      use Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Command := To_Command ("ls /non_existent_file");
   begin
      Run (The_Command);

      if The_Command.Failed
      then
         Put_Line ("Failed on command 'ls /non_existent_file' as expected.");
      end if;
   end;

   New_Line (2);

   declare
      use Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Command := To_Command ("ls /non_existent_file");
   begin
      Run (The_Command, Raise_Error => True);

   exception
      when Shell.Commands.Command_Error =>
         Put_Line ("Command failed and raised an exception, as expected.");
         Put_Line ("Failed command was 'ls /non_existent_file'.");
   end;

   New_Line (2);
   Put_Line ("End 'Command_Error' test.");
end Test_Command_Error;
