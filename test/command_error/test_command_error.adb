with
     Shell,
     Ada.Text_IO;

procedure Test_Command_Error
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Command_Error' test.");
   New_Line (2);

   declare
      use Shell;
      Command : Shell.Command := To_Command ("ls /non_existent_file");
   begin
      Run (Command);

      if Failed (Command)
      then
         Put_Line ("Failed on command '" & Name_of (Command) & "' as expected.");
      end if;
   end;

   New_Line (2);

   declare
      use Shell;
      Command : Shell.Command := To_Command ("ls /non_existent_file");
   begin
      Run (Command, Raise_Error => True);

   exception
      when Command_Error =>
         Put_Line ("Command failed and raised an exception, as expected.");
         Put_Line ("Failed command was '" & Name_of (Command) & "'.");
   end;

   New_Line (2);
   Put_Line ("End 'Command_Error' test.");
end Test_Command_Error;
