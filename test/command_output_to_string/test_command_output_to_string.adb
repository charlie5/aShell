with
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_Command_Output_To_String
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Command_Output_To_String' test.");
   New_Line (2);

   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command :          Unsafe.Command :=  To_Command ("ls -alh");
      Output      : constant String         := +Output_Of (The_Command.Run);
   begin
      Put_Line ("'" & Output & "'");
   end;

   New_Line (2);
   Put_Line ("End 'Command_Output_To_String' test.");
end Test_Command_Output_To_String;
