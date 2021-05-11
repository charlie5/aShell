with
     Shell.Commands,
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
          Shell.Commands.Forge;
      The_Command :          Command :=  To_Command ("ls -alh");
      Output      : constant String  := +Output_Of (Run (The_Command));
   begin
      Put_Line ("'" & Output & "'");
   end;

   New_Line (2);
   Put_Line ("End 'Command_Output_To_String' test.");
end Test_Command_Output_To_String;
