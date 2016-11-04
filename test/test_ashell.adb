with
     Shell,
     Ada.Text_IO;

procedure Test_aShell
is
   use Ada.Text_IO;
begin
   Put_Line ("Start test.");

   Test_1:
   declare
      use Shell;
      Piped_Commands : Command_Array := to_Commands ("ls | wc | cat");
   begin
      Run (Piped_Commands);
      delay 1.0;
   end Test_1;

   Put_Line ("End test.");
end Test_aShell;
