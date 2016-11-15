with
     Shell,
     Ada.Text_IO;


procedure Test_aShell
is
   use Ada.Text_IO;
begin
   Put_Line ("Start tests.");

   Put_Line ("Test 1 ~ Run single command =>'ls -alh'");
   Test_1:
   declare
      use Shell;
      The_Command : Command := To_Command ("ls -alh");
   begin
      Run (The_Command);
      delay 1.0;
   end Test_1;


   new_Line (5);
   Put_Line ("Test 2 ~ Run piped commands => 'ls -alh | wc'");
   Test_2:
   declare
      use Shell;
      Piped_Commands : Command_Array := To_Commands ("ls -alh | wc");
   begin
      Run (Piped_Commands);
      delay 1.0;
   end Test_2;


   New_Line (2);
   Put_Line ("End tests.");
end Test_aShell;
