with Ada.Text_IO;

with Shell;
with Shell.Environment;

procedure Environment_Test_Runner
is
   use Ada.Text_IO;
begin
   Put_Line ("Start tests.");

   New_Line (2);
   Put_Line ("Test 1 ~ Run piped commands => 'env | grep aShell_Test_Variable'");
   Test_1:
   declare
      use Shell;
      Piped_Commands : Command_Array := To_Commands ("env | grep aShell_Test_Variable");
   begin
      Shell.Environment.Set (Name  => "aShell_Test_Variable",
                             Value => "Working!");
      Run (Piped_Commands);
      delay 1.0;
   end Test_1;

   New_Line (2);
   Put_Line ("End tests.");
end Environment_Test_Runner;
