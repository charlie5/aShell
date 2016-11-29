with
     Shell,
     Ada.Text_IO;


procedure Test_aShell
is
   use Ada.Text_IO;
begin
   Put_Line ("Start tests.");

   new_Line (2);
   Put_Line ("Test 1 ~ Run single command =>'ls -alh'");
   Test_1:
   declare
      use Shell;
      The_Command : constant Command := To_Command ("ls -alh");
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


   new_Line (5);
   Put_Line ("Test 3 ~ Check pid of a running process => 'sleep 3'");
   Test_3:
   declare
      use Shell;
      The_Command : constant Command       := To_Command ("sleep 3");
      The_Process : constant Shell.Process := Run (The_Command);
   begin
      Put_Line ("Sleep process id: " & Image (The_Process));
   end Test_3;


   new_Line (5);
   Put_Line ("Test 4 ~ Check pid of running processes => 'sleep 3'");
   Test_4:
   declare
      use Shell;
      The_Commands  :          Command_Array       := To_Commands ("sleep 3 | sleep 3");
      The_Processes : constant Shell.Process_Array := Run (The_Commands, Pipeline => False);
   begin
      for i in The_Processes'Range
      loop
         Put_Line ("Sleep command" & Positive'Image (i) & " ~ process id: " & Image (The_Processes (i)));
      end loop;
   end Test_4;


   New_Line (2);
   Put_Line ("End tests.");
end Test_aShell;
