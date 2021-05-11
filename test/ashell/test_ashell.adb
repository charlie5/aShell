with
     Shell.Commands,
     Ada.Text_IO;


procedure Test_aShell
is
   use Ada.Text_IO;
   Error : exception;
begin
   Put_Line ("Begin aShell tests.");

   new_Line (2);
   Put_Line ("Test 1 ~ Run single command =>'ls -alh'");

   Test_1:
   declare
      use Shell,
          Shell.Commands;
      The_Command : Command := To_Command ("ls -alh");
   begin
      Start (The_Command);
      Wait_On (Process_of (The_Command).all);
   end Test_1;


   new_Line (5);
   Put_Line ("Test 2 ~ Run piped commands => 'ls -alh | wc'");
   Test_2:
   declare
      use Shell,
          Shell.Commands;
      Piped_Commands : Command_Array := To_Commands ("ls -alh | wc");
      Last_Command   : Command  renames Piped_Commands (Piped_Commands'Last);
   begin
      Start (Piped_Commands);
      Wait_On (Process_of (Last_Command).all);

      if not Has_Terminated (Process_of (Piped_Commands (1)).all)
      then
         raise Error with "Test 2 ~ Run piped commands failed ~ the first command has not terminated.";
      end if;
   end Test_2;


   new_Line (5);
   Put_Line ("Test 3 ~ Check pid of a running process => 'sleep 3'");
   Test_3:
   declare
      use Shell,
          Shell.Commands;
      The_Command : Command := To_Command ("sleep 3");
   begin
      Start (The_Command);
      Put_Line ("Sleep process id: " & Image (Process_of (The_Command).all));
      Wait_On (Process_of (The_Command).all);
   end Test_3;


   new_Line (5);
   Put_Line ("Test 4 ~ Check pid of running processes => 'sleep 3'");
   Test_4:
   declare
      use Shell,
          Shell.Commands;
      The_Commands : Command_Array := To_Commands ("sleep 3 | sleep 3");
   begin
      Start (The_Commands, Pipeline => False);

      for i in The_Commands'Range
      loop
         Put_Line (  "Sleep command"   & Positive'Image (i)
                   & " ~ process id: " & Image (Process_of (The_Commands (i)).all));
      end loop;

      for i in The_Commands'Range
      loop
         Wait_On (Process_of (The_Commands (i)).all);
      end loop;
   end Test_4;


   New_Line (2);
   Put_Line ("End aShell tests.");
end Test_aShell;
