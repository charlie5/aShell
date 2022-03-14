with
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_aShell
is
   procedure Log (Message : in String)
                  renames Ada.Text_IO.Put_Line;
   procedure NL  (Count : in Ada.Text_IO.Positive_Count := 1)
                  renames Ada.Text_IO.New_Line;

   Error : exception;
begin
   Log ("Begin aShell tests.");

   NL (2);
   Log ("Test 1 ~ Run single command =>'ls -alh'");

   Test_1:
   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Unsafe.Command := To_Command ("ls -alh");
   begin
      Start   (The_Command);
      Wait_On (The_Command);
      Log (+Output_of (Results_Of (The_Command)));
   end Test_1;


   NL (5);
   Log ("Test 2 ~ Run piped commands => 'ls -alh | wc'");
   Test_2:
   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      Piped_Commands : Command_Array       := To_Commands ("ls -alh | wc");
      Last_Command   : Unsafe.Command renames Piped_Commands (Piped_Commands'Last);
   begin
      Start (Piped_Commands);
      Wait_On (Last_Command);

      if not Piped_Commands (1).Has_Terminated
      then
         raise Error with "Test 2 ~ Run piped commands failed ~ the first command has not terminated.";
      end if;

      Log (+Output_of (Results_Of (Last_Command)));
   end Test_2;


   NL (5);
   Log ("Test 3 ~ Check pid of a running process => 'sleep 3'");
   Test_3:
   declare
      use Shell,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Command := To_Command ("sleep 3");
   begin
      Start    (The_Command);
      Log ("Sleep process id: " & Image (The_Command.Process.all));
      Wait_On  (The_Command);
   end Test_3;


   NL (5);
   Log ("Test 4 ~ Check pid of running processes => 'sleep 3'");
   Test_4:
   declare
      use Shell,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Commands : Command_Array := To_Commands ("sleep 3 | sleep 3");
   begin
      Start (The_Commands, Pipeline => False);

      for i in The_Commands'Range
      loop
         Log (  "Sleep command"   & Positive'Image (i)
                   & " ~ process id: " & Image (The_Commands (i).Process.all));
      end loop;

      for i in The_Commands'Range
      loop
         Wait_On (The_Commands (i));
      end loop;
   end Test_4;


   NL (2);
   Log ("End aShell tests.");
end Test_aShell;
