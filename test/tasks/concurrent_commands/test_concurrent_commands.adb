with
     Shell.Commands.Safe,
     Ada.Text_IO,
     Ada.Exceptions;

procedure Test_Concurrent_Commands
is
   use Ada.Text_IO,
       Ada.Exceptions;

   task      Task_1;
   task body Task_1
   is
   begin
      for i in 1 .. 50_000
      loop
         declare
            use Shell,
                Shell.Commands,
                Shell.Commands.Safe,
                Shell.Commands.Safe.Forge;
            The_Command : Safe.Command := To_Command ("ls /home");
         begin
            Safe.Run (The_Command);

            declare
               Output : constant String  := +Output_Of (Results_Of (The_Command));
            begin
               Put_Line ("Task 1   i =" & i'Image & " => " & Output);

               if Output = ""
               then
                  raise Program_Error with "Task 1: NO OUTPUT";
               end if;
            end;
         end;

         delay 0.01;     -- Allow other task a turn.
      end loop;

   exception
      when E : others =>
         Put_Line ("Task 1: Fatal Error");
         Put_Line (Exception_Information (E));
   end Task_1;


   task      Task_2;
   task body Task_2
   is
   begin
      for i in 1 .. 50_000
      loop
         declare
            use Shell,
                Shell.Commands,
                Shell.Commands.Safe,
                Shell.Commands.Safe.Forge;
            The_Command : Safe.Command := To_Command ("pwd");
         begin
            Safe.Run (The_Command);

            declare
               Output : constant String  := +Output_Of (Results_Of (The_Command));
            begin
               Put_Line ("Task 2   i =" & i'Image & " => " & Output);

               if Output = ""
               then
                  raise Program_Error with "Task 2: NO OUTPUT";
               end if;
            end;
         end;

         delay 0.01;     -- Allow other task a turn.
      end loop;

   exception
      when E : others =>
         Put_Line ("Task 2: Fatal Error");
         Put_Line (Exception_Information (E));
   end Task_2;


begin
   Shell.Open_Log ("aShell_spawn_Client.log");

   loop
      exit when Task_1'Terminated
            and Task_2'Terminated;
      delay 0.1;
   end loop;

   New_Line (2);
   Put_Line ("Main Task has terminated.");

   Shell.Commands.Safe.Stop_Spawn_Client;

   delay 0.2;
   Shell.Close_Log;

   --  Put_Line ("Delaying for 5 minutes.");
   --  delay 25.0; --  * 60.0;   -- Allow time to check for open pipes and zombie processes.
end Test_Concurrent_Commands;
