with
     Shell.Commands.Safe,
     Ada.Command_Line,
     Ada.Text_IO,
     Ada.Exceptions;

procedure Test_Concurrent_Pipelines
is
   use Ada.Text_IO,
       Ada.Exceptions;

   -- The optional argument is the number of times each task loops, for a shorter test.
   --
   Iterations : constant Positive := (if Ada.Command_Line.Argument_Count >= 1 then Positive'Value (Ada.Command_Line.Argument (1))
                                                                              else 5_000);

   task      Task_1;
   task body Task_1
   is
   begin
      for i in 1 .. Iterations
      loop
         declare
            use Shell,
                Shell.Commands,
                Shell.Commands.Safe,
                Shell.Commands.Safe.Forge;
            Commands : Safe.Command_Array := To_Commands ("ls -alh | grep test_concurrent_pipelines");
         begin
            declare
               Output : constant String := +Output_Of (Safe.Run (Commands));
            begin
               Put_Line ("Task 1   i =" & i'Image & " =>");
               Put_Line ("'" & Output & "'");

               if Output = ""
               then
                  raise Program_Error with "Task 1: NO OUTPUT";
               end if;
            end;
         end;
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
      for i in 1 .. Iterations
      loop
         declare
            use Shell,
                Shell.Commands,
                Shell.Commands.Safe,
                Shell.Commands.Safe.Forge;
            Commands : Safe.Command_Array :=  To_Commands ("ps -Af | wc"); -- grep test_concurrent_pipelines");
            Output   : constant String    := +Output_Of (Safe.Run (Commands));
         begin
            Put_Line ("Task 2   i =" & i'Image & " =>");
            Put_Line ("'" & Output & "'");

            if Output = ""
            then
               raise Program_Error with "Task 2: NO OUTPUT";
            end if;
         end;
      end loop;

   exception
      when E : others =>
         Put_Line ("Task 2: Fatal Error");
         Put_Line (Exception_Information (E));
   end Task_2;


begin
   loop
      exit when Task_1'Terminated
            and Task_2'Terminated;
      delay 0.1;
   end loop;

   if Ada.Command_Line.Argument_Count = 0
   then
      delay 160.0 * 60.0;   -- Allow time to check for open pipes and zombie processes.
   end if;

   Shell.Commands.Safe.Stop_Spawn_Client;

   Put_Line ("End 'Concurrent_Pipelines' test.");
end Test_Concurrent_Pipelines;
