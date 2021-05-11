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
      for i in 1 .. 10_000
      loop
         declare
            use Shell,
                Shell.Commands,
                Shell.Commands.Forge;
            The_Command :          Command := To_Command ("ls /home");
            Output      : constant String  := +Output_Of (Safe.Run (The_Command, Retry => Natural'Last));
         begin
            Put_Line ("Task 1   i =" & i'Image & " =>");
            Put_Line ("'" & Output & "'");

            if Output = ""
            then
               raise Program_Error with "Task 1: NO OUTPUT";
            end if;
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
      for i in 1 .. 10_000
      loop
         declare
            use Shell,
                Shell.Commands,
                Shell.Commands.Forge;
            The_Command :          Command :=  To_Command ("pwd");
            Output      : constant String  := +Output_Of (Safe.Run (The_Command, Retry => Natural'Last));
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
   delay 5.0 * 60.0;   -- Allow time to check for open pipes and zombie processes.
end Test_Concurrent_Commands;
