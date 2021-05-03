with
     Shell.Commands,
     Ada.Text_IO,
     Ada.Exceptions;

procedure Test_Tasks
is
   use Ada.Text_IO,
       Ada.Exceptions;

   task      Task_0001;
   task body Task_0001
   is
   begin
      for i in 1 .. 40_000
      loop
         declare
            use Shell,
                Shell.Commands;
            Commands : Command_Array   :=  To_Commands ("ls -alh | grep test_task");
         begin
            declare
               Output : constant String := +Output_Of (Run (Commands));
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
   end Task_0001;


   task      Task_0002;
   task body Task_0002
   is
   begin
      for i in 1 .. 10_000
      loop
         declare
            use Shell,
                Shell.Commands;
            Commands : Command_Array   :=  To_Commands ("ps -A | grep test_task");
            Output   : constant String := +Output_Of (Run (Commands));
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
   end Task_0002;


begin
   declare
      use Shell,
          Shell.Commands;
      Commands : Command_Array   :=  To_Commands ("ls -alh | grep test_task");
      Output   : constant String := +Output_Of (Run (Commands));
   begin
      Put_Line ("Main Task =>");
      Put_Line ("'" & Output & "'");
   end;

   delay 20.0 * 60.0;

end Test_Tasks;