with
     Shell.Commands.Unsafe,
     Ada.Text_IO;

procedure Test_Nonblocking_Pipe
is
   -- To do this test, run this executable in one terminal, open
   -- another terminal and start 'cat' (ie  $ cat - >> test.log).
   -- Then, type any string into the 'cat' terminal. Each entered
   -- string should then appear in the 'Test_Nonblocking_Pipe' terminal.
   --
   -- Entering 'end' on the 'cat' terminal will terminate the test.

   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Nonblocking_Pipe' test.");
   New_Line (2);

   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Unsafe;
      The_Command : Unsafe.Command := Forge.To_Command ("tail -f test.log");
   begin
      Start (The_Command);

      loop
         declare
            Results : constant Command_Results :=  Results_Of (The_Command);
            Output  : constant String          := +Output_Of  (Results);
         begin
            put_line ("'" & Output & "'");
            delay 1.0;
            exit when          Output'Length >= 3
                      and then Output (1..3)  = "end";
         end;
      end loop;
   end;

   New_Line (2);
   Put_Line ("End 'Nonblocking_Pipe' test.");
end Test_Nonblocking_Pipe;
