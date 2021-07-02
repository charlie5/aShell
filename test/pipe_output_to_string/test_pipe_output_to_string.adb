with
     Shell,
     Ada.Text_IO;


procedure Test_Pipe_Output_To_String
--
-- Starts a long process (ls -alhR /usr/share/doc) and periodically reads/prints the processes output.
--
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Pipe_Output_To_String' test.");
   New_Line (2);

   declare
      use Shell;
      ls_Pipe : constant Shell.Pipe := To_Pipe;
      ls      : Shell.Process       := Start (Program   => "ls",
                                              Arguments => (1 => +"-alhR",
                                                            2 => +"/usr/share/doc"),
                                              Output    => ls_Pipe);
   begin
      loop
         declare
            Output : constant String := +Output_Of (ls_Pipe);
         begin
            delay 0.1;                       -- Allow time to elapse so the process can pump the pipe with plenty of output.
            Put_Line ("'" & Output & "'");   -- The 'To_String' function reads any output from the pipe as a String.
         end;

         if Has_Terminated (ls)
         then
            Put_Line ("'" & (+Output_Of (ls_Pipe)) & "'");   -- Show any final output.
            exit;
         end if;
      end loop;

      Close (ls_Pipe);
   end;

   New_Line (2);
   Put_Line ("End 'Pipe_Output_To_String' test.");
end Test_Pipe_Output_To_String;
