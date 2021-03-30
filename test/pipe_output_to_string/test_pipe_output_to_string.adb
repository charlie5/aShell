with
     Shell,
     Ada.Text_IO;


procedure Test_Pipe_Output_To_String
--
-- Starts a long process (ls -alhR /) and periodically reads/prints the processes output.
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
                                                            2 => +"/etc"),
                                              Output    => ls_Pipe) with Unreferenced;
   begin
      for i in 1 .. 1000
      loop
         declare
            Output : constant String := +Output_Of (ls_Pipe);
         begin
            if Output = ""
            then
               exit;
            end if;

            delay 0.1;                       -- Allow time to elapse so the process can pump the pipe with plenty of output.
            Put_Line ("'" & Output & "'");   -- The 'To_String' function reads any output from the pipe as a String.
         end;
      end loop;
   end;

   New_Line (2);
   Put_Line ("End 'Pipe_Output_To_String' test.");
end Test_Pipe_Output_To_String;
