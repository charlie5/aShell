with
     Shell,
     Ada.Text_IO;


procedure Test_Pipe_Output_To_String
is
   use Ada.Text_IO;
begin
   Put_Line ("Start test.");
   New_Line (2);

   declare
      use Shell;
      ls_Pipe : constant Shell.Pipe := To_Pipe;
      ls      : Shell.Process       := Start (Program   => "ls",
                                              Arguments => (1 => +"-alh"),
                                              Output    => ls_Pipe);
   begin
      delay 1.0;
      Put_Line ("'" & To_String (ls_Pipe) & "'");
   end;

   New_Line (2);
   Put_Line ("End test.");
end Test_Pipe_Output_To_String;
