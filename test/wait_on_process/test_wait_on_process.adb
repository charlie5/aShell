with
     Shell,
     Ada.Text_IO;


procedure Test_Wait_On_Process
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Wait_On_Process' test.");
   New_Line (2);

   declare
      use Shell;

      Sleep : constant Shell.Process := Start (Program   => "sleep",
                                               Arguments => (1 => (+"3")));

   begin
      Put_Line ("Waiting on process ('sleep 3') ...");
      Wait_On (Sleep);
   end;

   New_Line (2);
   Put_Line ("End 'Wait_On_Process' test.");
end Test_Wait_On_Process;
