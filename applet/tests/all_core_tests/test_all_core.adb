with
     Test_Piped_Processes,
     Test_Pipe_Output_To_String,
     Test_Wait_On_Process,

     Shell.Testing,

     Ada.Text_IO;


procedure Test_All_Core
is
   use Shell.Testing,
       Ada.Text_IO;
begin
   Clear_Screen;
   Put_Line ("Begin all core tests.");

   --- Core
   --
   New_Line (2);
   Test_Piped_Processes;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Pipe_Output_To_String;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Wait_On_Process;

   New_Line (2);
   Put_Line ("End all core tests.");

end Test_All_Core;
