with
     Test_Ashell,
     Test_Command_Error,
     Test_Command_Output_To_String,
     Test_Environment,
     Test_Iterate_Directory,
     Test_Piped_Processes,
     Test_Pipeline_Error,
     Test_Pipeline_Output,
     Test_Pipe_Output_To_String,
     Test_Wait_On_Process,
     Ada.Text_IO;

procedure Test_All
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin all tests.");
   New_Line (2);

   Test_Ashell;
   Test_Command_Error;
   Test_Command_Output_To_String;
   Test_Environment;
   Test_Iterate_Directory;
   Test_Piped_Processes;
   Test_Pipeline_Output;
   Test_Pipeline_Error;
   Test_Pipe_Output_To_String;
   Test_Wait_On_Process;

   New_Line (2);
   Put_Line ("End all tests.");
end Test_All;
