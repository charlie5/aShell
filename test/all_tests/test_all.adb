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

   New_Line (2);
   Test_Command_Error;

   New_Line (2);
   Test_Command_Output_To_String;

   New_Line (2);
   Test_Environment;

   New_Line (2);
   Test_Iterate_Directory;

   New_Line (2);
   Test_Piped_Processes;

   New_Line (2);
   Test_Pipeline_Output;

   New_Line (2);
   Test_Pipeline_Error;

   New_Line (2);
   Test_Pipe_Output_To_String;

   New_Line (2);
   Test_Wait_On_Process;

   New_Line (2);
   Put_Line ("End all tests.");
end Test_All;
