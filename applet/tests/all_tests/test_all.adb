with
     --- Core
     --
     Test_Max_Pipes,
     Test_Max_Processes,
     Test_Piped_Processes,
     Test_Pipe_Output_To_String,
     Test_Wait_On_Process,

     --- Commands - Safe
     --
     Test_Command_Input,
     Test_Concurrent_Commands,
     Test_Concurrent_Pipelines,
     Test_Safe_Pipeline_Output,
     Test_Spawn_Server,

     --- Commands - Unsafe
     --
     Test_Ashell,
     Test_Command_Error,
     Test_Unsafe_Command_Status,
     Test_Unsafe_Command_Input,
     Test_Command_Output_To_String,
     Test_Environment,
     Test_Nonblocking_Pipe,
     Test_Pipeline_Error,
     Test_Pipeline_Output,

     --- Directory
     --
     Test_Iterate_Directory,

     Shell.Commands.Safe,
     Ada.Text_IO;


procedure Test_All
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin all tests.");

   --- Core
   --
   New_Line (2);
   Test_Max_Pipes;

   New_Line (2);
   --  Test_Max_Processes;

   New_Line (2);
   Test_Piped_Processes;

   New_Line (2);
   Test_Pipe_Output_To_String;

   New_Line (2);
   Test_Wait_On_Process;



   --- Commands - Safe
   --
   New_Line (2);
   --  Test_Command_Input;

   New_Line (2);
   --  Test_Concurrent_Commands;

   New_Line (2);
   --  Test_Concurrent_Pipelines;

   New_Line (2);
   --  Test_Safe_Pipeline_Output;

   New_Line (2);
   --  Test_Spawn_Server;



   --- Commands - Unsafe
   --
   New_Line (2);
   Test_Ashell;

   New_Line (2);
   Test_Command_Error;

   New_Line (2);
   Test_Unsafe_Command_Input;

   New_Line (2);
   Test_Unsafe_Command_Status;

   New_Line (2);
   Test_Command_Output_To_String;

   New_Line (2);
   Test_Environment;

   New_Line (2);
   --  Test_Nonblocking_Pipe;

   New_Line (2);
   Test_Pipeline_Error;

   New_Line (2);
   --  Test_Pipeline_Output;


   --- Directory
   --
   New_Line (2);
   Test_Iterate_Directory;


   Shell.Commands.Safe.Stop_Spawn_Client;

   New_Line (2);
   Put_Line ("End all tests.");
end Test_All;
