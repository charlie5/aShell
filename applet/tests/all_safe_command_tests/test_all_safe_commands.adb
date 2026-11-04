with
     Test_Command_Input,
     Test_Concurrent_Commands,
     Test_Concurrent_Pipelines,
     Test_Safe_Pipeline_Output,
     Test_Spawn_Server,

     Ada.Text_IO;


procedure Test_All_Safe_Commands
--
-- An argument is passed on to each test, as the number of times to loop, for a shorter run.
-- Without one, this takes hours.
--
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin all safe command tests.");

   New_Line (2);
   Test_Command_Input;

   New_Line (2);
   Test_Concurrent_Commands;

   New_Line (2);
   Test_Concurrent_Pipelines;

   New_Line (2);
   Test_Safe_Pipeline_Output;

   New_Line (2);
   Test_Spawn_Server;

   New_Line (2);
   Put_Line ("End all safe command tests.");
end Test_All_Safe_Commands;
