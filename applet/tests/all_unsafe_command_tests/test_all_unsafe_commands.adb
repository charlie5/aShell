with
     Test_Ashell,
     Test_Command_Error,
     Test_Unsafe_Command_Status,
     Test_Unsafe_Command_Input,
     Test_Command_Output_To_String,
     Test_Environment,
     Test_Pipeline_Error,
     Test_Pipeline_Output,
     Test_Unused_Unsafe_Command,

     Shell.Testing,

     Ada.Text_IO;


procedure Test_All_Unsafe_Commands
is
   use Shell.Testing,
       Ada.Text_IO;
begin
   Clear_Screen;
   Put_Line ("Begin all unsafe command tests.");

   New_Line (2);
   Test_Ashell;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Command_Error;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Unsafe_Command_Input;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Unsafe_Command_Status;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Command_Output_To_String;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Environment;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Pipeline_Error;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Pipeline_Output;
   Pause_Til_Tester_Is_Ready;

   New_Line (2);
   Test_Unused_Unsafe_Command;


   New_Line (2);
   Put_Line ("End all unsafe command tests.");

end Test_All_Unsafe_Commands;
