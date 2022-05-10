with
     Shell,
     Ada.Text_IO;


procedure Test_Pause_Resume
is
   use Ada.Text_IO;
begin
   Shell.Open_Log ("my_Log.txt");

   Put_Line ("Begin 'Test_Pause_Resume' test.");
   New_Line (2);

   declare
      use Shell;

      --  Sleep : Shell.Process := Start (Program   => "sleep",
      --                                  --  Arguments => (1 => (+"100000")));
      --                                  Arguments => (1 => (+"15")));

      Sleep : Shell.Process := Start (Program   => "ls",
                                      Arguments => (1 => (+"-R"),
                                                    2 => (+"/")));
   begin
      for i in 1 .. 1 -- _000_000
      loop
         --  Put_Line ("Pausing  process ('sleep 3') ...");
         delay 1.02;
         Put_Line (i'Image & Image (Sleep));
         Pause (Sleep);
         delay 3.02;

         --  Put_Line ("a1");
         --  if not Has_Terminated (Sleep)
         --  then
         --     raise Program_Error with "not Has_Terminated:" & i'Image;
         --  end if;

         --  Put_Line ("Has terminated: " & Has_Terminated (Sleep)'Image & ".");
         --  Put_Line ("Normal exit:    " & Normal_Exit    (Sleep)'Image & ".");
         --  Put_Line ("Exit status:    " & Exit_Status_Of (Sleep.Status))

         Put_Line ("Resuming process ('sleep 3') ...");
         Resume (Sleep);

         --  Put_Line ("a2");
         --  delay 0.02;

         --  if Has_Terminated (Sleep)
         --  then
         --     raise Program_Error with "Has_Terminated:" & i'Image;
         --  end if;

         Put_Line ("Waiting");
         Wait_On (Sleep);

         --  Put_Line ("Normal exit: " & Normal_Exit (Sleep)'Image & ".");
      end loop;
   end;

   New_Line (2);
   Put_Line ("End 'Test_Pause_Resume' test.");

   shell.Close_Log;
end Test_Pause_Resume;
