with
     Shell,
     Ada.Text_IO;


procedure Test_Paused_Process_Status
is
   use Ada.Text_IO;
   Count : Natural := 0;
   Error : exception;
begin
   Put_Line ("Begin 'Test_Paused_Process_Status'.");


   Count := Count + 1;
   Put_Line ("Test" & Count'Image);
   declare
      use Shell;
      The_Process : Process;
   begin
      if Status (The_Process) /= Not_Started
      then
         raise Error with "Process status should be 'Not_Started'.";
      end if;
   end;


   Count := Count + 1;
   New_Line (3);
   Put_Line ("Test" & Count'Image);
   declare
      use Shell;
      The_Process : Process;
   begin
      Start ("pwd", Process => The_Process);
      Start ("pwd", Process => The_Process);
      raise Error with "Process started twice and no exception raised.";
   exception
      when Shell.Process_Already_Started => null;
   end;


   Count := Count + 1;
   New_Line (3);
   Put_Line ("Test" & Count'Image);
   declare
      use Shell;
      The_Process : Process := Start (Program   => "sleep",
                                      Arguments => (1 => (+"3")));
   begin
      Put_Line ("Waiting.");
      Wait_On (The_Process);

      if Status (The_Process) /= Normal_Exit
      then
         raise Error with "Process status should be 'Normal_Exit' instead of " & Status (The_Process)'Image & ".";
      end if;

      Put_Line ("Done.");
   end;


   Count := Count + 1;
   New_Line (3);
   Put_Line ("Test" & Count'Image);
   declare
      use Shell;
      The_Process : Process := Start (Program   => "sleep",
                                      Arguments => (1 => (+"3")));
   begin
      Put_Line ("Started.");

      if Status (The_Process) /= Running
      then
         raise Error with "Process status should be 'Running'.";
      end if;


      Put_Line ("Pausing.");
      Pause (The_Process);

      if Status (The_Process) /= Paused
      then
         raise Error with "Process status should be 'Paused'.";
      end if;

      begin
         Pause (The_Process);
         raise Error with "Process paused twice.";
      exception
         when Process_Already_Paused =>
            null;
      end;

      Put_Line ("Resuming.");
      Resume (The_Process);

      if Status (The_Process) /= Running
      then
         raise Error with "Process status should be 'Running'.";
      end if;

      Put_Line ("Waiting.");
      Wait_On (The_Process);

      if Status (The_Process) /= Normal_Exit
      then
         raise Error with "Process status should be 'Normal_Exit' instead of " & Status (The_Process)'Image & ".";
      end if;

      Put_Line ("Done.");
   end;


   Count := Count + 1;
   New_Line (3);
   Put_Line ("Test" & Count'Image);
   declare
      use Shell;
      The_Process : Process := Start (Program   => "ls",
                                      Arguments => (1 => (+"/non_existant_file")));
   begin
      Put_Line ("Started.");
      Put_Line ("Waiting.");
      Wait_On (The_Process);

      if Status (The_Process) /= Failed_Exit
      then
         raise Error with "Process status should be 'Failed_Exit' instead of " & Status (The_Process)'Image & ".";
      end if;
   end;


   Count := Count + 1;
   New_Line (3);
   Put_Line ("Test" & Count'Image);
   declare
      use Shell;
      The_Process : Process := Start (Program   => "sleep",
                                      Arguments => (1 => (+"3")));
   begin
      Put_Line ("Started.");
      Put_Line ("Killing.");
      Kill (The_Process);

      if Status (The_Process) /= Killed
      then
         raise Error with "Process status should be 'Killed' instead of " & Status (The_Process)'Image;
      end if;
   end;


   New_Line;
   Put_Line ("End 'Test_Paused_Process_Status'.");
end Test_Paused_Process_Status;
