with
     POSIX.Configurable_System_Limits,
     Shell,
     Ada.Text_IO;


procedure Test_Max_Processes
is
   procedure Log (Message : in String)
                  renames Ada.Text_IO.Put_Line;
   procedure NL  (Count   : in Ada.Text_IO.Positive_Count := 1)
                  renames Ada.Text_IO.New_Line;

   Max_Child_Processes : constant Natural := POSIX.Configurable_System_Limits.Child_Processes_Maximum;

   Processes : Shell.Process_Array (1 .. Max_Child_Processes);
   Count     : Natural := 0;

   procedure Kill_All
   is
   begin
      for i in 1 .. Count
      loop
         Shell.Kill (Processes (i));
      end loop;
   end Kill_All;

begin
   NL;
   Log ("Begin max processes test.");
   Log ("Child processes maximum:" & Max_Child_Processes'Image);

   loop
      declare
         use Shell;

         Sleep : constant Shell.Process := Start (Program   => "sleep",
                                                  Arguments => (1 => (+"300")));
      begin
         Count             := Count + 1;
         Processes (Count) := Sleep;
         exit when Count = Processes'Last;
      end;
   end loop;

   Log ("All" & Processes'Length'Image & " processes started successfuly.");
   Kill_All;
   NL;

exception
   when Shell.Too_Many_Processes_Error =>
      Kill_All;

      Log ("Started" & Count'Image & " processes.");
      Log ("End max processes test.");
      NL;

   when others =>
      NL;
      Kill_All;
      raise;
end Test_Max_Processes;
