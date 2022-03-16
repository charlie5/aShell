with
     POSIX.Configurable_System_Limits,
     Shell,
     Ada.Text_IO;


procedure Test_Max_Pipes
is
   procedure Log (Message : in String)
                  renames Ada.Text_IO.Put_Line;
   procedure NL  (Count   : in Ada.Text_IO.Positive_Count := 1)
                  renames Ada.Text_IO.New_Line;

   Max_Pipes : constant Natural := POSIX.Configurable_System_Limits.Open_Files_Maximum;

   Pipes     : array (1 .. Max_Pipes) of Shell.Pipe;
   Count     : Natural := 0;

   procedure Close_All
   is
   begin
      for i in 1 .. Count
      loop
         Shell.Close (Pipes (i));
      end loop;
   end Close_All;

begin
   NL;
   Log ("Begin max pipes test.");
   Log ("Max pipes:" & Max_Pipes'Image);

   loop
      declare
         use Shell;
         P : constant Shell.Pipe := To_Pipe;
      begin
         Count         := Count + 1;
         Pipes (Count) := P;
         exit when Count = Pipes'Last;
      end;
   end loop;

   Log ("All" & Pipes'Length'Image & " pipes opened successfuly.");
   Close_All;
   NL;

exception
   when Shell.Too_Many_Pipes_Error =>
      Close_All;

      Log ("Opened" & Count'Image & " pipes.");
      Log ("End max pipes test.");
      NL;

   when others =>
      Close_All;
      NL;
      raise;
end Test_Max_Pipes;
