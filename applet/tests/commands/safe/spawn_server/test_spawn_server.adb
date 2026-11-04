with
     Shell.Commands.Safe,
     Ada.Command_Line,
     Ada.Text_IO;

procedure Test_Spawn_Server
--
--
--
is
   use Shell.Commands,
       Ada.Text_IO;

   -- The optional argument is the number of times to loop, for a shorter test.
   --
   Iterations : constant Positive := (if Ada.Command_Line.Argument_Count >= 1 then Positive'Value (Ada.Command_Line.Argument (1))
                                                                              else 100_000);

begin
   Shell.Open_Log ("aShell_spawn_Client.log");

   Put_Line ("Begin test.");
   New_Line (2);

   for i in 1 .. Iterations
   loop
      Put_Line ("Loop:" & i'Image);

      declare
         use Shell;
         The_Command : Safe.Command := Safe.Forge.To_Command ("ls -alh");
      begin
         Safe.Run (The_Command);

         declare
            Output : constant String := +Output_Of (Results_Of (The_Command));
         begin
            Put_Line ("Output =>");
            Put_Line (Output);

            if Output = ""
            then
               raise Program_Error with "Fatal Error ~ No output.";
            end if;
         end;
      end;
   end loop;

   Shell.Close_Log;

   New_Line (2);
   Put_Line ("End test.");
end Test_Spawn_Server;
