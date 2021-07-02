with
     Shell.Commands.Safe,
     Ada.Text_IO;

procedure Test_Spawn_Server
--
--
--
is
   use Shell.Commands,
       Ada.Text_IO;

begin
   Shell.Open_Log ("aShell_spawn_Client.log");

   Put_Line ("Begin test.");
   New_Line (2);

   for i in 1 .. 100_000
   loop
      Put_Line ("Loop:" & i'Image);

      declare
         use Shell;
         The_Command   : Command := Forge.To_Command ("ls -alh");
      begin
         Safe.Runn (The_Command);
         Put_Line ("Output =>");
         Put_Line (+Output_Of (Results_Of (The_Command)));
         --  Safe.Runn (The_Command_2);
      end;

      delay 0.1;
   end loop;

   Safe.Stop_Spawn_Client;
   Shell.Close_Log;


   New_Line (2);
   Put_Line ("End test.");
end Test_Spawn_Server;
