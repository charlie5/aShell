with
     Shell.Commands.Safe,
     Ada.Text_IO;

procedure Test_Spawn_Manager
--
--
--
is
   use Shell.Commands,
       Ada.Text_IO;

begin
   Put_Line ("Begin test.");
   New_Line (2);

   for i in 1 .. 1
   loop
      declare
         use Shell;
         The_Command   : Command := Forge.To_Command ("ls -alh");
         --  The_Command_2 : Command := Forge.To_Command ("pwd");
      begin
         Safe.Runn (The_Command);
         Put_Line ("Output =>");
         Put_Line (+Output_Of (Results_Of (The_Command)));
         --  Safe.Runn (The_Command_2);
      end;

      delay 0.1;
   end loop;

   New_Line (2);
   Put_Line ("End test.");
end Test_Spawn_Manager;
