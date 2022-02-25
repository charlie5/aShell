with
     Shell.Commands.Safe,
     Ada.Text_IO;

procedure Test_Safe_Pipeline_Output
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Pipeline_Output' test.");
   New_Line (2);

   for i in 1 .. 10
   loop
   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      --  Commands : Command_Array   :=  To_Commands ("ps -A | grep bash | wc");
      Commands : Command_Array   :=  To_Commands ("ps -A | grep bash");
      Output   : constant String := +Output_Of (Run (Commands));
   begin
      Put_Line ("'" & Output & "'");
   end;
   end loop;

   New_Line (2);
   Put_Line ("End 'Pipeline_Output' test.");


   delay 5 * 60.0;

   Shell.Commands.Safe.Stop_Spawn_Client;
end Test_Safe_Pipeline_Output;
