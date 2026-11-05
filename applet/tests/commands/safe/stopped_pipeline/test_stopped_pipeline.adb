with
     Shell.Commands.Safe,
     Ada.Text_IO;


procedure Test_Stopped_Pipeline
--
-- Stopping a pipeline closes pipes which the spawn server may have just found ready, as 'yes' keeps its output
-- pipe ready. Reading one would stop the server, and any failure of this test raises 'Command_Error'. Each loop is
-- slow, as the server reads all it is allowed to of what 'yes' keeps writing.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;
begin
   for Each in 1 .. 20
   loop
      declare
         The_Pipeline : Command_Array := Forge.To_Commands ("false | yes");
      begin
         Run (The_Pipeline);
      end;
   end loop;

   if +Output_Of (Run ("echo Still running")) /= "Still running" & ASCII.LF
   then
      raise Program_Error with "The server should still run commands.";
   end if;

   Ada.Text_IO.Put_Line ("End test.");
end Test_Stopped_Pipeline;
