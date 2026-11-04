with
     Shell.Commands.Safe,
     Ada.Text_IO;


procedure Test_Many_Commands
--
-- The server's descriptors will number more than 1024, which 'select' could not handle. If
-- the limit on open files is too low for that, those commands must simply fail to start.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;

   Normal : Natural := 0;

   Sleepers : Command_Array := [for Each in 1 .. 600 => Forge.To_Command ("sleep 3")];
begin
   for Each of Sleepers
   loop
      Each.Start;
   end loop;

   delay 1.0;

   if +Output_Of (Run ("echo Alive")) /= "Alive" & ASCII.LF
   then
      raise Program_Error with "A command alongside many others failed.";
   end if;

   for Each of Sleepers
   loop
      Each.Wait_On;

      if Each.Normal_Exit
      then
         Normal := Normal + 1;
      end if;
   end loop;

   Ada.Text_IO.Put_Line (Normal'Image & " of 600 ended normally.");
   Ada.Text_IO.Put_Line ("Commands all ended.");
end Test_Many_Commands;
