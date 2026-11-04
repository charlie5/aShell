with
     Shell.Commands.Safe,
     Ada.Text_IO;


procedure Test_Exit_With_Input
--
-- The program must end, even though a command is still waiting on its input. This fails by hanging.
--
is
   use Shell.Commands.Safe;

   The_Command : Command := Forge.To_Command ("cat");
begin
   The_Command.Start (Accepts_Input => True);

   Ada.Text_IO.Put_Line ("Main ending.");
end Test_Exit_With_Input;
