with
     Shell.Commands.Safe,
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_Mixed_Packages
--
-- A program can't use both, which raises Program_Error as it starts. So this must never run.
--
is
begin
   Ada.Text_IO.Put_Line ("Reached the main program.");
end Test_Mixed_Packages;
