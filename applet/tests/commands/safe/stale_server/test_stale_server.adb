with
     Shell.Commands.Safe,
     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Test_Stale_Server
--
-- Run with a server on the PATH which doesn't match this program, from one of the folders here. A caller
-- must get a 'Command_Error' saying what to do, and the program must still end. A failure raises, or hangs.
--
is
   use Shell.Commands,
       Shell.Commands.Safe;


   procedure Check_Is_Told
   is
   begin
      Run ("true");
      raise Program_Error with "A server which doesn't match should raise.";
   exception
      when E : Command_Error =>
         if Ada.Strings.Fixed.Index (Ada.Exceptions.Exception_Message (E), "install_spawn_server.sh") = 0
         then
            raise Program_Error with "Unexpected message: " & Ada.Exceptions.Exception_Message (E);
         end if;
   end Check_Is_Told;

begin
   Check_Is_Told;
   Ada.Text_IO.Put_Line ("A caller is told of a server which doesn't match passed.");

   Check_Is_Told;
   Ada.Text_IO.Put_Line ("A later caller is told as well passed.");

   Ada.Text_IO.Put_Line ("End test.");
end Test_Stale_Server;
