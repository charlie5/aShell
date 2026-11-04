with
     Shell.Commands.Safe,
     Ada.Real_Time,
     Ada.Text_IO;


procedure Test_Stopped_Program
--
-- A program stopped from outside the library, as if by Ctrl-Z, must not hold up the
-- other commands, nor stop this program from ending. Any failure raises, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe,
       Ada.Real_Time;


   procedure Check (Test : in String;
                    Ok   : in Boolean)
   is
   begin
      if not Ok
      then
         raise Program_Error with Test & " failed.";
      end if;

      Ada.Text_IO.Put_Line (Test & " passed.");
   end Check;


   Left_Stopped : Safe.Command := Forge.To_Command ("bash stops_itself.sh");
   Stops_Later  : Safe.Command := Forge.To_Command ("bash stops_later.sh");

begin
   -- Resuming a program which appears to be running.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("bash stops_itself.sh");
   begin
      The_Command.Start;
      delay 0.3;

      The_Command.Resume;
      The_Command.Wait_On;

      Check ("A stopped program can be resumed", +Output_Of (Results_Of (The_Command)) = "Resumed" & ASCII.LF);
   end;


   -- Pausing a stopped program must not stall the server.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("bash stops_itself.sh");
   begin
      The_Command.Start;
      delay 0.3;

      The_Command.Pause;
      delay 0.1;

      declare
         Before : constant Time   := Clock;
         Output : constant String := +Output_Of (Run ("echo Alive"));
      begin
         Check ("Other commands run after pausing a stopped program",     Output = "Alive" & ASCII.LF
                                                                      and To_Duration (Clock - Before) < 2.0);
      end;

      The_Command.Resume;
      The_Command.Wait_On;

      Check ("A stopped program which was paused can be resumed", The_Command.Normal_Exit);
   end;


   -- These must not stop this program from ending. One is
   -- stopped already, and the other will stop during the shutdown.
   --
   Left_Stopped.Start;
   Stops_Later .Start;
   delay 0.1;

   Ada.Text_IO.Put_Line ("Main ending.");
end Test_Stopped_Program;
