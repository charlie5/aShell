with
     Shell.Commands.Safe,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Test_Command_Failure
--
-- A failure which belongs to one command must not stop the spawn server, or leave the caller waiting.
-- Any failure of this test raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;


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


   function Contains (Source  : in String;
                      Pattern : in String) return Boolean
   is (Ada.Strings.Fixed.Index (Source, Pattern) /= 0);


   Missing : constant String := "no_such_program_for_ashell";

begin
   -- A program which doesn't exist.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command (Missing);
      The_Results : constant Command_Results := Run (The_Command);
   begin
      Check ("Missing program fails",          The_Command.Failed);
      Check ("Missing program gives an error", Contains (+Errors_Of (The_Results), Missing));
   end;

   declare
      Raised : Boolean := False;
   begin
      begin
         Run (Missing);
      exception
         when Command_Error =>
            Raised := True;
      end;

      Check ("Missing program raises in 'Run' procedure", Raised);
   end;


   -- A pipeline containing a program which doesn't exist.
   --
   for Position in 1 .. 3
   loop
      declare
         Line : constant String := (case Position is
                                       when 1      => Missing & " | cat | cat",
                                       when 2      => "sleep 20 | " & Missing & " | cat",
                                       when others => "sleep 20 | cat | " & Missing);

         The_Pipeline : Command_Array := Forge.To_Commands (Line);
      begin
         Run (The_Pipeline);

         Check ("Missing program in pipeline position" & Position'Image & " is the one which failed",
                Which_Failed (The_Pipeline) = Position);
      end;
   end loop;


   -- Sending to a command which doesn't accept input.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("sleep 1");
   begin
      The_Command.Start;
      The_Command.Send (+"Unwanted.");
      The_Command.Wait_On;

      Check ("Unwanted input is ignored", The_Command.Normal_Exit);
   end;


   -- A program which catches the interrupt, then exits. The server must not wait for it to be 'Interrupted'.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("bash -c 'trap ""exit 3"" INT; sleep 20 & wait'");
   begin
      The_Command.Start;
      delay 0.5;
      The_Command.Interrupt;
      The_Command.Wait_On;

      Check ("Program which catches an interrupt fails", not The_Command.Normal_Exit);
   end;


   -- An early command with large error output.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash large_errors.sh | cat");
   begin
      Run (The_Pipeline);

      Check ("Early command's errors don't block 'Run'", Errors_Of (Results_Of (The_Pipeline (1)))'Length = 300_000);
   end;


   -- The server must still be running.
   --
   Check ("Server still runs commands", +Output_Of (Run ("echo Alive")) = "Alive" & ASCII.LF);
end Test_Command_Failure;
