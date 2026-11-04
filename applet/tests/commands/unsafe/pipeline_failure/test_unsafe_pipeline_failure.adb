with
     Shell.Commands.Unsafe,
     Ada.Directories,
     Ada.Text_IO;


procedure Test_Unsafe_Pipeline_Failure
--
-- Failing to start a pipeline must raise cleanly, stop any commands which did start, and leak no descriptors.
-- 'Run' must also read the error output of every command. Any failure raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Unsafe;


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


   function Open_Descriptors return Natural
   is
      use Ada.Directories;

      Search : Search_Type;
      Item   : Directory_Entry_Type;
      Count  : Natural := 0;
   begin
      Start_Search (Search, "/proc/self/fd", "");

      while More_Entries (Search)
      loop
         Get_Next_Entry (Search, Item);
         Count := Count + 1;
      end loop;

      End_Search (Search);
      return Count;
   end Open_Descriptors;


   Missing : constant String := "no_such_program_for_ashell";

begin
   -- An early command with large error output.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("bash large_errors.sh | cat");
   begin
      Run (The_Pipeline);

      Check ("Early command's errors don't block 'Run'", Errors_Of (Results_Of (The_Pipeline (1)))'Length = 300_000);
   end;


   -- A missing program, in each position.
   --
   for Position in 1 .. 2
   loop
      declare
         Line : constant String := (if Position = 1 then Missing & " | cat"
                                                    else "sleep 30 | " & Missing);
      begin
         declare
            The_Pipeline : Command_Array := Forge.To_Commands (Line);
         begin
            begin
               Start (The_Pipeline);
               Check ("Missing program raises", False);
            exception
               when Process_Error =>
                  Check ("Missing program raises", True);
            end;

            if Position = 2
            then
               Check ("Command which started is stopped", The_Pipeline (1).Has_Terminated);
            end if;
         end;

         Check ("Leaving the scope doesn't raise", True);
      end;
   end loop;


   -- A failed single command returns to not having been started.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command (Missing);
   begin
      begin
         The_Command.Start;
      exception
         when Process_Error =>
            null;
      end;

      Check ("Failed command isn't running", State'(The_Command.Status) = Not_Started);
   end;


   -- Repeated failures.
   --
   declare
      Before : constant Natural := Open_Descriptors;
   begin
      for Each in 1 .. 50
      loop
         declare
            The_Command  : Unsafe.Command := Forge.To_Command (Missing);
            The_Pipeline : Command_Array  := Forge.To_Commands (Missing & " | cat");
         begin
            begin
               The_Command.Start (Accepts_Input => True);
            exception
               when Process_Error =>
                  null;
            end;

            begin
               Start (The_Pipeline);
            exception
               when Process_Error =>
                  null;
            end;
         end;
      end loop;

      Check ("Failed starts leak no descriptors", Open_Descriptors = Before);
   end;


   -- A program which catches the interrupt, then exits.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("bash -c 'trap ""exit 3"" INT; sleep 20 & wait'");
   begin
      The_Command.Start;
      delay 0.3;
      The_Command.Interrupt;

      Check ("'Interrupt' returns when the program catches it", True);
   end;

   Ada.Text_IO.Put_Line ("End test.");
end Test_Unsafe_Pipeline_Failure;
