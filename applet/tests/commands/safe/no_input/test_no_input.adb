with
     Shell.Commands.Safe,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Test_No_Input
--
-- A program started without any input must see the end of its input, rather than read the client's actions from the
-- spawn server's standard input. Any failure of this test raises an exception, or hangs.
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


   function Trimmed (Item : in String) return String
   is (Ada.Strings.Fixed.Trim (Item (Item'First .. Item'Last - 1), Ada.Strings.Both));     -- Without the line feed.

begin
   -- A command, with others started while it runs.
   --
   declare
      Reader : Safe.Command := Forge.To_Command ("cat");
   begin
      Reader.Start;

      for Each in 1 .. 5
      loop
         Check ("Command started alongside a reader runs", +Output_Of (Run ("echo Hello")) = "Hello" & ASCII.LF);
      end loop;

      Reader.Wait_On;

      Check ("Program without input ends normally",   Reader.Normal_Exit);
      Check ("Program without input reads nothing", Output_Of (Results_Of (Reader))'Length = 0);
   end;


   -- The first command in a pipeline.
   --
   declare
      The_Pipeline : Command_Array := Forge.To_Commands ("cat | wc -c");
   begin
      Check ("Pipeline without input reads nothing", Trimmed (+Output_Of (Run (The_Pipeline))) = "0");
   end;


   -- Input given as usual must still arrive.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("cat");
   begin
      Check ("Program with input reads it", +Output_Of (Run (The_Command, Input => +"Given")) = "Given");
   end;


   -- After the server is asked to shut down, it has closed the input of all commands. It must still act on
   -- other actions, or a program which never ends could not be stopped.
   --
   declare
      use Ada.Real_Time;

      The_Command : Safe.Command := Forge.To_Command ("sleep 1");
      Long_Runner : Safe.Command := Forge.To_Command ("sleep 25");
      Late_Paused : Safe.Command := Forge.To_Command ("sleep 1");
      Raised      : Boolean      := False;
      Before      : constant Time := Clock;
   begin
      The_Command.Start (Accepts_Input => True);
      Long_Runner.Start;
      Late_Paused.Start;

      Stop_Spawn_Client;
      delay 0.1;

      The_Command.Close_Input;
      Ada.Text_IO.Put_Line ("Closing input after shutdown does nothing passed.");

      Long_Runner.Kill;
      Late_Paused.Pause;     -- Nothing would resume it, so the server must not leave it paused.

      begin
         The_Command.Send (+"Too late.");
      exception
         when Command_Error =>
            Raised := True;
      end;

      Check ("Sending after shutdown raises", Raised);
      Check ("Killing after shutdown stops the program", To_Duration (Clock - Before) < 15.0);

      -- The client has now ended. Failing to start must not leave a command to wait on for ever.
      --
      declare
         Too_Late : Safe.Command := Forge.To_Command ("true");
      begin
         begin
            Too_Late.Start;
            Check ("Starting after the client has ended raises", False);
         exception
            when Command_Error =>
               Check ("Starting after the client has ended leaves it not started", State'(Too_Late.Status) = Not_Started);
         end;

         begin
            Too_Late.Wait_On;
            Check ("Waiting on a command which failed to start raises", False);
         exception
            when Command_Error =>
               Check ("Waiting on a command which failed to start raises", True);
         end;
      end;
   end;
end Test_No_Input;
