with
     Shell.Commands.Safe,
     Ada.Streams,
     Ada.Text_IO;


procedure Test_Safe_Close_Input
--
-- A command started with 'Accepts_Input' must see the end of its input when it is closed, and so finish.
-- Any failure raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;

   use type Ada.Streams.Stream_Element_Array;


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

begin
   -- A program which reads until the end of its input.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("sort");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Send  (+("b" & ASCII.LF));
      The_Command.Send  (+("a" & ASCII.LF));
      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("Command ends normally", The_Command.Normal_Exit);

      Check ("Output is complete",  +Output_Of (Results_Of (The_Command)) = "a" & ASCII.LF & "b" & ASCII.LF);
   end;


   -- All sent input must arrive before the end of the input.
   --
   declare
      The_Input   : constant Data  := [1 .. 30_000 => 65];
      The_Command : Safe.Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Send  (The_Input);
      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("All sent input arrives", Output_Of (Results_Of (The_Command)) = The_Input);
   end;


   -- Input given to 'Start'.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (+("Start." & ASCII.LF), Accepts_Input => True);
      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("Input given to 'Start' arrives", +Output_Of (Results_Of (The_Command)) = "Start." & ASCII.LF);
   end;


   -- Closing more than once, and sending afterwards.
   --
   declare
      The_Command : Safe.Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Close_Input;
      The_Command.Close_Input;
      The_Command.Send (+"Late.");     -- Ignored by the server, which logs it.
      The_Command.Wait_On;

      Check ("Sending to closed input is ignored", The_Command.Normal_Exit);
   end;


   -- Commands which have no input to close.
   --
   declare
      Unstarted : Safe.Command := Forge.To_Command ("true");
      The_Other : Safe.Command := Forge.To_Command ("sleep 1");
      Finished  : Safe.Command := Forge.To_Command ("true");
   begin
      Unstarted.Close_Input;

      The_Other.Start;
      The_Other.Close_Input;
      The_Other.Wait_On;

      Check ("Command without input is unaffected", The_Other.Normal_Exit);

      Finished.Run;
      Finished.Close_Input;

      Check ("Finished command is unaffected", Finished.Normal_Exit);
   end;
end Test_Safe_Close_Input;
