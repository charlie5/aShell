with
     Shell.Commands.Unsafe,
     Ada.Calendar,
     Ada.Strings.Unbounded,
     Ada.Streams,
     Ada.Text_IO;


procedure Test_Unsafe_Close_Input
--
-- A command started with 'Accepts_Input' must see the end of its input when it is closed, and so finish.
-- Any failure raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Unsafe;

   use type Ada.Streams.Stream_Element_Array;

   type Data_Access is access Data;


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
      The_Command : Unsafe.Command := Forge.To_Command ("sort");
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
      The_Command : Unsafe.Command := Forge.To_Command ("cat");
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
      The_Command : Unsafe.Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (+("Start." & ASCII.LF), Accepts_Input => True);
      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("Input given to 'Start' arrives", +Output_Of (Results_Of (The_Command)) = "Start." & ASCII.LF);
   end;


   -- Closing more than once, and sending afterwards.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("cat");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Close_Input;
      The_Command.Close_Input;
      The_Command.Wait_On;

      begin
         The_Command.Send (+"Late.");
         Check ("Sending to closed input raises", False);
      exception
         when Pipe_Error =>
            Check ("Sending to closed input raises", True);
      end;
   end;


   -- Commands which have no input to close.
   --
   declare
      Unstarted : constant Unsafe.Command := Forge.To_Command ("true");
      The_Other : Unsafe.Command := Forge.To_Command ("sleep 1");
      Finished  : Unsafe.Command := Forge.To_Command ("true");
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


   -- Large sends must keep their order, while the program's output fills up unread.
   --
   declare
      The_First   : constant Data_Access := new Data'[1 .. 1_000_000 => 65];
      The_Second  : constant Data_Access := new Data'[1 .. 1_000_000 => 66];
      The_Command : Unsafe.Command := Forge.To_Command ("cat");
      The_Output  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Send  (The_First.all);
      The_Command.Send  (The_Second.all);
      The_Command.Close_Input;

      -- This task is the only one reading the output, so it must keep doing so.
      --
      loop
         declare
            Finished : constant Boolean := The_Command.Has_Terminated;
         begin
            Ada.Strings.Unbounded.Append (The_Output, +Output_Of (Results_Of (The_Command)));
            exit when Finished;
         end;
      end loop;

      Check ("Large sends arrive in order",     Ada.Strings.Unbounded.Length  (The_Output)            = 2_000_000
                                            and Ada.Strings.Unbounded.Element (The_Output, 1)         = 'A'
                                            and Ada.Strings.Unbounded.Element (The_Output, 1_000_000) = 'A'
                                            and Ada.Strings.Unbounded.Element (The_Output, 1_000_001) = 'B'
                                            and Ada.Strings.Unbounded.Element (The_Output, 2_000_000) = 'B');
   end;


   -- A command started later must not hold the input open.
   --
   declare
      use Ada.Calendar;

      The_Start   : constant Time := Clock;
      The_Command : Unsafe.Command := Forge.To_Command ("wc -c");
      The_Other   : Unsafe.Command := Forge.To_Command ("sleep 6");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Other.Start;
      The_Command.Send (+"Hello");
      The_Command.Close_Input;
      The_Command.Wait_On;

      Check ("Later command doesn't hold the input open", Clock - The_Start < 3.0);

      The_Other.Kill;
   end;


   -- Sending to a program which has gone.
   --
   declare
      The_Command : Unsafe.Command := Forge.To_Command ("true");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Wait_On;

      for Each in 1 .. 50
      loop
         The_Command.Send (+"Text");
         delay 0.1;
      end loop;

      Check ("Sending to a program which has gone raises", False);
   exception
      when Pipe_Error =>
         Check ("Sending to a program which has gone raises", True);
   end;
end Test_Unsafe_Close_Input;
