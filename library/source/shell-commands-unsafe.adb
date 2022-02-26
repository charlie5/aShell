with
     Ada.Strings.Fixed,
     Ada.Characters.Handling,
     Ada.Exceptions,
     Ada.Unchecked_Conversion;


package body Shell.Commands.Unsafe
is

   overriding
   function Image (The_Command : in Command) return String
   is
      use String_Vectors;
      Result : Unbounded_String;
   begin
      Append (Result, "(" & The_Command.Name);

      if not The_Command.Arguments.Is_Empty
        then
         Append (Result, ", (");
      end if;

      for Each of The_Command.Arguments
      loop
         Append (Result, Each);

         if Each = Last_Element (The_Command.Arguments)
         then
            Append (Result, ")");
         else
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ", Input_Pipe => "  & Image (The_Command.Input_Pipe));
      Append (Result, ", Output_Pipe => " & Image (The_Command.Output_Pipe));
      Append (Result, ", Error_Pipe => "  & Image (The_Command.Error_Pipe));

      Append (Result, ", Is_Within_A_Pipeline => "  & The_Command.Is_Within_A_Pipeline'Image);
      Append (Result, ")");

      return To_String (Result);
   end Image;


   ---------
   --- Forge
   --
   package body Forge
   is

      function To_Command (Command_Line : in String) return Command
      is
      begin
         return Result : Command
         do
            Define (Result, Command_Line);
         end return;
      end to_Command;



      function To_Commands (Pipeline : in String) return Command_Array
      is
         All_Commands : constant String_Array := To_Command_Lines (Pipeline);
      begin
         return Result : Command_Array (1 .. All_Commands'Length)
         do
            for i in 1 .. All_Commands'Length
            loop
               Define ( Result (i),
                       +All_Commands (i));
            end loop;
         end return;
      end To_Commands;


   end Forge;



   procedure Connect (From, To : in out Command)
   is
      Pipe : constant Shell.Pipe := To_Pipe;
   begin
      From.Output_Pipe := Pipe;
      To.   Input_Pipe := Pipe;

      From.Owns_Input_Pipe  := True;
      To.  Owns_Output_Pipe := True;
   end Connect;



   procedure Connect (Commands : in out Command_Array)
   is
   begin
      for i in Commands'First .. Commands'Last - 1
      loop
         Connect (From => Commands (i),
                  To   => Commands (i + 1));
      end loop;
   end Connect;



   procedure Close_Pipe_Write_Ends (The_Command : in out Command)
   is
   begin
      if The_Command.Output_Pipe /= Standard_Output
      then
         Close_Write_End (The_Command.Output_Pipe);
      end if;

      if The_Command.Error_Pipe /= Standard_Error
      then
         Close_Write_End (The_Command.Error_Pipe);
      end if;
   end Close_Pipe_Write_Ends;



   function Input_Pipe (The_Command : in Command) return Pipe
   is
   begin
      return The_Command.Input_Pipe;
   end Input_Pipe;



   function Output_Pipe (The_Command : in Command) return Pipe
   is
   begin
      return The_Command.Output_Pipe;
   end Output_Pipe;



   function Error_Pipe (The_Command : in Command) return Pipe
   is
   begin
      return The_Command.Error_Pipe;
   end Error_Pipe;



   function Process (The_Command : in out Command) return access Shell.Process
   is
   begin
      return The_Command.Process'Unchecked_Access;
   end Process;


   ---------
   --- Start
   --

   overriding
   procedure Start (The_Command   : in out Command;
                    Input         : in     Data    := No_Data;
                    Accepts_Input : in     Boolean := False;
                    Pipeline      : in     Boolean := False)
   is
   begin
      if   Input /= No_Data
        or Accepts_Input
      then
         The_Command.Input_Pipe := To_Pipe;
         Write_To (The_Command.Input_Pipe, Input);
      end if;

      if The_Command.Output_Pipe = Null_Pipe
      then
         The_Command.Owns_Output_Pipe := True;
         The_Command.Output_Pipe      := To_Pipe (Blocking => False);
      end if;

      if The_Command.Error_Pipe = Null_Pipe
      then
         The_Command.Error_Pipe := To_Pipe (Blocking => False);
      end if;

      The_Command.Process := Start (Program   => +The_Command.Name,
                                    Arguments =>  To_String_Array (The_Command.Arguments),
                                    Input     =>  The_Command.Input_Pipe,
                                    Output    =>  The_Command.Output_Pipe,
                                    Errors    =>  The_Command.Error_Pipe,
                                    Pipeline  =>  Pipeline);
   end Start;



   procedure Start (Commands : in out Command_Array;
                    Input    : in     Data    := No_Data;
                    Pipeline : in     Boolean := True)
   is
   begin
      if not Pipeline
      then
         for Each of Commands
         loop
            Start (Each, Input);
         end loop;

         return;
      end if;

      Connect (Commands);

      for i in Commands'Range
      loop
         if i = Commands'First
         then
            Start (Commands (i),
                   Input,
                   Pipeline => True);
         else
            Start (Commands (i),
                   Pipeline => True);
         end if;

         -- Since we are making a pipeline, we need to close the write ends of
         -- the Output & Errors pipes ourselves.
         --
         if i /= Commands'First
         then
            Close_Pipe_Write_Ends (Commands (i - 1));    -- Close ends for the prior command.
         end if;

      end loop;

      Close_Pipe_Write_Ends (Commands (Commands'Last));  -- Close ends for the final command.
   end Start;



   overriding
   procedure Send (To    : in Command;
                   Input : in Data)
   is
   begin
      Write_To (To.Input_Pipe, Input);
   end Send;


   -------
   --- Run
   --

   overriding
   procedure Gather_Results (The_Command : in out Command)
   is
   begin
      declare
         The_Output : constant Data := Output_Of (The_Command.Output_Pipe);
      begin
         if The_Output'Length /= 0
         then
            The_Command.Output.Append (The_Output);
         end if;
      end;

      declare
         The_Errors : constant Data := Output_Of (The_Command.Error_Pipe);
      begin
         if The_Errors'Length /= 0
         then
            The_Command.Errors.Append (The_Errors);
         end if;
      end;
   end Gather_Results;



   overriding
   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False)
   is
   begin
      Start (The_Command, Input);

      loop
         Gather_Results (The_Command);   -- Gather on-going results.
         exit when Has_Terminated (The_Command.Process);
      end loop;

      Gather_Results (The_Command);      -- Gather any final results.

      if not Normal_Exit (The_Command.Process)
         and Raise_Error
      then
         declare
            Error : constant String := +Output_Of (The_Command.Error_Pipe);
         begin
            raise Command_Error with Error;
         end;
      end if;
   end Run;



   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False)
   is
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
      i            : Positive     := 1;
   begin
      Last_Command.Output_Pipe := To_Pipe;

      Start (The_Pipeline, Input);

      loop
         Gather_Results (Last_Command);            -- Gather on-going results.

         if Has_Terminated (The_Pipeline (i).Process)
         then
            if Normal_Exit (The_Pipeline (i).Process)
            then
               i := i + 1;

               if i > The_Pipeline'Last
               then
                  Gather_Results (Last_Command);   -- Gather any final results.
                  exit;
               end if;

            else
               declare
                  Error : constant String :=   "Pipeline command" & Integer'Image (i)
                                             & " '" & (+The_Pipeline (i).Name) & "' failed.";
               begin
                  -- Stop the pipeline.
                  --
                  while i <= The_Pipeline'Last
                  loop
                     Stop (The_Pipeline (i));
                     i := i + 1;
                  end loop;

                  if Raise_Error
                  then
                     raise Command_Error with Error;
                  else
                     exit;
                  end if;
               end;
            end if;
         end if;
      end loop;
   end Run;



   function Run (The_Pipeline : in out Command_Array;
                 Input        : in     Data    := No_Data;
                 Raise_Error  : in     Boolean := False) return Command_Results
   is
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Run (The_Pipeline, Input, Raise_Error);

      return Results_Of (Last_Command);
   end Run;



   function Run (Command_Line : in String;
                 Input        : in Data  := No_Data) return Command_Results
   is
      use Ada.Strings.Fixed,
          Unsafe.Forge;

      The_Index   : constant Natural := Index (Command_Line, " | ");
      Is_Pipeline : constant Boolean := The_Index /= 0;
   begin
      if Is_Pipeline
      then
         declare
            The_Commands : Command_Array := To_Commands (Command_Line);
         begin
            return Run (The_Commands, Input);
         end;
      else
         declare
            The_Command : Command := To_Command (Command_Line);
         begin
            return Run (The_Command, Input);
         end;
      end if;
   end Run;



   procedure Run (Command_Line : in String;
                  Input        : in Data  := No_Data)
   is
      Results : Command_Results := Run (Command_Line, Input) with Unreferenced;
   begin
      null;
   end Run;



   procedure Stop (The_Command : in out Command)
   is
      use Ada.Characters.Handling,
          Ada.Exceptions;
   begin
      The_Command.Gather_Results;

      Close (The_Command. Input_Pipe);
      Close (The_Command.Output_Pipe);
      Close (The_Command. Error_Pipe);

      begin
         Kill (The_Command);
      exception
         when E : POSIX.POSIX_Error =>
            if To_Upper (Exception_Message (E)) /= "NO_SUCH_PROCESS"
            then
               Log ("Unable to kill process" & Image (The_Command.Process));
               raise;
            end if;
      end;

      begin
         Wait_On (The_Command.Process);   -- Reap zombies.
      exception
         when E : POSIX.POSIX_Error =>
            if To_Upper (Exception_Message (E)) /= "NO_CHILD_PROCESS"
            then
               Log ("Unable to wait on process" & Image (The_Command.Process));
               raise;
            end if;
      end;
   end Stop;



   function Failed (The_Pipeline : in Command_Array) return Boolean
   is
   begin
      for Each of The_Pipeline
      loop
         if Failed (Each)
         then
            return True;
         end if;
      end loop;

      return False;
   end Failed;



   function Which_Failed (The_Pipeline : in Command_Array) return Natural
   is
   begin
      for i in The_Pipeline'Range
      loop
         if not Normal_Exit (The_Pipeline (i).Process)
         then
            return i;
         end if;
      end loop;

      return 0;
   end Which_Failed;



   overriding
   procedure Wait_On (The_Command : in out Command)
   is
   begin
      Wait_On (The_Command.Process);
   end Wait_On;



   overriding
   function Has_Terminated (The_Command : in out Command) return Boolean
   is
   begin
      return Has_Terminated (The_Command.Process);
   end Has_Terminated;



   overriding
   function Normal_Exit (The_Command : in Command) return Boolean
   is
   begin
      return Normal_Exit (The_Command.Process);
   end Normal_Exit;



   overriding
   procedure Kill (The_Command : in Command)
   is
   begin
      Kill (The_Command.Process);
   end Kill;



   overriding
   procedure Interrupt (The_Command : in Command)
   is
   begin
      Interrupt (The_Command.Process);
   end Interrupt;



   overriding
   procedure Pause (The_Command : in out Command)
   is
   begin
      Pause (The_Command.Process);
      The_Command.Paused := True;
   end Pause;



   overriding
   procedure Resume (The_Command : in out Command)
   is
   begin
      Resume (The_Command.Process);
      The_Command.Paused := False;
   end Resume;



   overriding
   function Is_Paused (The_Command : in Command) return Boolean
   is
   begin
      return The_Command.Paused;
   end Is_Paused;



   --------------
   --- Controlled
   --

   overriding
   procedure Adjust (The_Command : in out Command)
   is
   begin
      The_Command.Copy_Count.all := The_Command.Copy_Count.all + 1;
   end Adjust;



   overriding
   procedure Finalize (The_Command : in out Command)
   is
   begin
      if The_Command.Copy_Count.all = 1
      then
         if The_Command.Owns_Input_Pipe
         then
            Close (The_Command.Input_Pipe);
         end if;

         if The_Command.Owns_Output_Pipe
         then
            Close (The_Command.Output_Pipe);
         end if;

         Close (The_Command.Error_Pipe);
      end if;

      Commands.Command (The_Command).Finalize;
   end Finalize;


end Shell.Commands.Unsafe;
