with
     Ada.Unchecked_Deallocation,
     Ada.Strings.Fixed,
     Ada.Characters.Handling,
     Ada.Exceptions;


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
      From.Output_Pipe         := Pipe;
      From.Output_Is_Connected := True;
      To.   Input_Pipe         := Pipe;

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

   type Pipeline_Stage is
      record
         Output_Pipe : Pipe;
         Error_Pipe  : Pipe;

         Output      : Data_Vector;     -- Read while waiting, until the stage's command gathers it.
         Errors      : Data_Vector;
      end record;

   type Pipeline_Stages is array (Positive range <>) of Pipeline_Stage;

   type Shared_Pipeline (Count : Positive) is
      record
         Users  : Natural := Count;     -- The commands which refer to this.
         Stages : Pipeline_Stages (1 .. Count);
      end record;

   procedure Free is new Ada.Unchecked_Deallocation (Shared_Pipeline, Shared_Pipeline_Access);


   -- A program usually closes its pipes just before it can be seen to have ended, and nothing then wakes the wait.
   --
   Most_Wait_On_Pipes : constant Duration := 0.002;


   procedure Read_All_Stages (The_Pipeline : in out Shared_Pipeline)
   is
      The_Pipes : Pipe_Array (1 .. 2 * The_Pipeline.Count);
      Next      : Positive := 1;
   begin
      for Each of The_Pipeline.Stages
      loop
         The_Pipes (Next)     := Each.Output_Pipe;
         The_Pipes (Next + 1) := Each.Error_Pipe;
         Next                 := Next + 2;
      end loop;

      Wait_For_Output (The_Pipes, Timeout => Most_Wait_On_Pipes);

      for Each of The_Pipeline.Stages
      loop
         declare
            The_Output : constant Data := Available_Output_Of (Each.Output_Pipe);     -- Finds nothing for a pipe read by the next command.
            The_Errors : constant Data := Available_Output_Of (Each.Error_Pipe);
         begin
            if The_Output'Length /= 0
            then
               Each.Output.Append (The_Output);
            end if;

            if The_Errors'Length /= 0
            then
               Each.Errors.Append (The_Errors);
            end if;
         end;
      end loop;
   end Read_All_Stages;



   -- A pipeline's pipes are replaced before its commands are started, which would disturb any which are running.
   --
   procedure Check_None_Are_Started (The_Pipeline : in Command_Array)
   is
   begin
      for Each of The_Pipeline
      loop
         if Each.Status /= Not_Started
         then
            raise Command_Error with "Cannot start '" & (+Each.Name) & "' as it is already started.";
         end if;
      end loop;
   end Check_None_Are_Started;



   overriding
   procedure Start (The_Command   : in out Command;
                    Input         : in     Data    := No_Data;
                    Accepts_Input : in     Boolean := False;
                    Pipeline      : in     Boolean := False)
   is
      Least_Pipe_Capacity : constant := 4096;     -- More than this might block, until the program is started and reads it.

      Input_Fits : constant Boolean := Input'Length <= Least_Pipe_Capacity;

      Makes_Input  : constant Boolean :=    Input /= No_Data
                                         or Accepts_Input;
      Makes_Output : constant Boolean := The_Command.Output_Pipe = Null_Pipe;
      Makes_Errors : constant Boolean := The_Command.Error_Pipe  = Null_Pipe;
   begin
      Commands.Command (The_Command).Start (Input, Accepts_Input, Pipeline);

      -- Starting a command twice raises above, which must not disturb the command which is running.
      --
      begin
         if Makes_Input
         then
            The_Command.Input_Pipe := To_Pipe;

            if Input_Fits
            then
               Write_To (The_Command.Input_Pipe, Input);
            end if;
         end if;

         if Makes_Output
         then
            The_Command.Owns_Output_Pipe := True;
            The_Command.Output_Pipe      := To_Pipe (Blocking => False);
         end if;

         if Makes_Errors
         then
            The_Command.Error_Pipe := To_Pipe (Blocking => False);
         end if;

         The_Command.Process := Start (Program   => +The_Command.Name,
                                       Arguments =>  To_String_Array (The_Command.Arguments),
                                       Input     =>  The_Command.Input_Pipe,
                                       Output    =>  The_Command.Output_Pipe,
                                       Errors    =>  The_Command.Error_Pipe,
                                       Pipeline  =>  Pipeline);
         if not Input_Fits
         then
            Write_In_Background (The_Command.Input_Pipe, Input);
         end if;

         -- Let the program see the end of its input, once it has all been written.
         --
         if     Input /= No_Data
           and not Accepts_Input
         then
            Close_Write_End (The_Command.Input_Pipe);
         end if;

      exception
         when others =>     -- Stop any program, rid the pipes this made, and go back to not being started.
            begin
               if Shell.Status (The_Command.Process) /= Shell.Not_Started
               then
                  Kill (The_Command.Process);
               end if;

               if Makes_Input
               then
                  Close (The_Command.Input_Pipe);     -- Still the standard input, if making the pipe failed. This then does nothing.
               end if;

               if         Makes_Output
                 and then The_Command.Output_Pipe /= Null_Pipe
               then
                  Close (The_Command.Output_Pipe);
               end if;

               if         Makes_Errors
                 and then The_Command.Error_Pipe /= Null_Pipe
               then
                  Close (The_Command.Error_Pipe);
               end if;

            exception
               when E : others =>
                  Log (Ada.Exceptions.Exception_Information (E));     -- The original failure is the one to report.
            end;

            declare
               Unstarted : Shell.Process;
            begin
               The_Command.Process := Unstarted;
            end;

            if Makes_Input
            then
               The_Command.Input_Pipe := Standard_Input;
            end if;

            if Makes_Output
            then
               The_Command.Output_Pipe      := Null_Pipe;
               The_Command.Owns_Output_Pipe := False;
            end if;

            if Makes_Errors
            then
               The_Command.Error_Pipe := Null_Pipe;
            end if;

            The_Command.Status := Not_Started;
            raise;
      end;
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

      if Commands'Length = 0
      then
         raise Command_Error with "Cannot start a pipeline with no commands.";
      end if;

      Check_None_Are_Started (Commands);
      Connect (Commands);

      begin
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

      exception
         when others =>     -- Stop those which started, and rid the pipes of those which didn't.
            for Each of Commands
            loop
               begin
                  if Shell.Status (Each.Process) /= Shell.Not_Started
                  then
                     Stop (Each);
                  else
                     Close (Each.Input_Pipe);

                     if Each.Output_Pipe /= Null_Pipe
                     then
                        Close (Each.Output_Pipe);
                     end if;
                  end if;
               exception
                  when E : others =>
                     Log (Ada.Exceptions.Exception_Information (E));     -- The original failure is the one to report.
               end;
            end loop;

            raise;
      end;

      declare
         The_Pipeline : constant Shared_Pipeline_Access := new Shared_Pipeline (Commands'Length);
         Stage        : Positive := 1;
      begin
         for Each of Commands
         loop
            The_Pipeline.Stages (Stage).Output_Pipe := Each.Output_Pipe;
            The_Pipeline.Stages (Stage).Error_Pipe  := Each.Error_Pipe;

            Each.Pipeline := The_Pipeline;
            Each.Stage    := Stage;

            Stage := Stage + 1;
         end loop;
      end;
   end Start;



   overriding
   procedure Send (To    : in Command;
                   Input : in Data)
   is
   begin
      Write_In_Background (To.Input_Pipe, Input);
   end Send;



   overriding
   procedure Close_Input (The_Command : in Command)
   is
   begin
      Close_Write_End (The_Command.Input_Pipe);
   end Close_Input;


   -------
   --- Run
   --

   overriding
   procedure Gather_Results (The_Command : in out Command)
   is
   begin
      if The_Command.Pipeline /= null
      then
         declare
            The_Stage : Pipeline_Stage renames The_Command.Pipeline.Stages (The_Command.Stage);
         begin
            The_Command.Output.Append (The_Stage.Output);
            The_Command.Errors.Append (The_Stage.Errors);

            The_Stage.Output.Clear;
            The_Stage.Errors.Clear;
         end;
      end if;

      for Each in 1 .. Most_Reads_Per_Gather
      loop
         declare
            function Read (The_Pipe : in Pipe) return Data     -- A command which failed to start has no pipes.
            is (if The_Pipe = Null_Pipe then No_Data
                                        else Output_Of (The_Pipe));

            The_Output : constant Data := (if The_Command.Output_Is_Connected then No_Data     -- It is for the command it is connected to.
                                                                              else Read (The_Command.Output_Pipe));
            The_Errors : constant Data := Read (The_Command.Error_Pipe);
         begin
            exit when The_Output'Length = 0
                  and The_Errors'Length = 0;

            if The_Output'Length /= 0
            then
               The_Command.Output.Append (The_Output);
            end if;

            if The_Errors'Length /= 0
            then
               The_Command.Errors.Append (The_Errors);
            end if;
         end;
      end loop;
   end Gather_Results;



   -- For 'Run', a program which can't start simply fails, as it does with safe commands.
   --
   procedure Note_Failure_To_Start (The_Command : in out Command;
                                    Message     : in     String)
   is
   begin
      The_Command.Errors.Append (To_Data (Message & ASCII.LF));
      The_Command.Failed_To_Start := True;
      The_Command.Status          := Failed_Exit;
   end Note_Failure_To_Start;



   overriding
   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False)
   is
   begin
      begin
         Start (The_Command, Input);
      exception
         when E : Process_Error =>
            Note_Failure_To_Start (The_Command, Ada.Exceptions.Exception_Message (E));

            if Raise_Error
            then
               raise Command_Error with Failure_Message (The_Command);
            end if;

            return;
      end;

      Wait_On (The_Command);

      if not The_Command.Normal_Exit
         and Raise_Error
      then
         raise Command_Error with Failure_Message (The_Command);
      end if;
   end Run;



   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False)
   is
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
      i            : Positive     := The_Pipeline'First;
   begin
      Check_None_Are_Started (The_Pipeline);

      Last_Command.Output_Pipe      := To_Pipe;
      Last_Command.Owns_Output_Pipe := True;     -- 'Connect' only does this when there is a command before it.

      begin
         Start (The_Pipeline, Input);
      exception
         when E : Process_Error =>
            declare
               At_Fault : Natural := 0;
            begin
               for Each in The_Pipeline'Range     -- Those which did start have been stopped.
               loop
                  if Shell.Status (The_Pipeline (Each).Process) = Shell.Not_Started
                  then
                     if At_Fault = 0
                     then
                        At_Fault := Each;
                        Note_Failure_To_Start (The_Pipeline (Each), Ada.Exceptions.Exception_Message (E));
                     else
                        The_Pipeline (Each).Status := Failed_Exit;
                     end if;
                  end if;
               end loop;

               if Raise_Error
               then
                  raise Command_Error with "Pipeline command" & Integer'Image (At_Fault) & ": "
                                         & Failure_Message (The_Pipeline (At_Fault));
               end if;

               return;
            end;
      end;

      loop
         Read_All_Stages (Last_Command.Pipeline.all);     -- An earlier command may be blocked by a full pipe.

         if The_Pipeline (i).Has_Terminated     -- Of the command, rather than its process, so as to update its status.
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
               Gather_Results (The_Pipeline (i));

               declare
                  Error : constant String := "Pipeline command" & Integer'Image (i) & ": " & Failure_Message (The_Pipeline (i));
               begin
                  -- Stop the rest of the pipeline.
                  --
                  for Each in i + 1 .. The_Pipeline'Last
                  loop
                     Stop (The_Pipeline (Each));
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
      Earlier      : Data_Vector;
   begin
      Run (The_Pipeline, Input, Raise_Error);

      -- Otherwise, the failure of an earlier command would give results with no sign of it.
      -- These errors remain available from each command's own 'Results_Of'.
      --
      for i in The_Pipeline'First .. The_Pipeline'Last - 1
      loop
         Gather_Results (The_Pipeline (i));
         Earlier.Append (The_Pipeline (i).Errors);
      end loop;

      return With_Earlier_Errors (Results_Of (Last_Command), Earlier);
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
      use Ada.Strings.Fixed,
          Unsafe.Forge;
   begin
      if Index (Command_Line, " | ") /= 0
      then
         declare
            The_Commands : Command_Array := To_Commands (Command_Line);
         begin
            Run (The_Commands, Input, Raise_Error => True);
         end;
      else
         declare
            The_Command : Command := To_Command (Command_Line);
         begin
            Run (The_Command, Input, Raise_Error => True);
         end;
      end if;
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
         if not The_Command.Has_Terminated     -- It may have just seen the end of its input.
         then
            Kill (The_Command);
         end if;
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
      for i in The_Pipeline'Range     -- Commands before this were only stopped because of it, so aren't at fault.
      loop
         if The_Pipeline (i).Failed_To_Start
         then
            return i;
         end if;
      end loop;

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
      if The_Command.Status = Not_Started
      then
         raise Command_Error with "Cannot wait on '" & (+The_Command.Name) & "' as it has not been started.";
      end if;

      -- Nothing else reads the output, and a program can't end while it is blocked by a full pipe. In
      -- a pipeline, that may be the pipe of a later command, which then holds up the earlier commands.
      --
      loop
         if The_Command.Pipeline /= null
         then
            Read_All_Stages (The_Command.Pipeline.all);
         else
            declare
               Output_Pipe : constant Pipe := (if The_Command.Output_Is_Connected then Null_Pipe
                                                                                  else The_Command.Output_Pipe);
            begin
               Wait_For_Output ([Output_Pipe, The_Command.Error_Pipe], Timeout => Most_Wait_On_Pipes);

               declare
                  The_Output : constant Data := Available_Output_Of (Output_Pipe);
                  The_Errors : constant Data := Available_Output_Of (The_Command.Error_Pipe);
               begin
                  if The_Output'Length /= 0
                  then
                     The_Command.Output.Append (The_Output);
                  end if;

                  if The_Errors'Length /= 0
                  then
                     The_Command.Errors.Append (The_Errors);
                  end if;
               end;
            end;
         end if;

         exit when The_Command.Has_Terminated;
      end loop;

      The_Command.Gather_Results;     -- Gather any final results.
   end Wait_On;



   overriding
   function Has_Terminated (The_Command : in out Command) return Boolean
   is
   begin
      case The_Command.Status
      is
         when Not_Started =>
            return False;

         when Running
            | Paused =>
            declare
               Is_Terminated : constant Boolean := Has_Terminated (The_Command.Process);
            begin
               if The_Command.Process.State = Shell.Paused     -- It may have been stopped from outside, such as by Ctrl-Z.
               then
                  The_Command.Status := Paused;
               end if;

               if Is_Terminated
               then
                  if The_Command.Normal_Exit
                  then
                     The_Command.Status := Normal_Exit;
                  else
                     The_Command.Status := Failed_Exit;
                  end if;
               end if;

               return Is_Terminated;
            end;

         when Normal_Exit
            | Failed_Exit
            | Killed =>
            return True;
      end case;
   end Has_Terminated;



   overriding
   function Normal_Exit (The_Command : in Command) return Boolean
   is
   begin
      return Normal_Exit (The_Command.Process);
   end Normal_Exit;



   overriding
   procedure Kill (The_Command : in out Command)
   is
   begin
      Commands.Command (The_Command).Kill;
      Kill (The_Command.Process);
   end Kill;



   overriding
   procedure Interrupt (The_Command : in out Command)
   is
   begin
      Interrupt (The_Command.Process);
   end Interrupt;



   -- A program may have been stopped from outside, such as by Ctrl-Z.
   --
   procedure Note_Any_Outside_Stop (The_Command : in out Command)
   is
   begin
      if         The_Command.Status = Running
        and then Shell.Status (The_Command.Process) = Shell.Paused
      then
         The_Command.Status := Paused;
      end if;
   end Note_Any_Outside_Stop;



   overriding
   procedure Pause (The_Command : in out Command)
   is
   begin
      Note_Any_Outside_Stop (The_Command);
      Commands.Command (The_Command).Pause;
      Pause            (The_Command.Process);
   end Pause;



   overriding
   procedure Resume (The_Command : in out Command)
   is
   begin
      Note_Any_Outside_Stop (The_Command);
      Commands.Command (The_Command).Resume;
      Resume           (The_Command.Process);
   end Resume;



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
         if The_Command.Pipeline /= null
         then
            The_Command.Pipeline.Users := The_Command.Pipeline.Users - 1;

            if The_Command.Pipeline.Users = 0
            then
               Free (The_Command.Pipeline);
            end if;
         end if;

         Close_Write_End (The_Command.Input_Pipe);     -- Nothing can send any more, and it would stop any input writer from finishing.

         if The_Command.Owns_Input_Pipe
         then
            Close (The_Command.Input_Pipe);
         end if;

         if         The_Command.Owns_Output_Pipe
           and then The_Command.Output_Pipe /= Null_Pipe     -- It may have failed to start.
         then
            Close (The_Command.Output_Pipe);
         end if;

         if The_Command.Error_Pipe /= Null_Pipe
         then
            Close (The_Command.Error_Pipe);
         end if;
      end if;

      Commands.Command (The_Command).Finalize;
   end Finalize;


begin
   --- Ensure mutual exclusion of 'Safe' and 'Unsafe' commands.
   --
   if Safe_Commands_Are_Withed
   then
      Halt_Spawn_Client.all;
      raise Program_Error with "'Safe' and 'Unsafe' commands may not be used in the same program.";
   end if;

   Unsafe_Commands_Are_Withed := True;

end Shell.Commands.Unsafe;
