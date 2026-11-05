with
     Gnat.OS_Lib,
     Gnat.Strings,

     Ada.Unchecked_Deallocation,
     Ada.Containers.Hashed_Maps,
     Ada.Containers.Indefinite_Vectors,
     Ada.Real_Time,
     Ada.Strings.Fixed,
     Ada.Task_Identification,
     Ada.Text_IO,
     Ada.Exceptions;


package body Shell.Commands.Safe
is
   type Server_Loss_Kind is (Not_Lost, Stopped, Out_Of_Date, Silent);

   Server_Loss : Server_Loss_Kind := Not_Lost
     with Atomic;


   function Server_Lost_Message return String
   is (case Server_Loss is
          when Out_Of_Date => "The spawn server found on the PATH does not match this program. Reinstall it with 'install_spawn_server.sh'.",
          when Silent      => "The spawn server found on the PATH did not respond. It may be stuck, or older than this program. If older, reinstall it with 'install_spawn_server.sh'.",
          when others      => "The spawn server has stopped unexpectedly. See 'aShell_spawn_Server.log'.");


   function Client_Gone_Message return String
   is (if Server_Loss /= Not_Lost then Server_Lost_Message
                                  else "The Spawn client has shut down.");


   -----------------------
   --- Safe Client Outputs
   --

   protected
   body Client_Outputs
   is
      procedure Add_Outputs (Output : in Shell.Data;
                             Errors : in Shell.Data)
      is
      begin
         if Output'Length /= 0 then
            All_Output.Append (Output);
         end if;

         if Errors'Length /= 0 then
            All_Errors.Append (Errors);
         end if;
      end Add_Outputs;



      procedure Get_Outputs (Output      : out Data_Vector;
                             Errors      : out Data_Vector;
                             Normal_Exit : out Boolean)
      is
      begin
         Output      := All_Output;
         Errors      := All_Errors;
         Normal_Exit := Exit_Is_Normal;

         All_Output.Clear;
         All_Errors.Clear;
      end Get_Outputs;



      procedure Hold
      is
      begin
         Is_Held := True;
      end Hold;



      procedure Set_Done (Normal_Exit  : in     Boolean;
                          Not_At_Fault : in     Boolean;
                          Abandoned    :    out Boolean)
      is
      begin
         Exit_Is_Normal  := Normal_Exit;
         Is_Not_At_Fault := Not_At_Fault;
         Done            := True;
         Is_Held         := False;
         Abandoned       := Is_Abandoned;
      end Set_Done;



      procedure Abandon (Unused : out Boolean)
      is
      begin
         Is_Abandoned := True;
         Unused       := not Is_Held;
      end Abandon;



      procedure Set_Lost (Abandoned : out Boolean)
      is
      begin
         Is_Lost := True;
         Set_Done (Normal_Exit  => False,
                   Not_At_Fault => False,
                   Abandoned    => Abandoned);
      end Set_Lost;



      entry Wait_Til_Done
        when Done
      is
      begin
         if Is_Lost
         then
            raise Command_Error with Server_Lost_Message;
         end if;
      end Wait_Til_Done;



      function Is_Done return Boolean
      is
      begin
         return Done;
      end Is_Done;



      function Normal_Exit return Boolean
      is
      begin
         if Done
         then
            return Exit_Is_Normal;
         else
            raise Process_Error with "Process has not terminated.";
         end if;
      end Normal_Exit;



      function Not_At_Fault return Boolean
      is
      begin
         return Done and Is_Not_At_Fault;
      end Not_At_Fault;

   end Client_Outputs;


   procedure Deallocate is new Ada.Unchecked_Deallocation (Client_Outputs,
                                                           Client_Outputs_Access);


   ----------------
   --- Spawn_Client
   --

   task Spawn_Client
   is
      entry Add  (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Accepts_Input : in     Boolean := False;
                  Outputs       : in     Client_Outputs_Access);

      entry Add  (The_Pipeline : in out Command_Array;
                  Input        : in     Data := No_Data);

      entry Send (The_Command : in      Command;
                  Input       : in      Data);

      entry Close_Input (The_Command : in Command);

      entry Kill      (The_Command : in Command);
      entry Interrupt (The_Command : in Command);
      entry Pause     (The_Command : in Command);
      entry Resume    (The_Command : in Command);
      entry Stop      (The_Command : in Command);

      entry Shutdown;

      entry Deliver (Action : in Client_Action);     -- From the server.
      entry Server_Output_Failed;
      entry Server_Matches;
   end Spawn_Client;


   task body Spawn_Client
   is
      use Ada.Strings.Unbounded;

      package Id_Maps_of_Command_Outputs is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                            Element_Type    => Client_Outputs_Access,
                                                                            Hash            => Hash,
                                                                            Equivalent_Keys => "=");
      Command_Outputs_Map : Id_Maps_of_Command_Outputs.Map;

      Server_In_Pipe  : constant Shell.Pipe := To_Pipe;
      Server_Out_Pipe : constant Shell.Pipe := To_Pipe;
      Server_Err_Pipe : constant Shell.Pipe := To_Pipe;

      -- Actions accepted from callers, in order, waiting to be sent to the server. Holding them
      -- here means that a caller is never held up by the server, and that no action can be lost.
      --
      package Server_Action_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Server_Action);
      Pending_Actions : Server_Action_Vectors.Vector;

      Server_Input_Stream  : aliased Pipe_Stream := Stream (Server_In_Pipe);
      Next_Id        : Command_Id := 1;
      Shutting_Down  : Boolean    := False;     -- A shutdown is due to be sent to the server.
      Shutdown_Sent  : Boolean    := False;
      Server_Is_Done : Boolean    := False;

      Spawn_Server : Shell.Process;

      Lost_The_Server : exception;

      -- An older server says nothing and waits for actions it might misread, so hold them until the server has said which
      -- protocol it uses. The limit is a long one, as a server slow to start on a busy machine mustn't be taken for old.
      --
      Is_Matched         : Boolean  := False;
      Had_Grace          : Boolean  := False;
      Most_Time_To_Match : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Seconds (10);
      Match_Deadline     : Ada.Real_Time.Time;


      -- Blocks on the server's output, so this task can sleep until either a caller or the server needs it.
      --
      task Server_Output_Reader
      is
         entry Start;
      end Server_Output_Reader;


      task body Server_Output_Reader
      is
         Server_Output_Stream : aliased Pipe_Stream := Stream (Server_Out_Pipe);
         Is_Done              : Boolean             := False;
      begin
         select
            accept Start;
         or
            terminate;     -- The server failed to start.
         end select;

         declare
            Version : Integer;
         begin
            Integer'Read (Server_Output_Stream'Access, Version);

            if Version /= Protocol_Version
            then
               Server_Loss := Out_Of_Date;
               raise Program_Error with "The server has protocol version" & Version'Image & ".";
            end if;

            Spawn_Client.Server_Matches;
         end;

         while not Is_Done
         loop
            declare
               Action : constant Client_Action := Client_Action'Input (Server_Output_Stream'Access);
            begin
               Spawn_Client.Deliver (Action);
               Is_Done := Action.Kind = Server_Done;
            end;
         end loop;

      exception
         when E : others =>
            Log ("Server output reader stopped: " & Ada.Exceptions.Exception_Information (E));

            begin
               Spawn_Client.Server_Output_Failed;
            exception
               when Tasking_Error =>
                  null;     -- The spawn client has already ended.
            end;
      end Server_Output_Reader;


   begin
      Spawn_Server := Start (Program   => "ashell_spawn_server",
                             Arguments => Nil_Strings,
                             Input     => Server_In_Pipe,
                             Output    => Server_Out_Pipe,
                             Errors    => Server_Err_Pipe);

      Close (Server_In_Pipe,  Only_Read_End  => True);
      Close (Server_Out_Pipe, Only_Write_End => True);
      Close (Server_Err_Pipe);

      log ("Starting Spawn_Client");

      declare
         use type Ada.Real_Time.Time;
      begin
         Match_Deadline := Ada.Real_Time.Clock + Most_Time_To_Match;
      end;

      Server_Output_Reader.Start;

      loop
         select
            when not (Shutting_Down or Shutdown_Sent) =>
            accept Add (The_Command   : in out Command;
                        Input         : in     Data    := No_Data;
                        Accepts_Input : in     Boolean := False;
                        Outputs       : in     Client_Outputs_Access)
            do
               Log ("");
               Log ("Client: Accepting new command.");

               The_Command.Id := Next_Id;
               Next_Id        := Next_Id + 1;

               Outputs.Hold;
               Command_Outputs_Map.Insert (The_Command.Id,
                                           Outputs);

               Pending_Actions.Append (Server_Action' (New_Command,
                                                       The_Command.Id,
                                                       To_Command_Line (The_Command),
                                                       Data_Holders.To_Holder (Input),
                                                       Accepts_Input));
            end Add;
         or
            when not (Shutting_Down or Shutdown_Sent) =>
            accept Add (The_Pipeline : in out Command_Array;
                        Input        : in     Data := No_Data)
            do
               Log ("");
               Log ("Client: Accepting new pipeline.");

               declare
                  First_Id : constant Command_Id := Next_Id;
                  Lines    : Command_Lines;
               begin
                  for The_Command of The_Pipeline
                  loop
                     The_Command.Id := Next_Id;
                     Next_Id        := Next_Id + 1;

                     Lines.Append (To_Command_Line (The_Command));

                     The_Command.Safe_Outputs.Hold;
                     Command_Outputs_Map.Insert (The_Command.Id,
                                                 The_Command.Safe_Outputs);
                  end loop;

                  Pending_Actions.Append (Server_Action' (New_Pipeline,
                                                          First_Id,
                                                          Lines,
                                                          Data_Holders.To_Holder (Input)));
               end;
            end Add;
         or
            when not (Shutting_Down or Shutdown_Sent) =>     -- The server has closed all input.
            accept Send (The_Command : in Command;
                         Input       : in Data)
            do
               Log ("");
               Log ("Client: Sending data to command.");

               Pending_Actions.Append (Server_Action' (New_Input,
                                                       The_Command.Id,
                                                       Data_Holders.To_Holder (Input)));
            end Send;
         or
            accept Close_Input (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Closing input of command.");

               if not (Shutting_Down or Shutdown_Sent)     -- Otherwise, the server has already closed it.
               then
                  Pending_Actions.Append (Server_Action' (Close_Input, The_Command.Id));
               end if;
            end Close_Input;
         or
            accept Kill (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Killing command.");

               Pending_Actions.Append (Server_Action' (Kill, The_Command.Id));
            end Kill;
         or
            accept Interrupt (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Interrupt command.");

               Pending_Actions.Append (Server_Action' (Interrupt, The_Command.Id));
            end Interrupt;
         or
            accept Pause (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Pause command.");

               Pending_Actions.Append (Server_Action' (Pause, The_Command.Id));
            end Pause;
         or
            accept Resume (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Resume command.");

               Pending_Actions.Append (Server_Action' (Resume, The_Command.Id));
            end Resume;
         or
            accept Stop (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Stop command.");

               Pending_Actions.Append (Server_Action' (Stop, The_Command.Id));
            end Stop;
         or
            accept Shutdown
            do
               Log ("Client: Shutting down.");
               Shutting_Down := not Shutdown_Sent;
            end Shutdown;
         or
            accept Deliver (Action : in Client_Action)
            do
               declare
                  Command_Outputs : Client_Outputs_Access;
               begin
                  case Action.Kind
                  is
                     when New_Outputs =>
                        Log ("New Outputs for Command:" & Action.Id'Image);

                        Command_Outputs := Command_Outputs_Map.Element (Action.Id);
                        Command_Outputs.Add_Outputs (Action.Output.Element,
                                                     Action.Errors.Element);
                     when Command_Done =>
                        Log ("Command Done:" & Action.Id'Image);

                        Command_Outputs := Command_Outputs_Map.Element (Action.Id);

                        declare
                           Abandoned : Boolean;
                        begin
                           Command_Outputs.Set_Done   (Normal_Exit  => Action.Normal_Exit,
                                                       Not_At_Fault => Action.Not_At_Fault,
                                                       Abandoned    => Abandoned);
                           Command_Outputs_Map.Delete (Action.Id);

                           if Abandoned
                           then
                              Deallocate (Command_Outputs);
                           end if;
                        end;

                     when Server_Done =>
                        Log ("Server is done.");

                        Server_Is_Done := True;
                  end case;
               end;
            end Deliver;
         or
            accept Server_Output_Failed;
            raise Lost_The_Server;
         or
            accept Server_Matches;
            Is_Matched := True;
         or
            delay 0.05;     -- To notice the main program completing.
         end select;


         declare
            use type Ada.Real_Time.Time;
         begin
            if     not Is_Matched
              and then Ada.Real_Time.Clock > Match_Deadline
            then
               -- This program may have been stopped, with the version waiting for the reader to have its turn.
               -- The server may have been stopped along with it, before writing that, so give it a chance to.
               --
               if        Is_Empty (Server_Out_Pipe)
                 and then Had_Grace
               then
                  Log ("The server has not given its protocol version.");
                  Server_Loss := Silent;
                  raise Lost_The_Server;
               end if;

               Had_Grace      := True;
               Match_Deadline := Ada.Real_Time.Clock + Ada.Real_Time.Seconds (1);
            end if;
         end;


         if Is_Matched
         then
            for Each of Pending_Actions
            loop
               Log ("Sending " & Each.Kind'Image & " action for command" & Each.Id'Image & " to server.");

               begin
                  Server_Action'Output (Server_Input_Stream'Access, Each);
               exception
                  when others =>
                     -- The server may have finished, and exited, with its last results still on their way.
                     --
                     if not Shutdown_Sent
                     then
                        raise;
                     end if;

                     Log ("Dropped a late action, as the server has ended.");
               end;
            end loop;

            Pending_Actions.Clear;
         end if;


         if Shutting_Down
         then
            if Is_Matched
            then
               Log ("Client is shutting down.");
               Server_Action'Output (Server_Input_Stream'Access,
                                     Server_Action' (Shutdown,
                                                     Null_Id));
               Log ("Client asks server to stop.");
               Shutting_Down := False;
               Shutdown_Sent := True;

            -- Nothing needs the server, so don't wait to find out which it is. Any server ends once its input
            -- closes, without its version having to be read, and one which doesn't will reach the deadline.
            --
            elsif Command_Outputs_Map.Is_Empty
            then
               Log ("Client is shutting down, without having heard from the server.");
               Pending_Actions.Clear;
               Close (Server_In_Pipe, Only_Write_End => True);
               Shutting_Down := False;
               Shutdown_Sent := True;
            end if;

         -- The runtime makes the environment task uncallable when the main subprogram has completed (normally or
         -- due to an unhandled exception), and then waits for this task. So, shutdown if that has happened.
         --
         elsif     not Shutdown_Sent
           and then not Ada.Task_Identification.Is_Callable (Ada.Task_Identification.Environment_Task)
         then
            Log ("Client: The main program has completed.");
            Shutting_Down := True;
         end if;


         -- The server ended without finishing these commands, so no more results will come.
         --
         if     Server_Is_Done
            and not Command_Outputs_Map.Is_Empty
         then
            Log ("The server ended, with commands outstanding.");
            raise Lost_The_Server;
         end if;

         exit when Server_Is_Done
               and Command_Outputs_Map.Is_Empty;
      end loop;

      Close (Server_In_Pipe,  Only_Write_End => True);
      Close (Server_Out_Pipe, Only_Read_End  => True);

      Log ("Client is done.");

   exception
      when E : others =>
         Log ("Unhandled error in Spawn_Client.");
         Log (Ada.Exceptions.Exception_Information (E));

         if Server_Loss = Not_Lost
         then
            Server_Loss := Stopped;
         end if;

         -- The server has gone, or can't be understood. No more results will arrive, so release all waiting callers.
         --

         for Each of Command_Outputs_Map
         loop
            declare
               Command_Outputs : Client_Outputs_Access := Each;
               Abandoned       : Boolean;
            begin
               Command_Outputs.Set_Lost (Abandoned);

               if Abandoned
               then
                  Deallocate (Command_Outputs);
               end if;
            end;
         end loop;

         Command_Outputs_Map.Clear;

         -- A server which doesn't match has been sent nothing, so has nothing to finish. It may never close its output,
         -- and neither may a child of it, such as when it is a script. The reader of that would then never end, and
         -- neither would this program. So don't rely on the server's end to stop the reader.
         --
         if Server_Loss in Out_Of_Date | Silent
         then
            abort Server_Output_Reader;

            begin
               Shell.Kill (Spawn_Server);
            exception
               when Kill_Failure : others =>
                  Log (Ada.Exceptions.Exception_Information (Kill_Failure));
            end;
         end if;

         Close (Server_In_Pipe,  Only_Write_End => True);
         Close (Server_Out_Pipe, Only_Read_End  => True);
   end Spawn_Client;



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



   ---------
   --- Start
   --

   -- Marks the commands as started, ahead of handing them to the spawn client. If it has gone, they must return to not
   -- being started, as a later 'Wait_On' would otherwise wait for ever on commands which the client doesn't know of.
   --
   procedure Mark_As_Started (The_Pipeline : in out Command_Array)
   is
   begin
      for Each of The_Pipeline
      loop
         if Each.Status /= Not_Started
         then
            raise Command_Error with "Cannot start '" & (+Each.Name) & "' as it is already started.";
         end if;
      end loop;

      for Each of The_Pipeline
      loop
         Shell.Commands.Command (Each).Start;
      end loop;
   end Mark_As_Started;



   procedure Mark_As_Not_Started (The_Pipeline : in out Command_Array)
   is
   begin
      for Each of The_Pipeline
      loop
         Each.Status := Not_Started;
      end loop;
   end Mark_As_Not_Started;



   overriding
   procedure Start (The_Command   : in out Command;
                    Input         : in     Data    := No_Data;
                    Accepts_Input : in     Boolean := False;
                    Pipeline      : in     Boolean := False)
   is
   begin
      Commands.Command (The_Command).Start (Input, Accepts_Input, Pipeline);

      begin
         Spawn_Client.Add (The_Command,
                           Input,
                           Accepts_Input,
                           The_Command.Safe_Outputs);
      exception
         when Tasking_Error =>
            The_Command.Status := Not_Started;
            raise Command_Error with "Cannot run '" & (+The_Command.Name) & "'. " & Client_Gone_Message;
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

      Mark_As_Started (Commands);

      Spawn_Client.Add (Commands, Input);
   exception
      when Tasking_Error =>
         Mark_As_Not_Started (Commands);
         raise Command_Error with Client_Gone_Message;
   end Start;



   overriding
   procedure Send (To    : in Command;
                   Input : in Data)
   is
   begin
      Check_Is_Started (To, "send to");
      Spawn_Client.Send (To, Input);
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Send;



   overriding
   procedure Close_Input (The_Command : in Command)
   is
   begin
      Spawn_Client.Close_Input (The_Command);
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Close_Input;



   overriding
   procedure Gather_Results (The_Command : in out Command)
   is
      use type Ada.Containers.Count_Type;

      The_Output : Data_Vector;
      The_Errors : Data_Vector;
      Unused     : Boolean;

   begin
      The_Command.Safe_Outputs.Get_Outputs (The_Output, The_Errors, Unused);

      if The_Output.Length /= 0
      then
         The_Command.Output.Append (The_Output);
      end if;

      if The_Errors.Length /= 0
      then
         The_Command.Errors.Append (The_Errors);
      end if;
   end Gather_Results;



   --- Run
   --

   overriding
   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False)
   is
      Output      : Data_Vector;
      Errors      : Data_Vector;
      Normal_Exit : Boolean;
   begin
      Start (The_Command, Input);

      The_Command.Safe_Outputs.Wait_Til_Done;
      The_Command.Safe_Outputs.Get_Outputs (Output,
                                            Errors,
                                            Normal_Exit);
      The_Command.Output := Output;
      The_Command.Errors := Errors;

      if        Raise_Error
        and not Normal_Exit
      then
         raise Command_Error with Failure_Message (The_Command);
      end if;

   end Run;



   procedure Run_Commands (The_Pipeline : in out Command_Array;
                           Input        : in     Data;
                           Raise_Error  : in     Boolean)
   is
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
      i            : Positive := The_Pipeline'First;
   begin
      Mark_As_Started (The_Pipeline);

      begin
         Spawn_Client.Add (The_Pipeline, Input);
      exception
         when Tasking_Error =>
            Mark_As_Not_Started (The_Pipeline);
            raise Command_Error with Client_Gone_Message;
      end;

      loop
         The_Pipeline (i).Safe_Outputs.Wait_Til_Done;     -- Output is held for each command as it arrives, so just wait.

         if        Normal_Exit (The_Pipeline (i))
           or else The_Pipeline (i).Safe_Outputs.Not_At_Fault     -- Carry on, to the command which is.
         then
            i := i + 1;

            if i > The_Pipeline'Last
            then
               Gather_Results (Last_Command);
               exit;
            end if;

         else
            Gather_Results (The_Pipeline (i));

            declare
               Error : constant String := "Pipeline command" & Integer'Image (i) & ": " & Failure_Message (The_Pipeline (i));
            begin
               -- Stop the pipeline.
               --
               while i <= The_Pipeline'Last
               loop
                  if not Has_Terminated (The_Pipeline (i))
                  then
                     Spawn_Client.Stop (The_Pipeline (i));
                  end if;

                  i := i + 1;
               end loop;

               -- All that they have left to send is on its way. Without waiting, it would be missing from their results.
               --
               for Each of The_Pipeline
               loop
                  Each.Safe_Outputs.Wait_Til_Done;
               end loop;

               if Raise_Error
               then
                  raise Command_Error with Error;
               else
                  exit;
               end if;
            end;
         end if;
      end loop;
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Run_Commands;



   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False)
   is
   begin
      if The_Pipeline'Length = 0
      then
         raise Command_Error with "Cannot run a pipeline with no commands.";
      end if;

      Run_Commands (The_Pipeline, Input, Raise_Error);
   end Run;



   function Run (The_Pipeline : in out Command_Array;
                 Input        : in     Data    := No_Data;
                 Raise_Error  : in     Boolean := False) return Command_Results
   is
      Earlier : Data_Vector;
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

      return With_Earlier_Errors (Results_Of (The_Pipeline (The_Pipeline'Last)), Earlier);
   end Run;



   function Run (Command_Line : in String;
                 Input        : in Data  := No_Data) return Command_Results
   is
      use Ada.Strings.Fixed,
          Safe.Forge;

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
          Safe.Forge;
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



   function Failed (The_Pipeline : in Command_Array) return Boolean
   is
   begin
      return Which_Failed (The_Pipeline) /= 0;
   end Failed;



   function Which_Failed (The_Pipeline : in Command_Array) return Natural
   is
      Start_Failed : Boolean := False;     -- The server has said so of a command, and what it says of the rest is on its way.
      Stopped      : Natural := 0;         -- The first command stopped because of that, for when the one at fault isn't given.
   begin
      -- Only one pass, as what the server says may arrive at any time, and a later look may then see something different.
      --
      for i in The_Pipeline'Range
      loop
         if Start_Failed
         then
            The_Pipeline (i).Safe_Outputs.Wait_Til_Done;
         end if;

         if The_Pipeline (i).Safe_Outputs.Is_Done     -- First, as all that the server says of a command arrives together with this.
         then
            if The_Pipeline (i).Safe_Outputs.Not_At_Fault
            then
               Start_Failed := True;

               if     Stopped = 0
                 and then not The_Pipeline (i).Normal_Exit
               then
                  Stopped := i;
               end if;

            elsif not The_Pipeline (i).Normal_Exit
            then
               return i;
            end if;
         end if;
      end loop;

      return Stopped;
   end Which_Failed;



   -- The server tells of a command ending, and the status isn't changed by that until this is called.
   --
   procedure Note_Any_End (The_Command : in out Command)
   is
   begin
      if        The_Command.Status in Running | Paused     -- A killed command retains its status.
        and then The_Command.Safe_Outputs.Is_Done
      then
         The_Command.Status := (if The_Command.Safe_Outputs.Normal_Exit then Normal_Exit
                                                                        else Failed_Exit);
      end if;
   end Note_Any_End;



   overriding
   function Status (The_Command : in out Command) return State
   is
   begin
      Note_Any_End (The_Command);
      return The_Command.Status;
   end Status;



   overriding
   procedure Wait_On (The_Command : in out Command)
   is
   begin
      if The_Command.Status = Not_Started
      then
         raise Command_Error with "Cannot wait on '" & (+The_Command.Name) & "' as it has not been started.";
      end if;

      The_Command.Safe_Outputs.Wait_Til_Done;
   end Wait_On;



   overriding
   function Has_Terminated (The_Command : in out Command) return Boolean
   is
   begin
      return The_Command.Safe_Outputs.Is_Done;
   end Has_Terminated;



   overriding
   function Normal_Exit (The_Command : in Command) return Boolean
   is
   begin
      return The_Command.Safe_Outputs.Normal_Exit;
   end Normal_Exit;



   overriding
   procedure Kill (The_Command : in out Command)
   is
   begin
      Note_Any_End (The_Command);
      Commands.Command (The_Command).Kill;
      Spawn_Client.Kill (The_Command);
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Kill;



   overriding
   procedure Interrupt (The_Command : in out Command)
   is
   begin
      Check_Is_Started (The_Command, "interrupt");
      Spawn_Client.Interrupt (The_Command);
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Interrupt;



   overriding
   procedure Pause (The_Command : in out Command)
   is
   begin
      Note_Any_End (The_Command);
      Commands.Command   (The_Command).Pause;
      Spawn_Client.Pause (The_Command);
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Pause;



   overriding
   procedure Resume (The_Command : in out Command)
   is
   begin
      Note_Any_End (The_Command);

      -- A program may have been stopped from outside, such as by Ctrl-Z, which only
      -- the server knows of. So a command which appears to be running may be resumed.
      --
      if The_Command.Status /= Running
      then
         Commands.Command (The_Command).Resume;
      end if;

      Spawn_Client.Resume (The_Command);
   exception
      when Tasking_Error =>
         raise Command_Error with Client_Gone_Message;
   end Resume;



   overriding
   procedure Finalize (The_Command : in out Command)
   is
   begin
      if The_Command.Copy_Count.all = 1
      then
         declare
            Unused : Boolean;
         begin
            The_Command.Safe_Outputs.Abandon (Unused);

            if Unused
            then
               Deallocate (The_Command.Safe_Outputs);
            end if;
         end;
      end if;

      Finalize (Commands.Command (The_Command));
   end Finalize;



   procedure Stop_Spawn_Client
   is
   begin
      Spawn_Client.Shutdown;
   exception
      when Tasking_Error =>
         null;     -- The spawn client has already shut down.
   end Stop_Spawn_Client;



   use type Gnat.Strings.String_Access;

   Spawn_Server_Found : constant Boolean := Gnat.OS_Lib.Locate_Exec_On_Path ("ashell_spawn_server") /= null;

begin
   if not Spawn_Server_Found
   then
      Ada.Text_IO.Put_Line ("___________________________________________________________________");
      Ada.Text_IO.Put_Line ("Program 'ashell_spawn_server' not found on PATH. Please install it.");
      Ada.Text_IO.Put_Line ("___________________________________________________________________");

      raise Process_Error with "'ashell_spawn_server' not found on PATH.";
   end if;


   --- Ensure mutual exclusion of 'Safe' and 'Unsafe' commands.
   --
   if Unsafe_Commands_Are_Withed
   then
      Stop_Spawn_Client;
      raise Program_Error with "'Safe' and 'Unsafe' commands may not be used in the same program.";
   end if;

   Safe_Commands_Are_Withed := True;
   Halt_Spawn_Client        := Stop_Spawn_Client'Access;

end Shell.Commands.Safe;
