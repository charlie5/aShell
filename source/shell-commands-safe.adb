with
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation,
     Ada.Containers.Hashed_Maps,
     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Text_IO,
     Ada.Exceptions;

package body Shell.Commands.Safe
is

   -----------------------
   --- Safe_Client_Outputs
   --

   protected
   body Safe_Client_Outputs
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


      entry Get_Outputs (Output      : out Data_Vector;
                         Errors      : out Data_Vector;
                         Normal_Exit : out Boolean)
        when Done
      is
      begin
         Output      := All_Output;
         Errors      := All_Errors;
         Normal_Exit := Exit_Is_Normal;
      end Get_Outputs;


      procedure Set_Done (Normal_Exit : in Boolean)
      is
      begin
         Exit_Is_Normal := Normal_Exit;
         Done           := True;
      end Set_Done;


      entry Wait_Til_Done
        when Done
      is
      begin
         null;
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

   end Safe_Client_Outputs;



   ----------------
   --- Spawn_Client
   --

   task Spawn_Client
   is
      entry Add  (The_Command : in Command;
                  Input       : in Data   := No_Data;
                  Outputs     : in Safe_Client_Outputs_Access);

      entry Kill      (The_Command : in Command);
      entry Interrupt (The_Command : in Command);
      entry Pause     (The_Command : in Command);
      entry Resume    (The_Command : in Command);

      entry Stop;
   end Spawn_Client;


   task body Spawn_Client
   is
      use Ada.Strings.Unbounded;

      package Id_Maps_of_Command_Outputs is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                            Element_Type    => Safe_Client_Outputs_Access,
                                                                            Hash            => Hash,
                                                                            Equivalent_Keys => "=");
      Command_Outputs_Map : Id_Maps_of_Command_Outputs.Map;

      Server_In_Pipe  : constant Shell.Pipe := To_Pipe;
      Server_Out_Pipe : constant Shell.Pipe := To_Pipe (Blocking => False);
      Server_Err_Pipe : constant Shell.Pipe := To_Pipe;

      Command_Line     : Unbounded_String;
      Have_New_Command : Boolean := False;
      Command_Input    : Data_Holder;

      Killing_Command      : Command_Id := Null_Id;
      Interrupting_Command : Command_Id := Null_Id;
      Pausing_Command      : Command_Id := Null_Id;
      Resuming_Command     : Command_Id := Null_Id;

      Server_Input_Stream  : aliased Pipe_Stream := Stream (Server_In_Pipe);
      Server_Output_Stream : aliased Pipe_Stream := Stream (Server_Out_Pipe);

      Next_Id        : Command_Id := 1;
      Stopping       : Boolean    := False;
      Server_Is_Done : Boolean    := False;

      Spawn_Server : Shell.Process with Unreferenced;

   begin
      Spawn_Server := Start (Program => "ashell_spawn_server",
                             Input   => Server_In_Pipe,
                             Output  => Server_Out_Pipe,
                             Errors  => Server_Err_Pipe);

      Close (Server_In_Pipe,  Only_Read_End  => True);
      Close (Server_Out_Pipe, Only_Write_End => True);
      Close (Server_Err_Pipe);

      log ("Starting Spawn_Client");

      loop
         select
            accept Add (The_Command : in Command;
                        Input       : in Data   := No_Data;
                        Outputs     : in Safe_Client_Outputs_Access)
            do
               Log ("");
               Log ("Client: Accepting new command.");

               Have_New_Command := True;

               Set_Unbounded_String (Command_Line,
                                     Name (The_Command)
                                     & " "
                                     & Arguments (The_Command));

               Command_Input.Replace_Element (Input);
               Command_Outputs_Map.Insert (Next_Id,
                                           Outputs);
            end Add;
         or
            accept Kill (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Killing command.");

               Killing_Command := The_Command.Id;
            end Kill;
         or
            accept Interrupt (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Interrupt command.");

               Interrupting_Command := The_Command.Id;
            end Interrupt;
         or
            accept Pause (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Pause command.");

               Pausing_Command := The_Command.Id;
            end Pause;
         or
            accept Resume (The_Command : in Command)
            do
               Log ("");
               Log ("Client: Resume command.");

               Resuming_Command := The_Command.Id;
            end Resume;
         or
            accept Stop
            do
               Log ("Client: Stopping.");
               Stopping := True;
            end Stop;
         or
            delay 0.01;
         end select;


         if Stopping
         then
            Log ("Client is stopping.");
            Server_Action'Output (Server_Input_Stream'Access,
                                  Server_Action' (Stop,
                                                  Null_Id));
            Log ("Client asks server to stop.");
            Stopping := False;

         elsif Have_New_Command
         then
            Log ("New Command:" & Next_Id'Image & "   '" & (+Command_Line) & "'");

            Server_Action'Output (Server_Input_Stream'Access,
                                  Server_Action' (New_Command,
                                                  Next_Id,
                                                  Command_Line,
                                                  Command_Input));

            Have_New_Command := False;
            Next_Id          := Next_Id + 1;

         elsif Killing_Command /= Null_Id
         then
            Log ("Sending 'Kill' action for command" & Killing_Command'Image & " to server.");

            Server_Action'Output (Server_Input_Stream'Access,
                                  Server_Action' (Kill,
                                                  Killing_Command));
            Killing_Command := Null_Id;

         elsif Interrupting_Command /= Null_Id
         then
            Log ("Sending 'Interrupt' action for command" & Interrupting_Command'Image & " to server.");

            Server_Action'Output (Server_Input_Stream'Access,
                                  Server_Action' (Interrupt,
                                                  Interrupting_Command));
            Interrupting_Command := Null_Id;

         elsif Pausing_Command /= Null_Id
         then
            Log ("Sending 'Pause' action for command" & Pausing_Command'Image & " to server.");

            Server_Action'Output (Server_Input_Stream'Access,
                                  Server_Action' (Pause,
                                                  Pausing_Command));
            Pausing_Command := Null_Id;

         elsif Resuming_Command /= Null_Id
         then
            Log ("Sending 'Resume' action for command" & Resuming_Command'Image & " to server.");

            Server_Action'Output (Server_Input_Stream'Access,
                                  Server_Action' (Resume,
                                                  Resuming_Command));
            Resuming_Command := Null_Id;
         end if;


         if not Is_Empty (Server_Out_Pipe, Timeout => 0.06)
         then
            delay 0.01;

            declare
               Action          : constant Client_Action := Client_Action'Input (Server_Output_Stream'Access);
               Command_Outputs : Safe_Client_Outputs_Access;
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
                     Command_Outputs.Set_Done (Normal_Exit => Action.Normal_Exit);
                     Command_Outputs_Map.Delete (Action.Id);

                  when Server_Done =>
                     Log ("Server is done.");
                     Server_Is_Done := True;
               end case;
            end;
         end if;

         exit when Server_Is_Done
               and Command_Outputs_Map.Is_Empty;
      end loop;

      Close (Server_In_Pipe,  Only_Write_End => True);
      Close (Server_Out_Pipe, Only_Read_End  => True);

      Log ("Client is done.");

   exception
      when Process_Error =>
         Ada.Text_IO.New_Line (2);
         Ada.Text_IO.Put_Line ("__________________________________________________________________");
         Ada.Text_IO.Put_Line ("Program 'ashell_spawn_server' not found on PATH. Please install it.");
         Ada.Text_IO.Put_Line ("Spawn client is shutting down.");
         Ada.Text_IO.Put_Line ("__________________________________________________________________");
         Ada.Text_IO.New_Line (2);

      when E : others =>
         Log ("Unhandled error in Spawn_Client.");
         Log (Ada.Exceptions.Exception_Information (E));
   end Spawn_Client;



   ---------
   --- Forge
   --
   package body Forge
   is

      function To_Command (Command_Line : in String) return Command
      is
         --  use Ada.Strings.Fixed;
         --  I : constant Natural := Index (Command_Line, " ");
      begin
         --  if I = 0
         --  then
         --     return Result : Command
         --     do
         --        Result.Name       := +Command_Line;
         --        Result.Copy_Count := new Count' (1);
         --     end return;
         --  end if;

         declare
            --  Name      : constant String       :=               Command_Line (Command_Line'First .. I - 1);
            --  Arguments : constant String_Array := To_Arguments (Command_Line (I + 1              .. Command_Line'Last));
         begin
            return Result : Command
            do
               Define (Result, Command_Line);
               --  Result.Name       := +(Name);
               --  Result.Arguments  := To_String_Vector (Arguments);
               --  Result.Copy_Count := new Count' (1);
            end return;
         end;
      end to_Command;


      function To_Commands (Pipeline : in String) return Command_Array
      is
         use Ada.Strings.Fixed;

         Cursor : Positive := Pipeline'First;
         First,
         Last   : Positive;
         Count  : Natural := 0;

         Max_Commands_In_Pipeline : constant := 50;     -- Arbitrary.

         All_Commands : String_Array (1 .. Max_Commands_In_Pipeline);
      begin
         loop
            Find_Token (Source => Pipeline,
                        Set    => Ada.Strings.Maps.To_Set ('|'),
                        From   => Cursor,
                        Test   => Ada.Strings.Outside,
                        First  => First,
                        Last   => Last);
            declare
               Full_Command : constant String := Trim (Pipeline (First .. Last),
                                                       Ada.Strings.Both);
            begin
               Count                :=  Count + 1;
               All_Commands (Count) := +Full_Command;
            end;

            exit when Last = Pipeline'Last;

            Cursor := Last + 1;
         end loop;

         return Result : Command_Array (1 .. Count)
         do
            for i in 1 .. Count
            loop
               Define ( Result (i),
                       +All_Commands (i));
            end loop;
         end return;
      end To_Commands;

   end Forge;



   --- Start
   --

   overriding
   procedure Start (The_Command : in out Command;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := False)
   is
   begin
      if Input /= No_Data
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
         The_Command. Error_Pipe := To_Pipe (Blocking => False);
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

      --  Connect (Commands);

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
         --  if i /= Commands'First
         --  then
         --     Close_Pipe_Write_Ends (Commands (i - 1));    -- Close ends for the prior command.
         --  end if;

      end loop;

      --  Close_Pipe_Write_Ends (Commands (Commands'Last));  -- Close ends for the final command.
   end Start;



   --- Run
   --

   overriding
   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False)
   is
      Output      :         Data_Vector;
      Errors      :         Data_Vector;
      Normal_Exit :         Boolean;
   begin
      Spawn_Client.Add (The_Command,
                        Input,
                        The_Command.Safe_Outputs);

      The_Command.Safe_Outputs.Get_Outputs (Output,
                                            Errors,
                                            Normal_Exit);

      The_Command.Output := Output;
      The_Command.Errors := Errors;

      if        Raise_Error
        and not Normal_Exit
      then
         raise Command_Error with "Command '" & (+The_Command.Name) & "' failed.";
      end if;

   exception
      when Tasking_Error =>
         raise Command_Error with "Cannot run '" & (+The_Command.Name) & "'. The Spawn client has shut down.";
   end Run;



   overriding
   function Run (The_Command   : in out Command;
                 Input         : in     Data    := No_Data;
                 Raise_Error   : in     Boolean := False) return Command_Results
   is
   begin
      Run (The_Command, Input, Raise_Error);
      return Results_Of (The_Command);
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





   overriding
   procedure Wait_On (The_Command : in out Command)
   is
   begin
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
   procedure Kill (The_Command : in Command)
   is
   begin
      Spawn_Client.Kill (The_Command);
   end Kill;



   overriding
   procedure Interrupt (The_Command : in Command)
   is
   begin
      Spawn_Client.Interrupt (The_Command);
   end Interrupt;



   overriding
   procedure Pause (The_Command : in out Command)
   is
   begin
      Spawn_Client.Pause (The_Command);
      The_Command.Paused := True;
   end Pause;



   overriding
   procedure Resume (The_Command : in out Command)
   is
   begin
      Spawn_Client.Resume (The_Command);
      The_Command.Paused := False;
   end Resume;



   overriding
   procedure Finalize (The_Command : in out Command)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (Safe_Client_Outputs,
                                                              Safe_Client_Outputs_Access);
   begin
      if The_Command.Copy_Count.all = 1
      then
         Deallocate (The_Command.Safe_Outputs);
      end if;

      Finalize (Commands.Command (The_Command));
   end Finalize;



   procedure Stop_Spawn_Client
   is
   begin
      Spawn_Client.Stop;
   end Stop_Spawn_Client;



end Shell.Commands.Safe;
