with
     Ada.Strings.Fixed,
     Ada.Unchecked_Conversion,
     Ada.Task_Identification,
     Ada.Characters.Handling,
     Ada.Containers.Hashed_Maps,
     Ada.IO_Exceptions,
     Ada.Exceptions;

package body Shell.Commands.Safe
is

   ----------------------
   --- Safe_Client_Output
   --

   protected
   type Safe_Client_Outputs
   is
      procedure Add_Outputs (Output : in     Shell.Data;
                             Errors : in     Shell.Data);

      entry     Get_Outputs (Output      :    out Data_Vector;
                             Errors      :    out Data_Vector;
                             Normal_Exit :    out Boolean);

      procedure Set_Done (Normal_Exit : in   Boolean);

   private
      All_Output : Data_Vector;
      All_Errors : Data_Vector;

      Exit_Is_Normal : Boolean;
      Done           : Boolean := False;
   end Safe_Client_Outputs;


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


      entry Get_Outputs (Output      :    out Data_Vector;
                         Errors      :    out Data_Vector;
                         Normal_Exit :    out Boolean) when Done
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

   end Safe_Client_Outputs;


   type Safe_Client_Outputs_Access is access all Safe_Client_Outputs;


   ----------------
   --- Spawn_Client
   --

   task Spawn_Client
   is
      entry Add (The_Command : in Command;
                 Input       : in     Data := No_Data;
                 Outputs     : in Safe_Client_Outputs_Access);
      entry Stop;
   end Spawn_Client;


   task body Spawn_Client
   is
      use Ada.Strings.Unbounded;

      package Id_Maps_of_Command_Outputs is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                            Element_Type    => Safe_Client_Outputs_Access,
                                                                            Hash            => Hash,
                                                                            Equivalent_Keys =>  "=");
      Command_Outputs_Map : Id_Maps_of_Command_Outputs.Map;

      Server_In_Pipe  : constant Shell.Pipe := To_Pipe;
      Server_Out_Pipe : constant Shell.Pipe := To_Pipe (Blocking => False);
      Server_Err_Pipe : constant Shell.Pipe := To_Pipe;

      Spawn_Server : Shell.Process  := Start (Program   => "ashell_spawn_server",
                                              Input     => Server_In_Pipe,
                                              Output    => Server_Out_Pipe,
                                              Errors    => Server_Err_Pipe) with Unreferenced;
      Command_Line     : Unbounded_String;
      Have_New_Command : Boolean := False;
      Command_Input    : Data_Holder;

      Server_Input_Stream  : aliased Pipe_Stream := Stream (Server_In_Pipe);
      Server_Output_Stream : aliased Pipe_Stream := Stream (Server_Out_Pipe);

      Next_Id        : Command_Id := 1;
      Stopping       : Boolean    := False;
      Server_Is_Done : Boolean    := False;

   begin
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
               log ("");
               log ("Client: Accepting new command.");

               Have_New_Command := True;
               Set_Unbounded_String (Command_Line,
                                     Name (The_Command) & " " & Arguments (The_Command));
               Command_Input.Replace_Element (Input);
               Command_Outputs_Map.Insert (Next_Id,
                                           Outputs);
            end Add;
         or
            accept Stop
            do
               log ("Client: Stopping.");
               Stopping := True;
            end Stop;
         or
            delay 0.01;
         end select;

         if Stopping
         then
            log ("Client is stopping.");
            Server_Action'Output (Server_Input_Stream'Access,
                                  (Stop,
                                   Null_Id));
            log ("Client asks server to stop.");
            Stopping := False;

         elsif Have_New_Command
         then
            log ("New Command:" & Next_Id'Image & "   '" & (+Command_Line) & "'");

            Server_Action'Output (Server_Input_Stream'Access,
                                  (New_Command,
                                   Next_Id,
                                   Command_Line,
                                   Command_Input));

            Have_New_Command := False;
            Next_Id          := Next_Id + 1;
         end if;

         begin
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
                     Command_Outputs := Command_Outputs_Map.Element (Action.Id);
                     Command_Outputs.Add_Outputs (Action.Output.Element,
                                                  Action.Errors.Element);
                  when Command_Done =>
                     Log ("Command Done:" & Action.Id'Image);
                     Command_Outputs := Command_Outputs_Map.Element (Action.Id);
                     Command_Outputs.Set_Done (Normal_Exit => Action.Normal_Exit);
                     Command_Outputs_Map.Delete (Action.Id);

                  when Server_Done =>
                     Server_Is_Done := True;
                     Log ("Server is done.");
                  end case;
               end;
            end if;
         end;

         exit when Server_Is_Done
               and Command_Outputs_Map.Is_Empty;
      end loop;

      log ("Client is done.");

      Close (Server_In_Pipe,  Only_Write_End => True);
      Close (Server_Out_Pipe, Only_Read_End  => True);

   exception
      when E : others =>
         Log ("Unhandled error in Spawn_Client.");
         Log (Ada.Exceptions.Exception_Information (E));
   end Spawn_Client;



   procedure Runn (The_Command : in out Command;
                   Input       : in     Data    := No_Data;
                   Raise_Error : in     Boolean := False)
   is
      Outputs     : aliased Safe_Client_Outputs;
      Output      :         Data_Vector;
      Errors      :         Data_Vector;
      Normal_Exit :         Boolean;
   begin
      Spawn_Client.Add (The_Command,
                        Input,
                        Outputs'Unchecked_Access);

      Outputs.Get_Outputs (Output,
                           Errors,
                           Normal_Exit);

      The_Command.Output := Output;
      The_Command.Errors := Errors;

      if        Raise_Error
        and not Normal_Exit
      then
         raise Command_Error with "Command '" & (+The_Command.Name) & "' failed.";
      end if;
   end Runn;


   procedure Stop_Spawn_Client
   is
   begin
      Spawn_Client.Stop;
   end Stop_Spawn_Client;



   function Runn (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False) return Command_Results
   is
   begin
      Runn (The_Command, Input, Raise_Error);

      return Results_Of (The_Command);
   end Runn;


   -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

   --- Run
   --

   procedure Run (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True)
   is
      use Ada.Characters.Handling,
          Ada.Exceptions;
   begin
      The_Command.Owns_Output_Pipe := True;

      The_Command.Output_Pipe := To_Pipe (Blocking => False);
      The_Command. Error_Pipe := To_Pipe (Blocking => False);

      Start (The_Command, Input);

      declare
         use Data_Vectors,
             Ada.Task_Identification;
         Restart_Command : Boolean  := False;
         Retry_Count     : Natural  := 0;
      begin
         loop
            begin
               Gather_Results (The_Command);   -- Gather on-going results.

               if Restart_Command
               then
                  Log ("restarting command: " & The_Command.Error_Count'Image & "  " & Restart_Command'Image);
                  Retry_Count             := Retry_Count + 1;
                  Restart_Command         := False;
                  The_Command.Error_Count := 0;

                  Clear (The_Command.Output);
                  Clear (The_Command.Errors);

                  Stop (The_Command);

                  The_Command.Output_Pipe := To_Pipe;
                  The_Command. Error_Pipe := To_Pipe;

                  Start (The_Command, Input);
               end if;

               if Has_Terminated (The_Command.Process)
               then
                  if Normal_Exit (The_Command.Process)
                  then
                     if (    Expect_Output
                         and Is_Empty (The_Command.Output))
                       or not Is_Readable (The_Command.Output_Pipe)
                       or not Is_Readable (The_Command.Error_Pipe)
                     then
                        Restart_Command := True;
                     else
                        Gather_Results (The_Command);   -- Gather any final results.
                        exit;
                     end if;

                  else
                     if Retry_Count < Retry
                     then
                        Restart_Command := True;

                     elsif Raise_Error
                     then
                        declare
                           Error : constant String := "The command '" & (+The_Command.Name) & "' failed.";
                        begin
                           raise Command_Error with Error;
                        end;

                     else
                        exit;
                     end if;
                  end if;

               end if;

            exception
               when E : POSIX.POSIX_Error =>
                  if To_Upper (Exception_Message (E)) = "INVALID_ARGUMENT"
                  then
                     Restart_Command := True;
                  else
                     raise;
                  end if;

               when E : others =>
                  Log (Image (Current_Task) & "   "  & Exception_Information (E));
                  raise;
            end;
         end loop;

      end;
   end Run;


   function Run (The_Command   : in out Command;
                 Input         : in     Data    := No_Data;
                 Raise_Error   : in     Boolean := False;
                 Retry         : in     Natural := Natural'Last;
                 Expect_Output : in     Boolean := True) return Command_Results
   is
   begin
      Run (The_Command, Input, Raise_Error, Retry, Expect_Output);

      return Results_Of (The_Command);
   end Run;


   procedure Run (The_Pipeline  : in out Command_Array;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True)
   is
      use Ada.Characters.Handling,
          Ada.Exceptions;
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Last_Command.Output_Pipe := To_Pipe;
      Start (The_Pipeline, Input);

      declare
         use Data_Vectors,
             Ada.Task_Identification;
         Restart_Pipeline : Boolean  := False;
         Retry_Count      : Natural  := 0;
         i                : Positive := 1;
      begin
         loop
            begin
               Gather_Results (Last_Command);   -- Gather on-going results.

               if   Last_Command.Error_Count > 5
                 or Restart_Pipeline
               then
                  Retry_Count              := Retry_Count + 1;
                  Restart_Pipeline         := False;
                  Last_Command.Error_Count := 0;

                  Clear (Last_Command.Output);
                  Clear (Last_Command.Errors);

                  for Each of The_Pipeline
                  loop
                     Stop (Each);
                  end loop;

                  Last_Command.Output_Pipe := To_Pipe;
                  Start (The_Pipeline, Input);
                  i := 1;
               end if;

               if Has_Terminated (The_Pipeline (i).Process)
               then
                  if Normal_Exit (The_Pipeline (i).Process)
                  then
                     i := i + 1;

                     if i > The_Pipeline'Last
                     then
                        if (    Expect_Output
                            and Is_Empty (Last_Command.Output))
                          or not Is_Readable (Last_Command.Output_Pipe)
                          or not Is_Readable (Last_Command.Error_Pipe)
                        then
                           Restart_Pipeline := True;
                        else
                           Gather_Results (Last_Command);   -- Gather any final results.
                           exit;
                        end if;
                     end if;

                  else
                     if Retry_Count < Retry
                     then
                        Restart_Pipeline := True;

                     elsif Raise_Error
                     then
                        declare
                           Error : constant String :=   "Pipeline command" & Integer'Image (i)
                                                      & " '" & (+The_Pipeline (i).Name) & "' failed.";
                        begin
                           raise Command_Error with Error;
                        end;

                     else
                        exit;
                     end if;
                  end if;

               end if;

            exception
               when E : POSIX.POSIX_Error =>
                  if To_Upper (Exception_Message (E)) = "INVALID_ARGUMENT"
                  then
                     Restart_Pipeline := True;
                  else
                     raise;
                  end if;

               when E : others =>
                  Log (Image (Current_Task) & "   "  & Exception_Information (E));
                  raise;
            end;
         end loop;

      end;
   end Run;


   function Run (The_Pipeline  : in out Command_Array;
                 Input         : in     Data    := No_Data;
                 Raise_Error   : in     Boolean := False;
                 Retry         : in     Natural := Natural'Last;
                 Expect_Output : in     Boolean := True) return Command_Results
   is
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Run (The_Pipeline, Input, Raise_Error, Retry, Expect_Output);

      return Results_Of (Last_Command);
   end Run;


   function Run (Command_Line  : in String;
                 Input         : in Data    := No_Data;
                 Raise_Error   : in Boolean := False;
                 Retry         : in Natural := Natural'Last;
                 Expect_Output : in Boolean := True) return Command_Results
   is
      use Ada.Strings.Fixed,
          Shell.Commands.Forge;
      The_Index   : constant Natural := Index (Command_Line, " | ");
      Is_Pipeline : constant Boolean := The_Index /= 0;
   begin
      if Is_Pipeline
      then
         declare
            The_Commands : Command_Array := To_Commands (Command_Line);
         begin
            return Safe.Run (The_Commands, Input, Raise_Error, Retry, Expect_Output);
         end;
      else
         declare
            The_Command : Command := To_Command (Command_Line);
         begin
            return Safe.Run (The_Command, Input, Raise_Error, Retry, Expect_Output);
         end;
      end if;
   end Run;


   procedure Run (Command_Line  : in String;
                  Input         : in Data    := No_Data;
                  Raise_Error   : in Boolean := False;
                  Retry         : in Natural := Natural'Last;
                  Expect_Output : in Boolean := True)
   is
      Results : Command_Results := Safe.Run (Command_Line, Input, Raise_Error, Retry, Expect_Output) with Unreferenced;
   begin
      null;
   end Run;


end Shell.Commands.Safe;
