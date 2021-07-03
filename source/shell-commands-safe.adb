with
     Ada.Strings.Fixed,
     Ada.Unchecked_Conversion,
     Ada.Task_Identification,
     Ada.Characters.Handling,
     Ada.Containers.Hashed_Maps,
     Ada.Text_IO,
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

      Command_Line     : Unbounded_String;
      Have_New_Command : Boolean := False;
      Command_Input    : Data_Holder;

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

         exit when Server_Is_Done
               and Command_Outputs_Map.Is_Empty;
      end loop;

      log ("Client is done.");

      Close (Server_In_Pipe,  Only_Write_End => True);
      Close (Server_Out_Pipe, Only_Read_End  => True);

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



   procedure Run (The_Command : in out Command;
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

   exception
      when Tasking_Error =>
         raise Command_Error with "Spawn client has shut down.";
   end Run;


   function Run (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False) return Command_Results
   is
   begin
      Run (The_Command, Input, Raise_Error);

      return Results_Of (The_Command);
   end Run;


   procedure Stop_Spawn_Client
   is
   begin
      Spawn_Client.Stop;
   end Stop_Spawn_Client;


end Shell.Commands.Safe;
