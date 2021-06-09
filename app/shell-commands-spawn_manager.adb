with
Ada.Text_IO,
     Ada.Containers.Hashed_Maps,
     Ada.IO_Exceptions,
     Ada.Exceptions;

procedure Shell.Commands.Spawn_Manager
is
   use Ada.Text_IO;

   Log_File : File_Type;

   procedure log (Message : in String)
   is
   begin
      Put_Line (Log_File, Message);
   end log;

   function log (Message : in String) return Boolean
   is
   begin
      Log (Message);
      return True;
   end log;

begin
   Create (Log_File, Out_File, "aShell_spawn_Manager.error_log");
   log ("K1");
   declare



      package Id_Maps_of_Command is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                    Element_Type    => Command,
                                                                    Hash            => Hash,
                                                                    Equivalent_Keys =>  "=");
      Command_Map   : Id_Maps_of_Command.Map;

      Input_Stream  : aliased Pipe_Stream := Stream (Shell.Standard_Input);
      Output_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Output);
      Errors_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Error);

   begin
      --     --  Create (Log_File, Out_File, "aShell_spawn_Manager.error_log");
      log ("Starting Spawn Manager");

      for i in 1 .. 20
      loop
         delay 0.1;
         log ("loop: " & i'Image);

         begin
            declare
               Id          : constant Command_Id := Command_Id'Input (Input_Stream'Access);
               A0          :          Boolean    := Log ("Before Input") with Unreferenced;
               Input       : constant String     := String'Input (Input_Stream'Access);
               A1          :          Boolean    := Log ("Input => '" & Input & "'") with Unreferenced;

               The_Command :          Command    := Forge.To_Command (Input);
               --  A2          :          Boolean := Log (Image (The_Command)) with Unreferenced;

            begin
               The_Command.Owns_Output_Pipe := True;

               The_Command.Output_Pipe := To_Pipe (Blocking => False);
               The_Command. Error_Pipe := To_Pipe (Blocking => False);

               The_Command.Start;
               Log ("Command => '" & Image (The_Command) & "'");

               Command_Map.Insert (Id, The_Command);
               log ("After Insert");
            end;

            declare
               use Id_Maps_of_Command;
               Cursor : Id_Maps_of_Command.Cursor := Command_Map.First;
            begin
               delay 1.0;
               while Has_Element (Cursor)
               loop
                  declare
                     Id          : Command_Id := Key     (Cursor);
                     The_Command : Command    := Element (Cursor);

                     Results     : constant Command_Results := Results_Of (The_Command);

                     Output      : constant Data    := Output_Of (Results);
                     A3          :          Boolean := Log ("Output '" &(+Output) & "'") with Unreferenced;

                     Errors      : constant Data    := Errors_Of (Results);
                     A4          :          Boolean := Log ("Errors '" & (+Errors) & "'") with Unreferenced;
                  begin
                     null;
                     Data'Output (Output_Stream'Access, Output);
                     Data'Output (Errors_Stream'Access, Errors);
                  end;

                  Next (Cursor);
               end loop;
            end;
            log ("Done Cursor loop");
            --  exit;
         exception
            when Ada.IO_Exceptions.End_Error =>   -- No new command.
               log ("SM: End_Error");
               exit;
               delay 0.1;
         end;
      end loop;

      log ("SM: Done");
      Close (Log_File);
   end;

exception
   when E : others =>
      Log ("Unhandled error in aShell_spawn_Manager.");
      Log (Ada.Exceptions.Exception_Information (E));
      Close (Log_File);
end Shell.Commands.Spawn_Manager;
