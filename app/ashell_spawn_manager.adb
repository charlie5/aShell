with
     Shell.Commands,
     Ada.Text_IO,
     Ada.IO_Exceptions,
     Ada.Exceptions;

procedure aShell_spawn_Manager
is
   use Shell,
       Shell.Commands,
       Ada.Text_IO;

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

   Input_Stream  : aliased Pipe_Stream := Stream (Shell.Standard_Input);
   Output_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Output);
   Errors_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Error);

begin
   Create (Log_File, Out_File, "aShell_spawn_Manager.error_log");

   for i in 1 .. 20
   loop
      delay 0.1;
      log ("loop: " & i'Image);

      begin
         declare
            Input       : constant String  := String'Input (Input_Stream'Access);
            A1          :          Boolean := Log ("Input => '" & Input & "'") with Unreferenced;

            The_Command :          Command := Forge.To_Command (Input);
            A2          :          Boolean := Log (Image (The_Command)) with Unreferenced;

            Results     : constant Command_Results := Run (The_Command);

            Output      : constant Data    := Output_Of (Results);
            A3          :          Boolean := Log (+Output) with Unreferenced;

            Errors      : constant Data    := Errors_Of (Results);
            A4          :          Boolean := Log (+Errors) with Unreferenced;
         begin
            Data'Output (Output_Stream'Access, Output);
            Data'Output (Errors_Stream'Access, Errors);
         end;

      exception
         when Ada.IO_Exceptions.End_Error =>   -- No new command.
            delay 0.1;
      end;
   end loop;

   Close (Log_File);

exception
   when E : others =>
      Log ("Unhandled error in aShell_spawn_Manager.");
      Log (Ada.Exceptions.Exception_Information (E));
      Close (Log_File);
end aShell_spawn_Manager;
