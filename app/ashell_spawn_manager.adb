with
     Shell.Commands,
     Ada.Text_IO,
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

begin
   Create (Log_File, Out_File, "aShell_spawn_Manager.error_log");

   for i in 1 .. 20
   loop
      delay 0.1;

      begin
         declare
            Input       : constant String  := +Output_Of (Shell.Standard_Input);
            A1          :          Boolean := Log ("Input => '" & Input & "'");

            The_Command :          Command := Forge.To_Command (Input);
            A2          :          Boolean := Log (Image (The_Command));

            Output      : constant Data    := Output_Of (Run (The_Command));
            A3          :          Boolean := Log (+Output);
         begin
            Write_To (Shell.Standard_Output,
                      Output);
         end;


      exception
         when Shell.No_Output_Error =>   -- No new command.
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
