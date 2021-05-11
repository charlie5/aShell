with
     Ada.Strings.Fixed,
     Ada.Unchecked_Conversion,
     Ada.Text_IO,
     Ada.Task_Identification,
     Ada.Characters.Handling,
     Ada.Exceptions;

package body Shell.Commands.Safe
is
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
             Ada.Task_Identification,
             Ada.Text_IO;
         Restart_Pipeline : Boolean  := False;
         Retry_Count      : Natural  := 0;
      begin
         loop
            begin
               Gather_Results (The_Command);   -- Gather on-going results.

               if   The_Command.Error_Count > 5
                 or Restart_Pipeline
               then
                  Retry_Count             := Retry_Count + 1;
                  Restart_Pipeline        := False;
                  The_Command.Error_Count := 0;

                  Clear (The_Command.Output);
                  Clear (The_Command.Errors);

                  Close (The_Command. Input_Pipe);
                  Close (The_Command.Output_Pipe);
                  Close (The_Command. Error_Pipe);

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
                        Restart_Pipeline := True;
                     else
                        Gather_Results (The_Command);   -- Gather any final results.
                        exit;
                     end if;

                  else
                     if Retry_Count < Retry
                     then
                        Restart_Pipeline := True;

                     elsif Raise_Error
                     then
                        declare
                           Error : constant String :=   "The command '" & (+The_Command.Name) & "' failed.";
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
                  Put_Line (Image (Current_Task) & "   "  & Exception_Information (E));
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
             Ada.Task_Identification,
             Ada.Text_IO;
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
                     begin
                        Close (Each. Input_Pipe);
                        Close (Each.Output_Pipe);
                        Close (Each. Error_Pipe);

                        Stop (Each);
                     end;
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
                  Put_Line (Image (Current_Task) & "   "  & Exception_Information (E));
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


   function Run (Command_Line : in String;
                 Input        : in Data  := No_Data) return Command_Results
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
            return Safe.Run (The_Commands, Input);
         end;
      else
         declare
            The_Command : Command := To_Command (Command_Line);
         begin
            return Safe.Run (The_Command, Input);
         end;
      end if;
   end Run;


   procedure Run (Command_Line : in String;
                  Input        : in Data  := No_Data)
   is
      Results : Command_Results := Safe.Run (Command_Line, Input) with Unreferenced;
   begin
      null;
   end Run;


end Shell.Commands.Safe;
