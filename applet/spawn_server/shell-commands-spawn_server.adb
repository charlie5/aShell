with
     Shell.Commands.Unsafe.Privy,

     Gnat.OS_Lib,

     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Hashed_Maps,
     Ada.Exceptions;


procedure Shell.Commands.Spawn_Server
is
   -- The client's actions arrive on the standard input. A program started without any input would otherwise inherit it,
   -- and read those actions.
   --
   Client_Input : constant Pipe := Move_Standard_Input;

   Log_Is_Open  : Boolean := False;

   -- Written to when an action has been queued, so that the main loop can sleep until there is something to do. It
   -- can't watch the client's input instead, which may show as read before the action has reached the queue.
   --
   Wake_Up      : constant Pipe := To_Pipe (Blocking => False);

   package Server_Action_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Server_Action);


   protected New_Actions
   is
      procedure Add (Action    : in     Server_Action;
                     Was_Empty :    out Boolean);
      procedure Get (Action :    out Server_Action);
      function  Has_Actions return Boolean;
   private
      Actions : Server_Action_Vectors.Vector;
   end New_Actions;


   protected
   body New_Actions
   is
      procedure Add (Action    : in     Server_Action;
                     Was_Empty :    out Boolean)
      is
      begin
         Was_Empty := Actions.Is_Empty;
         Actions.Append (Action);
      end Add;


      function Has_Actions return Boolean
      is
      begin
         return not Actions.Is_Empty;
      end Has_Actions;


      procedure Get (Action : out Server_Action)
      is
      begin
         if not Actions.Is_Empty
         then
            Action := Actions.First_Element;
            Actions.Delete_First;
         else
            Action := (Nil, Null_Id);
         end if;
      end Get;

   end New_Actions;



   -- The main loop only sleeps when the queue is empty, so a wake up is needed only then. This
   -- also means that the wake up pipe can't fill, which would block the fetcher.
   --
   procedure Add_Action (Action : in Server_Action)
   is
      Was_Empty : Boolean;
   begin
      New_Actions.Add (Action, Was_Empty);

      if Was_Empty
      then
         Write_To (Wake_Up, Input => [1 => 0]);
      end if;
   end Add_Action;



   task New_Action_Fetcher
   is
      entry Start;
   end  New_Action_Fetcher;


   task body New_Action_Fetcher
   is
      Input_Stream : aliased Pipe_Stream := Stream (Client_Input);
   begin
      accept Start;

      loop
         declare
            Action : constant Server_Action := Server_Action'Input (Input_Stream'Access);
         begin
            Add_Action (Action);     -- Keep reading after a shutdown, as the client may yet kill a command.
         end;
      end loop;

   exception
      when E : others =>
         Log ("New_Action_Fetcher stopped reading.");
         Log (Ada.Exceptions.Exception_Information (E));

         Add_Action ((Shutdown, Null_Id));     -- The client has gone, so no more actions will arrive.
   end New_Action_Fetcher;


begin
   declare
      use type Unsafe.Command;
      package Id_Maps_of_Command is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                    Element_Type    => Unsafe.Command,
                                                                    Hash            => Hash,
                                                                    Equivalent_Keys =>  "=");
      Command_Map   :         Id_Maps_of_Command.Map;
      Output_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Output);
      Shutting_Down :         Boolean     := False;


      function To_Command (The_Line : in Command_Line) return Unsafe.Command
      is
      begin
         return Result : Unsafe.Command := Unsafe.Privy.To_Command_With_Pipes
         do
            Command (Result).Name      := The_Line.Name;
            Command (Result).Arguments := The_Line.Arguments;
         end return;
      end To_Command;


      function To_Commands (The_Lines : in Command_Lines) return Unsafe.Command_Array
      is
      begin
         return Result : Unsafe.Command_Array (1 .. Natural (The_Lines.Length))
         do
            for i in Result'Range
            loop
               Command (Result (i)).Name      := The_Lines.Element (i).Name;
               Command (Result (i)).Arguments := The_Lines.Element (i).Arguments;
            end loop;
         end return;
      end To_Commands;


      procedure Send_Error (Id      : in Command_Id;
                            Message : in String)
      is
      begin
         Client_Action'Output (Output_Stream'Access,
                               (New_Outputs,
                                Id,
                                Data_Holders.To_Holder (No_Data),
                                Data_Holders.To_Holder (+(Message & ASCII.LF))));
      end Send_Error;


      procedure Send_Done (Id           : in Command_Id;
                           Normal_Exit  : in Boolean;
                           Not_At_Fault : in Boolean := False)
      is
      begin
         Client_Action'Output (Output_Stream'Access,
                               (Command_Done,
                                Id,
                                Normal_Exit  => Normal_Exit,
                                Not_At_Fault => Not_At_Fault));
      end Send_Done;



      -- 'Stop' gathers what was left in the pipes it closes, and this is the only chance to send it.
      --
      procedure Send_Results_Of (Stopped : in out Unsafe.Command;
                                 Id      : in     Command_Id)
      is
         The_Results : constant Command_Results := Results_Of (Stopped);
      begin
         if    Output_Of (The_Results)'Length /= 0
            or Errors_Of (The_Results)'Length /= 0
         then
            Client_Action'Output (Output_Stream'Access,
                                  (New_Outputs,
                                   Id,
                                   Data_Holders.To_Holder (Output_Of (The_Results)),
                                   Data_Holders.To_Holder (Errors_Of (The_Results))));
         end if;
      end Send_Results_Of;


      procedure Perform (Action : in out Server_Action)
      is
         use Shell.Commands.Unsafe;
      begin
         -- A command may terminate before the client's action for it arrives.
         --
         if         Action.Kind in New_Input | Close_Input | Kill | Interrupt | Pause | Resume | Stop
           and then not Command_Map.Contains (Action.Id)
         then
            Log ("Ignoring " & Action.Kind'Image & " action for terminated command:" & Action.Id'Image);
            return;
         end if;

         -- Once shutting down, nothing would resume the command, which would then stop the server from ending.
         --
         if     Action.Kind = Pause
            and Shutting_Down
         then
            Log ("Ignoring Pause action during shutdown, for command:" & Action.Id'Image);
            return;
         end if;

         case Action.Kind
         is
            when Nil =>
               null;

            when New_Command =>
               Log ("New_Command action.");

               begin
                  declare
                     The_Command : Unsafe.Command := To_Command (Action.The_Line);
                  begin
                     The_Command.Start (Input => Action.Command_Input.Element,
                                        Accepts_Input => Action.Accepts_Input);
                     Log ("New Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");

                     Command_Map.Insert (Action.Id, The_Command);
                  end;
               exception
                  when E : others =>     -- Such as the program not being found.
                     Log (Ada.Exceptions.Exception_Information (E));

                     Send_Error (Action.Id, Ada.Exceptions.Exception_Message (E));
                     Send_Done  (Action.Id, Normal_Exit => False);
               end;

            when New_Pipeline =>
               Log ("New_Pipeline action.");

               declare
                  First_Id : constant Command_Id := Action.Id;
               begin
                  declare
                     use Shell.Commands.Unsafe.Privy;

                     The_Commands  : Unsafe.Command_Array := To_Commands (Action.The_Lines);
                     Error_Is_Sent : Boolean              := False;
                  begin
                     begin
                        Start (The_Commands,
                               Input    => Action.Pipeline_Input.Element,
                               Pipeline => True);

                     exception
                        when E : others =>     -- Such as a program not being found.
                           Log (Ada.Exceptions.Exception_Information (E));

                           -- Stop the commands which did start. Only the first which didn't is at
                           -- fault, so that the client can find it, and the error is reported against it.
                           --
                           for i in The_Commands'Range
                           loop
                              declare
                                 Started     : Boolean := True;
                                 Exited_Well : Boolean := False;     -- It may have ended before it could be stopped.
                              begin
                                 begin
                                    Started :=     Shell.Status (Unsafe.Process (The_Commands (i)).all)
                                                /= Shell.Not_Started;
                                    if Started
                                    then
                                       Privy.Stop (The_Commands (i));
                                       Exited_Well := The_Commands (i).Normal_Exit;
                                    end if;
                                 exception
                                    when Stop_Failure : others =>     -- The client must still be told of every command.
                                       Log (Ada.Exceptions.Exception_Information (Stop_Failure));
                                 end;

                                 if Started
                                 then
                                    Send_Results_Of (The_Commands (i), Action.Id);
                                 end if;

                                 declare
                                    At_Fault : constant Boolean :=         not (Started and i /= The_Commands'Last)
                                                                   and then not Error_Is_Sent;
                                 begin
                                    if At_Fault
                                    then
                                       Send_Error (Action.Id, Ada.Exceptions.Exception_Message (E));
                                       Error_Is_Sent := True;
                                    end if;

                                    Send_Done (Action.Id,
                                               Normal_Exit  => Exited_Well and not At_Fault,
                                               Not_At_Fault => not At_Fault);
                                 end;

                                 Action.Id := Action.Id + 1;
                              end;
                           end loop;

                           goto Pipeline_Done;
                     end;

                     Log ("New Pipeline: First command Id =>" & Action.Id'Image);

                     for i in The_Commands'Range
                     loop
                        if i /= The_Commands'Last
                        then
                           Is_Within_A_Pipeline (The_Commands (i));
                        end if;

                        Command_Map.Insert (Action.Id, The_Commands (i));
                        Action.Id := Action.Id + 1;
                     end loop;

                     <<Pipeline_Done>>
                  end;

               exception
                  when E : others =>     -- No command exists. Release the client's callers.
                     Log (Ada.Exceptions.Exception_Information (E));

                     Send_Error (First_Id, Ada.Exceptions.Exception_Message (E));

                     for Each in 0 .. Command_Id (Action.The_Lines.Length) - 1
                     loop
                        Send_Done (First_Id + Each, Normal_Exit => False);
                     end loop;
               end;

            when New_Input =>
               Log ("New_Input action.");

               declare
                  The_Command : constant Unsafe.Command := Command_Map.Element (Action.Id);
               begin
                  The_Command.Send (Action.Data.Element);
                  Log ("New Input sent to Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
               end;

            when Close_Input =>
               Log ("Close_Input action.");

               Command_Map.Element (Action.Id).Close_Input;

            when Kill =>
               Log ("Kill action.");

               declare
                  The_Command : Unsafe.Command renames Command_Map (Action.Id);     -- Not a copy, as it may see the program end, just once.
               begin
                  The_Command.Kill;
                  Log ("Killed Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
               end;

            when Interrupt =>
               Log ("Interrupt action.");

               declare
                  The_Command : Unsafe.Command renames Command_Map (Action.Id);     -- Not a copy, as it may see the program end, just once.
               begin
                  Privy.Interrupt_Without_Waiting (The_Command);
                  Log ("Interrupted Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
               end;

            when Pause =>
               Log ("Pause action.");

               declare
                  The_Command : Unsafe.Command renames Command_Map (Action.Id);     -- Not a copy, as it may see the program end, just once.
               begin
                  The_Command.Pause;
                  Log ("Paused Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
               end;

            when Resume =>
               Log ("Resume action.");

               declare
                  The_Command : Unsafe.Command renames Command_Map (Action.Id);     -- Not a copy, as it may see the program end, just once.
               begin
                  The_Command.Resume;
                  Log ("Resumes Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
               end;

            when Stop =>
               Log ("Stop action.");

               declare
                  use Shell.Commands.Unsafe.Privy;

                  The_Command : Unsafe.Command renames Command_Map (Action.Id);     -- Not a copy, as it may see the program end, just once.
               begin
                  Stop (The_Command);
                  Log ("Stop Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");

                  Send_Results_Of (The_Command, Action.Id);
               end;

            when Shutdown =>
               Log ("Shutdown action.");
               Shutting_Down := True;

               -- The client cannot send more input to a command waiting on it, which would then
               -- prevent the server, and the client, from ending. The main loop sees to any paused commands.
               --
               for Each of Command_Map
               loop
                  Each.Close_Input;
               end loop;
         end case;

      exception
         when E : others =>     -- Keep serving the other commands. For example, sending to a command which has no input.
            Log ("Failed to perform " & Action.Kind'Image & " action for command:" & Action.Id'Image);
            Log (Ada.Exceptions.Exception_Information (E));
      end Perform;


      -- A pass of the main loop can take a while, as it reads every command's pipes.
      -- So take all of the waiting actions, up to a limit which leaves those pipes still being read.
      --
      procedure Perform_Waiting_Actions
      is
         Most_Actions_Per_Pass : constant := 256;
      begin
         for Each in 1 .. Most_Actions_Per_Pass
         loop
            declare
               Action : Server_Action;
            begin
               New_Actions.Get (Action);

               exit when Action.Kind = Nil;

               Perform (Action);
            end;
         end loop;
      end Perform_Waiting_Actions;


      -- A program usually closes its pipes just before it can be seen to have ended, and nothing would then wake the
      -- main loop. So check quickly for a while, which also covers a program which closes its output and carries on.
      --
      Quick_Checks : Natural := 0;


      Ready : Pipe_Id_Sets.Set;     -- The pipes which the last wait found ready, so that the others needn't be asked.


      function Read_Noting_End (The_Pipe : in Pipe) return Data
      is
         Result : constant Data := Output_If_Ready (The_Pipe, Ready);
      begin
         if         Ready.Contains (The_Pipe.Id)
           and then not Is_Readable (The_Pipe)
         then
            Quick_Checks := 25;
         end if;

         return Result;
      end Read_Noting_End;


      -- Sleeps until there is output to send or an action to perform. A program
      -- may end without any change to its pipes, so wake up to check, now and then.
      --
      procedure Wait_For_Something
      is
         use Unsafe;

         The_Pipes : Pipe_Array (1 .. 1 + 2 * Natural (Command_Map.Length));
         Next      : Positive := 2;
      begin
         The_Pipes (1) := Wake_Up;

         for Each of Command_Map
         loop
            The_Pipes (Next)     := (if Privy.Is_Within_A_Pipeline (Each) then Null_Pipe     -- Its output goes to the next command.
                                                                          else Each.Output_Pipe);
            The_Pipes (Next + 1) := Each.Error_Pipe;
            Next                 := Next + 2;
         end loop;

         -- With actions waiting, a burst of them may take more than one pass, so just see which pipes are ready.
         --
         Wait_For_Output (The_Pipes,
                          Timeout => (if    New_Actions.Has_Actions then 0.0
                                      elsif Quick_Checks > 0        then 0.001
                                      else                               0.05),
                          Ready   => Ready);

         if Quick_Checks > 0
         then
            Quick_Checks := Quick_Checks - 1;
         end if;

         declare
            Unused : constant Data := Output_If_Ready (Wake_Up, Ready);
         begin
            null;
         end;
      end Wait_For_Something;


   begin
      Integer'Write (Output_Stream'Access, Protocol_Version);     -- First of all, so that nothing here can make the client wait for it.

      begin
         Open_Log ("aShell_spawn_Server.log");
         Log_Is_Open := True;
      exception
         when others =>     -- The working directory may not be writeable. Commands can still be run, without a log.
            null;
      end;

      Log ("Starting Spawn Server");


      New_Action_Fetcher.Start;

      loop
         begin
            Wait_For_Something;
            Perform_Waiting_Actions;


            declare
               use Id_Maps_of_Command;
               package Command_Id_Vectors is new Ada.Containers.Vectors (Positive, Command_Id);

               Done_Commands : Command_Id_Vectors.Vector;
               Cursor        : Id_Maps_of_Command.Cursor := Command_Map.First;
            begin
               -- Send ongoing command results to the client.
               --
               while Has_Element (Cursor)
               loop
                  declare
                     use Data_Holders,
                         Shell.Commands.Unsafe.Privy,
                         Shell.Commands.Unsafe;

                     Id          :          constant Command_Id := Key (Cursor);
                     The_Command : Unsafe.Command renames Command_Map (Cursor);     -- Not a copy, as a program stopping is seen only once.

                     procedure Send (Output : in Data;
                                     Errors : in Data)
                     is
                     begin
                        if not (    Output'Length = 0
                                and Errors'Length = 0)
                        then
                           Client_Action'Output (Output_Stream'Access,
                                                 (New_Outputs,
                                                  Id,
                                                  To_Holder (Output),
                                                  To_Holder (Errors)));
                        end if;
                     end Send;


                     -- The output of a command within a pipeline goes to the next command. Its error
                     -- output must still be read, or it would block when its error pipe fills.
                     --
                     procedure Send_New_Results
                     is
                     begin
                        if Is_Within_A_Pipeline (The_Command)
                        then
                           Send (Output => No_Data,
                                 Errors => Read_Noting_End (The_Command.Error_Pipe));
                        else
                           --  log ("Send new results for command: " & Image (The_Command));

                           Send (Output => Read_Noting_End (The_Command.Output_Pipe),
                                 Errors => Read_Noting_End (The_Command. Error_Pipe));
                        end if;
                     end Send_New_Results;


                     -- The command has ended, so this is the last chance to read what it left in its pipes.
                     --
                     procedure Send_Final_Results
                     is
                     begin
                        for Each in 1 .. Most_Reads_Per_Gather
                        loop
                           declare
                              Output : constant Data := Shell.Output_Of (The_Command.Output_Pipe);     -- Finds nothing within a pipeline.
                              Errors : constant Data := Shell.Output_Of (The_Command. Error_Pipe);
                           begin
                              exit when Output'Length = 0
                                    and Errors'Length = 0;

                              Send (Output, Errors);
                           end;
                        end loop;
                     end Send_Final_Results;

                  begin
                     Send_New_Results;        -- Send ongoing results.

                     if The_Command.Has_Terminated
                     then
                        Log ("Command: " & Id'Image & " has terminated. Normal exit: " & The_Command.Normal_Exit'Image);

                        Send_Final_Results;

                        declare
                           Act : constant Client_Action := (Command_Done,
                                                            Id,
                                                            Normal_Exit  => The_Command.Normal_Exit,
                                                            Not_At_Fault => False);
                        begin
                           Client_Action'Output (Output_Stream'Access, Act);
                        end;

                        Done_Commands.Append (Id);

                     -- Nothing could resume it, and it would stop the server and the client from ending. It
                     -- may have stopped since the shutdown began, and may not have been stopped by the client.
                     --
                     elsif     Shutting_Down
                           and The_Command.Status = Paused
                     then
                        The_Command.Kill;
                        Log ("Killed paused Command:" & Id'Image);
                     end if;
                  end;

                  Next (Cursor);
               end loop;

               -- Rid completed commands.
               --
               for Each of Done_Commands
               loop
                  Command_Map.Delete (Each);
               end loop;
            end;


            exit when Shutting_Down
                  and Command_Map.Is_Empty;

         end;
      end loop;


      declare
         Act : constant Client_Action := (Server_Done, Null_Id);
      begin
         Client_Action'Output (Output_Stream'Access, Act);
      end;

      Log ("Ending Spawn Server");

   exception
      when others =>
         for Each of Command_Map     -- Leave no commands running, without a server to manage them.
         loop
            begin
               if Each.Status not in Normal_Exit | Failed_Exit | Killed
               then
                  Each.Kill;
               end if;
            exception
               when E : others =>
                  Log (Ada.Exceptions.Exception_Information (E));
            end;
         end loop;

         raise;
   end;

   if Log_Is_Open
   then
      Close_Log;
   end if;

   -- The new action fetcher may still be blocked reading from the client. So end now, rather than waiting on it.
   --
   Gnat.OS_Lib.OS_Exit (0);

exception
   when E : others =>
      begin
         Log ("Unhandled error in aShell_Spawn_Server.");
         Log (Ada.Exceptions.Exception_Information (E));

         if Log_Is_Open
         then
            Close_Log;
         end if;
      exception
         when others =>     -- Nothing must prevent the exit below.
            null;
      end;

      -- The new action fetcher is blocked reading from the client, which would otherwise keep this process alive and
      -- leave the client waiting for ever. Ending lets the client see that the server has gone.
      --
      Gnat.OS_Lib.OS_Exit (1);
end Shell.Commands.Spawn_Server;
