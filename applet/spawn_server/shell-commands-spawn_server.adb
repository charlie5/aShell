with
     Shell.Commands.Unsafe.Privy,

     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Hashed_Maps,
     Ada.Exceptions;


procedure Shell.Commands.Spawn_Server
is

   package Server_Action_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Server_Action);


   protected New_Actions
   is
      procedure Add (Action : in     Server_Action);
      procedure Get (Action :    out Server_Action);
   private
      Actions : Server_Action_Vectors.Vector;
   end New_Actions;


   protected
   body New_Actions
   is
      procedure Add (Action : in Server_Action)
      is
      begin
         Actions.Append (Action);
      end Add;


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



   task New_Action_Fetcher
   is
      entry Start;
   end  New_Action_Fetcher;


   task body New_Action_Fetcher
   is
      Input_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Input);
   begin
      accept Start;

      loop
         declare
            Action : constant Server_Action := Server_Action'Input (Input_Stream'Access);
         begin
            New_Actions.Add (Action);
            exit when Action.Kind = Stop;
         end;
      end loop;

   exception
      when E : others =>
         Log ("Unhandled error in New_Action_Fetcher.");
         Log (Ada.Exceptions.Exception_Information (E));
   end New_Action_Fetcher;


begin
   Open_Log ("aShell_spawn_Server.log");

   declare
      use type Unsafe.Command;
      package Id_Maps_of_Command is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                    Element_Type    => Unsafe.Command,
                                                                    Hash            => Hash,
                                                                    Equivalent_Keys =>  "=");
      Command_Map   :         Id_Maps_of_Command.Map;
      Output_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Output);
      Shutting_Down :         Boolean     := False;
   begin
      Log ("Starting Spawn Server");

      New_Action_Fetcher.Start;

      loop
         Log ("");
         Log ("looping");

         begin
            declare
               use Shell.Commands.Unsafe;
               Action : Server_Action;
            begin
               New_Actions.Get (Action);

               case Action.Kind
               is
                  when Nil =>
                     delay 0.02;

                  when New_Command =>
                     Log ("New_Command action.");

                     declare
                        The_Command : Unsafe.Command := Privy.To_Command (+Action.Command_Line);
                     begin
                        The_Command.Start (Input => Action.Command_Input.Element,
                                           Accepts_Input => Action.Accepts_Input);
                        Log ("New Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");

                        Command_Map.Insert (Action.Id, The_Command);
                     end;

                  when New_Pipeline =>
                     Log ("New_Pipeline action.");

                     declare
                        use Shell.Commands.Unsafe.Privy;

                        The_Commands : Unsafe.Command_Array := Forge.To_Commands (+Action.Pipeline);
                     begin
                        Start (The_Commands,
                               Input    => Action.Pipeline_Input.Element,
                               Pipeline => True);

                        Log ("New Pipeline: First command Id =>" & Action.Id'Image & "   '" & (+Action.Pipeline) & "'");

                        for i in The_Commands'Range
                        loop
                           if i /= The_Commands'Last
                           then
                              Is_Within_A_Pipeline (The_Commands (i));
                           end if;

                           Command_Map.Insert (Action.Id, The_Commands (i));
                           Action.Id := Action.Id + 1;
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

                  when Kill =>
                     Log ("Kill action.");

                     declare
                        The_Command : Unsafe.Command := Command_Map.Element (Action.Id);
                     begin
                        The_Command.Kill;
                        Log ("Killed Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
                        Command_Map.Replace (Action.Id, The_Command);
                     end;

                  when Interrupt =>
                     Log ("Interrupt action.");

                     declare
                        The_Command : Unsafe.Command := Command_Map.Element (Action.Id);
                     begin
                        The_Command.Interrupt;
                        Log ("Interrupted Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
                        Command_Map.Replace (Action.Id, The_Command);
                     end;

                  when Pause =>
                     Log ("Pause action.");

                     declare
                        The_Command : Unsafe.Command := Command_Map.Element (Action.Id);
                     begin
                        The_Command.Pause;
                        Log ("Paused Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
                        Command_Map.Replace (Action.Id, The_Command);
                     end;

                  when Resume =>
                     Log ("Resume action.");

                     declare
                        The_Command : Unsafe.Command := Command_Map.Element (Action.Id);
                     begin
                        The_Command.Resume;
                        Log ("Resumes Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
                        Command_Map.Replace (Action.Id, The_Command);
                     end;

                  when Stop =>
                     Log ("Stop action.");

                     declare
                        use Shell.Commands.Unsafe.Privy;

                        The_Command : Unsafe.Command := Command_Map.Element (Action.Id);
                     begin
                        Stop (The_Command);
                        Log ("Stop Command:" & Action.Id'Image & "   '" & Image (The_Command) & "'");
                        Command_Map.Replace (Action.Id, The_Command);
                     end;

                  when Shutdown =>
                     Log ("Shutdown action.");
                     Shutting_Down := True;
               end case;
            end;


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

                     Id          : constant Command_Id := Key     (Cursor);
                     The_Command :   Unsafe.Command    := Element (Cursor);

                     procedure Send_New_Results
                     is
                     begin
                        if not Is_Within_A_Pipeline (The_Command)
                        then
                           --  log ("Send new results for command: " & Image (The_Command));

                           declare
                              Output : constant Data := Privy.Output_Of (The_Command.Output_Pipe);
                              Errors : constant Data := Privy.Output_Of (The_Command. Error_Pipe);
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
                           end;
                        end if;
                     end Send_New_Results;

                  begin
                     Send_New_Results;        -- Send ongoing results.

                     if The_Command.Has_Terminated
                     then
                        Log ("Command: " & Id'Image & " has terminated. Normal exit: " & The_Command.Normal_Exit'Image);

                        Send_New_Results;     -- Send any final results.

                        declare
                           Act : constant Client_Action := (Command_Done,
                                                            Id,
                                                            Normal_Exit => The_Command.Normal_Exit);
                        begin
                           Client_Action'Output (Output_Stream'Access, Act);
                        end;

                        Done_Commands.Append (Id);
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

            if Command_Map.Is_Empty
            then
               delay 0.1;
            end if;
         end;
      end loop;


      declare
         Act : constant Client_Action := (Server_Done, Null_Id);
      begin
         Client_Action'Output (Output_Stream'Access, Act);
      end;

      Log ("Ending Spawn Server");
   end;

   Close_Log;

exception
   when E : others =>
      Log ("Unhandled error in aShell_Spawn_Server.");
      Log (Ada.Exceptions.Exception_Information (E));
      Close_Log;
end Shell.Commands.Spawn_Server;
