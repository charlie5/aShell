with
     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Hashed_Maps,
     Ada.IO_Exceptions,
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


      procedure Get (Action :    out Server_Action)
      is
      begin
         if not Actions.Is_Empty
         then
            Action := Actions.Last_Element;
            Actions.Delete_Last;
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
      package Id_Maps_of_Command is new Ada.Containers.Hashed_Maps (Key_Type        => Command_Id,
                                                                    Element_Type    => Command,
                                                                    Hash            => Hash,
                                                                    Equivalent_Keys =>  "=");
      Command_Map   : Id_Maps_of_Command.Map;

      Output_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Output);
      Errors_Stream : aliased Pipe_Stream := Stream (Shell.Standard_Error);

      Stopping : Boolean := False;
   begin
      log ("Starting Spawn Manager");

      New_Action_Fetcher.Start;

      loop
         log ("");
         log ("looping");

         begin
            declare
               Action : Server_Action;
            begin
               New_Actions.Get (Action);

               case Action.Kind
               is
                  when Nil =>
                     Log ("Nil action.");
                     null;

                  when New_Command =>
                     Log ("New_Command action.");
                     declare
                        The_Command : Command := Forge.To_Command (+Action.Command_Line);
                     begin
                        The_Command.Owns_Output_Pipe := True;

                        The_Command.Output_Pipe := To_Pipe (Blocking => False);
                        The_Command. Error_Pipe := To_Pipe (Blocking => False);

                        The_Command.Start;
                        Log ("Command => '" & Image (The_Command) & "'");

                        Command_Map.Insert (Action.Id, The_Command);
                     end;

                  when Stop =>
                     Log ("Stop action.");
                     Stopping := True;
               end case;
            end;

            declare
               use Id_Maps_of_Command;
               package Command_Id_Vectors is new Ada.Containers.Vectors (Positive, Command_Id);

               Done_Comands : Command_Id_Vectors.Vector;
               Cursor       : Id_Maps_of_Command.Cursor := Command_Map.First;
            begin
               -- Send ongoing command results to the client.
               --
               while Has_Element (Cursor)
               loop
                  declare
                     use Data_Holders;

                     Id          : constant Command_Id := Key     (Cursor);
                     The_Command :          Command    := Element (Cursor);

                     Results     : constant Command_Results := Results_Of (The_Command);

                     Output      : constant Data := Output_Of (Results);
                     Errors      : constant Data := Errors_Of (Results);
                  begin
                     Client_Action'Output (Output_Stream'Access,
                                           (New_Outputs,
                                            Id,
                                            To_Holder (Output),
                                            To_Holder (Errors)));

                     if The_Command.Has_Terminated
                     then
                        log ("Has_Terminated");

                        declare
                           Act : constant Client_Action := (Command_Done, Id);
                        begin
                           Client_Action'Output (Output_Stream'Access, Act);
                        end;

                        Done_Comands.Append (Id);
                     end if;
                  end;

                  Next (Cursor);
               end loop;

               -- Rid completed commands.
               --
               for Each of Done_Comands
               loop
                  Command_Map.Delete (Each);
               end loop;
            end;

            exit when Stopping
                  and Command_Map.Is_Empty;

            if Command_Map.Is_Empty
            then
               delay 0.1;
            end if;

         end;
      end loop;

      log ("Spawn Server: Done");
   end;

   Close_Log;

exception
   when E : others =>
      Log ("Unhandled error in aShell_Spawn_Server.");
      Log (Ada.Exceptions.Exception_Information (E));
      Close_Log;
end Shell.Commands.Spawn_Server;
