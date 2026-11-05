package Shell.Commands.Safe
--
-- Allows commands to be safely run in different tasks.
--
-- Requires the 'ashell_spawn_server' binary to be installed
-- in a folder on the users PATH (such as /usr/bin).
--
is
   type Command       is new Commands.Command with private;
   type Command_Array is array (Positive range <>) of Command;


   package Forge
   is
      function To_Command  (Command_Line  : in String) return Command;           -- An example 'Command_Line' is "ps -A".
      function To_Commands (Pipeline      : in String) return Command_Array;     -- An example 'Pipeline' is "ps -A | grep bash | wc". Raises 'Command_Error' if a command is missing, such as after a final '|'.

      function "+"         (Command_Line  : in String) return Command       renames To_Command;
      function "+"         (Pipeline      : in String) return Command_Array renames To_Commands;
   end Forge;


   ---------
   --- Start ~ Commands return before the process completes.
   --

   overriding
   procedure Start (The_Command   : in out Command;
                    Input         : in     Data    := No_Data;
                    Accepts_Input : in     Boolean := False;
                    Pipeline      : in     Boolean := False);

   procedure Start (Commands    : in out Command_Array;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := True);

   overriding
   procedure Send  (To    : in Command;
                    Input : in Data);

   overriding
   procedure Close_Input (The_Command : in Command);

   -------
   --- Run - Commands block until process completes.
   --

   overriding
   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False);

   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False);

   function  Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) return Command_Results;
   --
   -- The results hold the output of the last command, and the errors of all the commands, in order.
   -- Each command's own 'Results_Of' gives only its own errors.

   function  Run (Command_Line : in     String;
                  Input        : in     Data  := No_Data) return Command_Results;
   --
   -- Takes a command line and runs a Command or a Pipeline, as appropriate.

   procedure Run (Command_Line : in     String;
                  Input        : in     Data  := No_Data);
   --
   -- Takes a command line (single or multiple piped commands).
   -- Wait for process completion and raise a Command_Error if any command fails.
   -- This includes a command ended by SIGPIPE, as in 'yes | head -1'.
   -- Any process error message is attached to the exception.



   function  Failed       (The_Pipeline : in Command_Array) return Boolean;
   function  Which_Failed (The_Pipeline : in Command_Array) return Natural;
   --
   -- Returns 0 if no command failed. When a command of the pipeline couldn't start, this briefly waits to hear
   -- which, and raises 'Command_Error' if the server is lost. Otherwise, each command before the one which failed
   -- must have ended, or this raises 'Process_Error'.


   overriding
   function  Status         (The_Command : in out Command) return State;

   overriding
   procedure Wait_On        (The_Command : in out Command);

   overriding
   function  Has_Terminated (The_Command : in out Command) return Boolean;

   overriding
   function  Normal_Exit    (The_Command : in     Command) return Boolean;


   overriding
   procedure Kill      (The_Command : in out Command);

   overriding
   procedure Interrupt (The_Command : in out Command);

   overriding
   procedure Pause     (The_Command : in out Command);

   overriding
   procedure Resume    (The_Command : in out Command);


   procedure Stop_Spawn_Client;
   --
   -- Shuts down the spawn client task and spawn server process. This occurs automatically when the main subprogram
   -- has completed, so only call this to shut down earlier.
   --
   -- Library level tasks cannot run commands after the main subprogram has completed. They would receive a
   -- Tasking_Error or Command_Error, whether or not this is called.
   --
   -- Paused commands are killed. Running commands are left to complete, and the program waits on them.



private

   -----------------------
   --- Safe Client Outputs
   --

   protected
   type Client_Outputs
   is
      procedure Add_Outputs (Output      : in     Shell.Data;
                             Errors      : in     Shell.Data);

      procedure Get_Outputs (Output      :    out Data_Vector;
                             Errors      :    out Data_Vector;
                             Normal_Exit :    out Boolean);

      -- Both the command and the spawn client use these outputs, and the command may be finalized
      -- before the server reports that it is done. The last of the two to finish deallocates them.
      --
      procedure Hold;                                            -- The spawn client now has access.

      procedure Set_Done    (Normal_Exit  : in     Boolean;
                             Not_At_Fault : in     Boolean;
                             Abandoned    :    out Boolean);     -- True when the spawn client should deallocate.

      procedure Abandon     (Unused      :    out Boolean);      -- True when the command should deallocate.

      procedure Set_Lost    (Abandoned   :    out Boolean);      -- The server has gone, so 'Set_Done' will never be called.

      entry     Wait_Til_Done;                                   -- Raises Command_Error, if lost.
      function  Is_Done     return Boolean;
      function  Normal_Exit return Boolean;
      function  Not_At_Fault return Boolean;                     -- It only ended as another command of its pipeline couldn't start.

   private
      All_Output : Data_Vector;
      All_Errors : Data_Vector;

      Exit_Is_Normal : Boolean := False;     -- 'Get_Outputs' reads this while the command is still running.
      Done           : Boolean := False;
      Is_Lost        : Boolean := False;

      Is_Not_At_Fault : Boolean := False;

      Is_Held        : Boolean := False;
      Is_Abandoned   : Boolean := False;
   end Client_Outputs;


   type Client_Outputs_Access is access all Client_Outputs;


   ----------------
   --- Safe Command
   --

   type Command is new Commands.Command with
      record
         Id           : Command_Id            := Null_Id;
         Safe_Outputs : Client_Outputs_Access := new Client_Outputs;
      end record;


   overriding
   procedure Gather_Results (The_Command : in out Command);

   overriding
   procedure Finalize (The_Command : in out Command);


end Shell.Commands.Safe;
