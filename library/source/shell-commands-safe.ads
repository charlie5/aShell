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
      function To_Commands (Pipeline      : in String) return Command_Array;     -- An example 'Pipeline' is "ps -A | grep bash | wc".

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

   function  Run (Command_Line : in     String;
                  Input        : in     Data  := No_Data) return Command_Results;
   --
   -- Takes a command line and runs a Command or a Pipeline, as appropriate.

   procedure Run (Command_Line : in     String;
                  Input        : in     Data  := No_Data);
   --
   -- Takes a command line (single or multiple piped commands).
   -- Wait for (final) process completion and raise a Command_Error on failure.
   -- Any process error message is attached to the exception.



   function  Failed       (The_Pipeline : in Command_Array) return Boolean;
   function  Which_Failed (The_Pipeline : in Command_Array) return Natural;
   --
   -- Returns 0 if no command failed.


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
   -- Called at program completion to shutdown the spawn client task and spawn server process.



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

      procedure Set_Done    (Normal_Exit : in     Boolean);

      entry     Wait_Til_Done;
      function  Is_Done     return Boolean;
      function  Normal_Exit return Boolean;

   private
      All_Output : Data_Vector;
      All_Errors : Data_Vector;

      Exit_Is_Normal : Boolean;
      Done           : Boolean := False;
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
