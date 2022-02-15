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



   --- The Start subprograms return before the process completes.
   --

   overriding
   procedure Start (The_Command : in out Command;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := False);

   procedure Start (Commands    : in out Command_Array;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := True);


   --- Run - Block until process completes.
   --

   overriding
   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False);

   overriding
   function  Run (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False) return Command_Results;


   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False);

   function  Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) return Command_Results;


   overriding
   procedure Wait_On        (The_Command : in out Command);

   overriding
   function  Has_Terminated (The_Command : in out Command) return Boolean;

   overriding
   function  Normal_Exit    (The_Command : in     Command) return Boolean;


   overriding
   procedure Kill      (The_Command : in     Command);

   overriding
   procedure Interrupt (The_Command : in     Command);

   overriding
   procedure Pause     (The_Command : in out Command);

   overriding
   procedure Resume    (The_Command : in out Command);


   procedure Stop_Spawn_Client;
   --
   -- Called at program completion to halt the spawn client task and spawn server process.



private

   -----------------------
   --- Safe_Client_Outputs
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

      entry     Wait_Til_Done;
      function  Is_Done     return Boolean;
      function  Normal_Exit return Boolean;

   private
      All_Output : Data_Vector;
      All_Errors : Data_Vector;

      Exit_Is_Normal : Boolean;
      Done           : Boolean := False;
   end Safe_Client_Outputs;



   type Safe_Client_Outputs_Access is access all Safe_Client_Outputs;


   ----------------
   --- Safe Command
   --

   type Command is new Commands.Command with
      record
         Id           : Command_Id                 := Null_Id;
         Safe_Outputs : Safe_Client_Outputs_Access := new Safe_Client_Outputs;
      end record;


   overriding
   procedure Finalize (The_Command : in out Command);

end Shell.Commands.Safe;
