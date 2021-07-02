package Shell.Commands.Safe
--
-- Allows commands to be safely run in different tasks.
--
is
   --- Run - Block until process completes.
   --

   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False);

   function  Run (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False) return Command_Results;


   procedure Stop_Spawn_Client;
   --
   -- Called at program completion to halt the spawn client task and spawn server process.


end Shell.Commands.Safe;
