package Shell.Commands.Safe
--
-- Commands run simultaneously in different tasks will sometimes (rarely) fail. The 'Expect_Output'
-- and 'Retry' parameters are present to help manage this issue. The defaults are usually sufficient.
--
-- If a command or pipeline fail, it will repeat up to 'Retry' times.
-- Set 'Expect_Output' to false if a command or pipeline may produce no output.

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


end Shell.Commands.Safe;
