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

   procedure Run (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True);

   function  Run (The_Command   : in out Command;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True) return Command_Results;

   procedure Run (The_Pipeline  : in out Command_Array;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True);

   function  Run (The_Pipeline  : in out Command_Array;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True) return Command_Results;

   function  Run (Command_Line  : in     String;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True) return Command_Results;
   --
   -- Takes a command line and runs a Command or a Pipeline, as appropriate.

   procedure Run (Command_Line  : in     String;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True);
   --
   -- Takes a command line (single or multiple piped commands).
   -- Wait for (final) process completion and raise a Command_Error on failure.
   -- Any process error message is attached to the exception.


end Shell.Commands.Safe;
