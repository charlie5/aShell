package Shell.Commands.Safe
--
-- Commands run simultaneously in different tasks will sometimes (rarely) fail. The 'Expect_Output'
-- and 'Retry' parameters are present to help manage this issue.
--
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
   --
   -- If the pipeline fails, it will repeat up to 'Retry' times.

   function  Run (The_Pipeline  : in out Command_Array;
                  Input         : in     Data    := No_Data;
                  Raise_Error   : in     Boolean := False;
                  Retry         : in     Natural := Natural'Last;
                  Expect_Output : in     Boolean := True) return Command_Results;

   function  Run (Command_Line  : in     String;
                  Input         : in     Data  := No_Data) return Command_Results;
   --
   -- Takes a command line and runs a Command or a Pipeline, as appropriate.

   procedure Run (Command_Line  : in     String;
                  Input         : in     Data  := No_Data);
   --
   -- Takes a command line (single or multiple piped commands).
   -- Wait for (final) process completion and raise a Command_Error on failure.
   -- Any process error message is attached to the exception.


end Shell.Commands.Safe;
