package Shell.Commands
--
-- Any open pipes attached to a command will be automatically closed when the command goes out of scope.
--
is
   Command_Error : exception;

   type Command       is tagged private;
   type Command_Array is array (Positive range <>) of Command;

   function To_Command  (Command_Line : in String;                   -- An example 'Command_Line' is "ps -A".
                         Input        : in Pipe  := Standard_Input;
                         Output       : in Pipe  := Standard_Output;
                         Errors       : in Pipe  := Standard_Error) return Command;

   function To_Commands (Pipeline     : in String) return Command_Array;  -- An example 'Pipeline'     is "ps -A | grep bash | wc".

   function "+"         (Command_Line : in String) return Command;
   function "+"         (Pipeline     : in String) return Command_Array renames To_Commands;

   procedure Connect (From, To : in out Command);        -- Connects 'From's output to 'To's input via a pipe.
   procedure Connect (Commands : in out Command_Array);  -- Connects each command in a pipeline.

   function   Input_Pipe (The_Command : in Command) return Pipe;
   function  Output_Pipe (The_Command : in Command) return Pipe;
   function   Error_Pipe (The_Command : in Command) return Pipe;

   function  Name_of    (The_Command : in out Command) return String;
   function  Process_of (The_Command : in out Command) return access Process;
   function  Failed     (The_Command : in     Command) return Boolean;


   --- The Start subprograms return before the process completes.
   --

   procedure Start (The_Command : in out Command;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := False);

   procedure Start (Commands    : in out Command_Array;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := True);


   --- Command Results
   --

   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is private;

   function  Results_Of (The_Command : in out Command) return Command_Results;

   function  Output_Of  (The_Results : in Command_Results) return Data;
   function  Errors_Of  (The_Results : in Command_Results) return Data;


   --- Run - Block until process completes.
   --

   procedure Run (The_Command  : in out Command;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False);

   function  Run (The_Command  : in out Command;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) return Command_Results;

   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False);

   function  Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) return Command_Results;


   function  Run (Command_Line : in     String;
                  Input        : in     Data := No_Data) return Command_Results;
   --
   -- Takes a command line and runs a Command or a Pipeline, as appropriate.

   procedure Run (Command_Line : in     String;
                  Input        : in     Data := No_Data);
   --
   -- Takes a command line (single or multiple piped commands).
   -- Wait for (final) process completion and raise a Command_Error on failure.
   -- Any process error message is attached to the exception.


   function  Failed       (The_Pipeline : in Command_Array) return Boolean;
   function  Which_Failed (The_Pipeline : in Command_Array) return Natural;
   --
   -- Returns 0 if no command failed.

   procedure Kill (The_Command : in Command);



private

   type Command is new Ada.Finalization.Controlled
     with
         record
            Name        : Unbounded_String;
            Arguments   : String_Vector;
            Input_Pipe  : Pipe := Standard_Input;
            Output_Pipe : Pipe := Standard_Output;
            Error_Pipe  : Pipe := Standard_Error;
            Process     : aliased Shell.Process;
         end record;

   overriding
   procedure Finalize (The_Command : in out Command);


   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is
      record
         Output  : Data (1 .. Output_Size);
         Errors  : Data (1 ..  Error_Size);
      end record;

end Shell.Commands;
