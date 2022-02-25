package Shell.Commands.Unsafe
--
-- Any open pipes attached to a command will be automatically closed when the command goes out of scope.
--
is
   type Command       is new Commands.Command with private;
   type Command_Array is array (Positive range <>) of Command;

   Command_Error : exception;


   ---------
   --- Forge
   --
   package Forge
   is
      function To_Command  (Command_Line : in String) return Command;           -- An example 'Command_Line' is "ps -A".
      function To_Commands (Pipeline     : in String) return Command_Array;     -- An example 'Pipeline' is "ps -A | grep bash | wc".

      function "+"         (Command_Line : in String) return Command       renames To_Command;
      function "+"         (Pipeline     : in String) return Command_Array renames To_Commands;
   end Forge;


   overriding
   function  Image       (The_Command : in     Command) return String;

   procedure Connect     (From, To    : in out Command);           -- Connects 'From's output to 'To's input via a pipe.
   procedure Connect     (Commands    : in out Command_Array);     -- Connects each command in a pipeline.

   function   Input_Pipe (The_Command : in     Command) return Pipe;
   function  Output_Pipe (The_Command : in     Command) return Pipe;
   function   Error_Pipe (The_Command : in     Command) return Pipe;

   function  Process     (The_Command : in out Command) return access Shell.Process;


   ---------
   --- Start ~ Commands return before the process completes.
   --

   overriding
   procedure Start (The_Command : in out Command;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := False);

   procedure Start (Commands    : in out Command_Array;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := True);

   overriding
   procedure Send (To    : in Command;
                   Input : in Data);


   -------
   --- Run - Commands block until process completes.
   --

   overriding
   procedure Run (The_Command  : in out Command;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False);

   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False);

   function  Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) return Command_Results;

   function  Run (Command_Line : in     String;
                  Input        : in     Data  := No_Data) return Command_Results;
   --
   -- Takes a command line and runs a command or a pipeline, as appropriate.

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
   procedure Kill      (The_Command : in     Command);
   overriding
   procedure Interrupt (The_Command : in     Command);
   overriding
   procedure Pause     (The_Command : in out Command);
   overriding
   procedure Resume    (The_Command : in out Command);
   overriding
   function  Is_Paused (The_Command : in     Command) return Boolean;



private

   type Command is new Commands.Command with
      record
         Input_Pipe       : Pipe := Standard_Input;
         Output_Pipe      : Pipe := Null_Pipe;
         Error_Pipe       : Pipe := Null_Pipe;

         Owns_Output_Pipe : Boolean := False;
         Owns_Input_Pipe  : Boolean := False;

         Process          : aliased Shell.Process;

         Is_Within_A_Pipeline : Boolean := False;
      end record;

   overriding
   procedure Adjust   (The_Command : in out Command);
   overriding
   procedure Finalize (The_Command : in out Command);

   overriding
   procedure Gather_Results (The_Command : in out Command);
   procedure Stop           (The_Command : in out Command);


end Shell.Commands.Unsafe;
