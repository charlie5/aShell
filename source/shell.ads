with
     Ada.Strings.Unbounded,
     Ada.Streams;

private
with
     POSIX.IO,
     POSIX.Process_Identification,
     POSIX.Process_Primitives,
     Ada.Containers.Vectors,
     Ada.Finalization;

package Shell
is
   --- Data
   --
   use Ada.Streams;

   subtype Data        is Stream_Element_Array;
   subtype Data_Offset is Stream_Element_Offset;

   No_Data : constant Data;


   --- Strings
   --
   type Unbounded_String is new Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in String)           return Unbounded_String;
   function "+" (Item : in Unbounded_String) return String;

   type String_Array is array (Positive range <>) of Unbounded_String;

   Nil_String  : constant Unbounded_String;
   Nil_Strings : constant String_Array;


   -- Conversion

   function To_String (From : in Data)   return String;
   function To_Stream (From : in String) return Data;

   function "+"       (From : in Data)   return String renames To_String;
   function "+"       (From : in String) return Data   renames To_Stream;


   --- Pipes
   --
   type Pipe is private;

   function  To_Pipe return Pipe;

   function  Output_Of (The_Pipe : in Pipe)       return Data;   -- Returns available output from the 'read end'.
   procedure Write_To  (The_Pipe : in Pipe;   Input : in Data);

   procedure Close     (The_Pipe : in Pipe);

   procedure Close_Write_End (The_Pipe : in Pipe);
   function  Close_Write_End (The_Pipe : in Pipe) return Boolean;

   Standard_Input  : constant Pipe;
   Standard_Output : constant Pipe;
   Standard_Error  : constant Pipe;

   type Pipe_Stream is new Ada.Streams.Root_Stream_Type with private;


   --- Processes
   --

   type Process       is private;
   type Process_Array is array (Positive range <>) of Process;

   -- For 'Start', when pipeline is true, closing the write ends of any
   -- non-standard 'Output' and 'Errors' pipes becomes the callers responsibility.

   function Start (Program           : in String;
                   Arguments         : in String_Array := Nil_Strings;
                   Working_Directory : in String       := ".";
                   Input             : in Pipe         := Standard_Input;
                   Output            : in Pipe         := Standard_Output;
                   Errors            : in Pipe         := Standard_Error;
                   Pipeline          : in Boolean      := False) return Process;

   function Start (Command           : in String;
                   Working_Directory : in String       := ".";
                   Input             : in Pipe         := Standard_Input;
                   Output            : in Pipe         := Standard_Output;
                   Errors            : in Pipe         := Standard_Error;
                   Pipeline          : in Boolean      := False) return Process;

   procedure Wait_On        (Process : in out Shell.Process);
   function  Has_Terminated (Process : in out Shell.Process) return Boolean;
   function  Wait_On_Normal_Exit    (Process : in out Shell.Process) return Boolean;
   --
   -- Returns True if the process has terminated and the exit status is normal.

   function  Normal_Exit (Process : in Shell.Process) return Boolean;

   function Image (Process : in Shell.Process) return String;


   --- Commands
   --
   Command_Error : exception;


   -- Any open pipes attached to a command will be automatically closed when the command goes out of scope.
   --

   type Command       is tagged private;
   type Command_Array is array (Positive range <>) of Command;

   function To_Command  (Command_Line : in String) return Command;        -- An example 'Command_Line' is "ps -A".
   function To_Commands (Pipeline     : in String) return Command_Array;  -- An example 'Pipeline'     is "ps -A | grep bash | wc".

   function "+"         (Command_Line : in String) return Command       renames To_Command;
   function "+"         (Pipeline     : in String) return Command_Array renames To_Commands;

   procedure Connect (From, To : in out Command);        -- Connects 'From's output to 'To's input via a pipe.
   procedure Connect (Commands : in out Command_Array);  -- Connects each command in a pipeline.

   function   Input_Pipe (The_Command : in Command) return Pipe;
   function  Output_Pipe (The_Command : in Command) return Pipe;
   function   Error_Pipe (The_Command : in Command) return Pipe;

   function  Name_of    (The_Command : in out Command) return String;
   function  Process_of (The_Command : in out Command) return access Process;


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

private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;
   subtype File_Descriptor  is POSIX.IO.File_Descriptor;
   subtype Process_ID       is POSIX.Process_Identification.Process_ID;

   No_Data : constant Data (1 .. 0) := (others => <>);

   Nil_String  : constant Unbounded_String := Unbounded_String (Ada.Strings.Unbounded.Null_Unbounded_String);
   Nil_Strings : constant String_Array     := (1 .. 0 => <>);

   Max_Commands_In_Pipeline : constant := 50;     -- Arbitrary.

   package String_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_String);
   subtype String_Vector  is String_Vectors.Vector;

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

   Null_File_Descriptor : constant File_Descriptor := File_Descriptor'Last;     -- TODO: Better way to define a null file descriptor ?

   type Pipe is
      record
         Write_End,
         Read_End : File_Descriptor := Null_File_Descriptor;
      end record;

   Standard_Input  : constant Pipe := (Write_End => Null_File_Descriptor,
                                       Read_End  => POSIX.IO.Standard_Input);

   Standard_Output : constant Pipe := (Write_End => POSIX.IO.Standard_Output,
                                       Read_End  => Null_File_Descriptor);

   Standard_Error  : constant Pipe := (Write_End => POSIX.IO.Standard_Error,
                                       Read_End  => Null_File_Descriptor);


   type Pipe_Stream is new Ada.Streams.Root_Stream_Type with
      record
         Pipe : Shell.Pipe;
      end record;

   overriding
   procedure Read  (Stream : in out Pipe_Stream;
                    Item   :    out Stream_Element_Array;
                    Last   :    out Stream_Element_Offset);
   overriding
   procedure Write (Stream : in out Pipe_Stream;
                    Item   : in     Stream_Element_Array);

   type Process is
      record
         Id     : Process_ID := POSIX.Process_Identification.Null_Process_ID;
         Status : POSIX.Process_Primitives.Termination_Status;
      end record;

   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is
      record
         Output  : Data (1 .. Output_Size);
         Errors  : Data (1 ..  Error_Size);
      end record;

end Shell;
