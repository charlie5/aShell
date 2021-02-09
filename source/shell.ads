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

   -- Strings
   --
   type Unbounded_String is new Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in String) return Unbounded_String;
   function "+" (Item : in Unbounded_String) return String;

   type String_Array is array (Positive range <>) of Unbounded_String;

   Nil_String  : constant Unbounded_String;
   Nil_Strings : constant String_Array;


   --- Pipes
   --
   type Pipe is private;

   use Ada.Streams;

   function  To_Pipe return Pipe;
   function  Output_Of (The_Pipe : in Pipe) return Stream_Element_Array;   -- Returns available output from the 'read end' as a stream array.
   function  Output_Of (The_Pipe : in Pipe) return String;                 -- Returns available output from the 'read end' as a string.
   procedure Write_To  (The_Pipe : in Pipe;   Input : in Stream_Element_Array);
   procedure Write_To  (The_Pipe : in Pipe;   Input : in String);
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

   function Start (Program           : in String;
                   Arguments         : in String_Array := Nil_Strings;
                   Working_Directory : in String       := ".";
                   Input             : in Pipe         := Standard_Input;
                   Output            : in Pipe         := Standard_Output;
                   Errors            : in Pipe         := Standard_Error;
                   Pipeline          : in Boolean      := False) return Process;
   --
   -- With a pipeline, closing the write ends of the 'Output' & 'Errors' pipes becomes the callers responsibility.

   function Start (Command           : in String;
                   Working_Directory : in String       := ".";
                   Input             : in Pipe         := Standard_Input;
                   Output            : in Pipe         := Standard_Output;
                   Errors            : in Pipe         := Standard_Error;
                   Pipeline          : in Boolean      := False) return Process;
   --
   -- With a pipeline, closing the write ends of the 'Output' & 'Errors' pipes becomes the callers responsibility.

   procedure Wait_On        (Process : in Shell.Process);
   function  Has_Terminated (Process : in Shell.Process) return Boolean;
   function  Normal_Exit    (Process : in Shell.Process) return Boolean;
   --
   -- Returns True if the process has terminated and the exit status is normal.

   function Image (Process : in Shell.Process) return String;


   --- Commands
   --
   type Command       is tagged limited private;
   type Command_Array is array (Positive range <>) of Command;

   function To_Command  (Command_Line : in String) return Command;        -- An example 'Command_Line' is "ps -A".
   function To_Commands (Pipeline     : in String) return Command_Array;  -- An example 'Pipeline'     is "ps -A | grep bash | wc".


   procedure Connect (From, To : in out Command);        -- Connects 'From's standard output to 'To's standard input via a pipe.
   procedure Connect (Commands : in out Command_Array);  -- Connects each command in a pipeline.


   function  Run (The_Command : in out Command;
                  Input       : in     String  := "";
                  Pipeline    : in     Boolean := False) return Process;

   procedure Run (The_Command : in out Command;
                  Input       : in     String  := "";
                  Pipeline    : in     Boolean := False);

   procedure Run (Commands    : in out Command_Array;
                  Input       : in     String  := "";
                  Pipeline    : in     Boolean      := True);

   function  Run (Commands    : in out Command_Array;
                  Input       : in     String  := "";
                  Pipeline    : in     Boolean      := True) return Process_Array;

   function  Run (The_Command : in out Command;
                  Input       : in     Stream_Element_Array;
                  Pipeline    : in     Boolean := False) return Process;

   procedure Run (The_Command : in out Command;
                  Input       : in     Stream_Element_Array;
                  Pipeline    : in     Boolean := False);

   procedure Run (Commands    : in out Command_Array;
                  Input       : in     Stream_Element_Array;
                  Pipeline    : in     Boolean      := True);

   function  Run (Commands    : in out Command_Array;
                  Input       : in     Stream_Element_Array;
                  Pipeline    : in     Boolean      := True) return Process_Array;


   function  Command_Output  (The_Command  : in out Command;
                              Input        : in     String := "") return String;

   function  Command_Output  (The_Command  : in out Command;
                              Input        : in     String := "") return Stream_Element_Array;

   function  Pipeline_Output (The_Commands : in out Command_Array;
                              Input        : in     String := "") return String;

   function  Pipeline_Output (The_Commands : in out Command_Array;
                              Input        : in     String := "") return Stream_Element_Array;

   function  Output_Of       (Command_Line : in     String;
                              Input        : in     String := "") return String;
   --
   -- Takes a command line and calls Command_Output or Pipeline_Output, as appropriate.

   function  Output_Of       (Command_Line : in     String;
                              Input        : in     String := "") return Stream_Element_Array;
   --
   -- Takes a command line and calls Command_Output or Pipeline_Output, as appropriate.

   function  Command_Output  (The_Command  : in out Command;
                              Input        : in     Stream_Element_Array) return String;

   function  Command_Output  (The_Command  : in out Command;
                              Input        : in     Stream_Element_Array) return Stream_Element_Array;

   function  Pipeline_Output (The_Commands : in out Command_Array;
                              Input        : in     Stream_Element_Array) return String;

   function  Pipeline_Output (The_Commands : in out Command_Array;
                              Input        : in     Stream_Element_Array) return Stream_Element_Array;

   function  Output_Of       (Command_Line : in     String;
                              Input        : in     Stream_Element_Array) return String;
   --
   -- Takes a command line and calls Command_Output or Pipeline_Output, as appropriate.

   function  Output_Of       (Command_Line : in     String;
                              Input        : in     Stream_Element_Array) return Stream_Element_Array;
   --
   -- Takes a command line and calls Command_Output or Pipeline_Output, as appropriate.

   procedure Run (Command_Line : in String;
                  Input        : in String := "");
   --
   -- Runs a command line and raises Command_Error on failure.

   procedure Run (Command_Line : in String;
                  Input        : in Stream_Element_Array);
   --
   -- Runs a command line and raises Command_Error on failure.

   Command_Error : exception;


   -- Command Results
   --

   type Command_Results is limited private;

   function  Results_Of (The_Command : in out Command) return Command_Results;
   --
   -- Runs the command and returns the results.

   function  Output_Of  (The_Results : in Command_Results) return String;
   function  Errors_Of  (The_Results : in Command_Results) return String;


private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;
   subtype File_Descriptor  is POSIX.IO.File_Descriptor;
   subtype Process_ID       is POSIX.Process_Identification.Process_ID;

   Nil_String  : constant Unbounded_String := Unbounded_String (Ada.Strings.Unbounded.Null_Unbounded_String);
   Nil_Strings : constant String_Array     := (1 .. 0 => <>);

   Max_Commands_In_Pipeline : constant := 50;     -- Arbitrary.

   package String_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_String);
   subtype String_Vector  is String_Vectors.Vector;

   type Command is limited new Ada.Finalization.Limited_Controlled
     with
         record
            Name        : Unbounded_String;
            Arguments   : String_Vector;
            Input_Pipe  : Pipe := Standard_Input;
            Output_Pipe : Pipe := Standard_Output;
            Error_Pipe  : Pipe := Standard_Error;
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
         Id : Process_ID;
      end record;

   type Command_Results is
      record
         Output : Unbounded_String;
         Errors : Unbounded_String;
      end record;

end Shell;
