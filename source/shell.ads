with
     Ada.Strings.Unbounded,
     Ada.Streams;

private
with
     POSIX.IO,
     POSIX.Process_Identification,
     POSIX.Process_Primitives,
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


   -- Pipes
   --
   type Pipe is private;

   function  To_Pipe return Pipe;
   function  To_String (The_Pipe : in     Pipe) return String;     -- Returns available output from the 'read end' as a string.
   procedure Close     (The_Pipe : in     Pipe);

   procedure Close_Write_End (The_Pipe : in     Pipe);
   function  Close_Write_End (The_Pipe : in     Pipe) return Boolean;

   Standard_Input  : constant Pipe;
   Standard_Output : constant Pipe;
   Standard_Error  : constant Pipe;


   type Pipe_Stream is new Ada.Streams.Root_Stream_Type with private;


   -- Processes
   --
   type Process       is private;
   type Process_Array is array (Positive range <>) of Process;

   function Start (Program           : in     String;
                   Arguments         : in     String_Array := Nil_Strings;
                   Working_Directory : in     String       := ".";
                   Input             : in     Pipe         := Standard_Input;
                   Output            : in     Pipe         := Standard_Output;
                   Errors            : in     Pipe         := Standard_Error;
                   Pipeline          : in     Boolean      := False) return Process;
   --
   -- When in a pipeline, closing the write ends of the 'Output' & 'Errors' pipes becomes the callers responsibility.

   function Start (Command           : in     String;
                   Working_Directory : in     String       := ".";
                   Input             : in     Pipe         := Standard_Input;
                   Output            : in     Pipe         := Standard_Output;
                   Errors            : in     Pipe         := Standard_Error;
                   Pipeline          : in     Boolean      := False) return Process;
   --
   -- When in a pipeline, closing the write ends of the 'Output' & 'Errors' pipes becomes the callers responsibility.

   procedure Wait_On        (Process : in     Shell.Process);
   function  Has_Terminated (Process : in     Shell.Process) return Boolean;
   function  Normal_Exit    (Process : in     Shell.Process) return Boolean;
   --
   -- Returns True if the process has terminated and the exit status is normal.

   function Image (Process : in Shell.Process) return String;


   -- Commands
   --
   Max_Arguments : constant := 32;     -- Arbitrary.

   subtype Argument_Range is Natural        range 0 .. Max_Arguments;
   subtype Argument_Id    is Argument_Range range 1 .. Argument_Range'Last;

   type Command (Argument_Count : Argument_Range := 0)  is private;
   type Command_Array is array (Positive range <>) of Command;

   function To_Command  (Command_Line : in    String) return Command;           -- An example 'Command_Line' is "ps -A".
   function To_Commands (Pipeline     : in    String) return Command_Array;     -- An example 'Pipeline'     is "ps -A | grep bash | wc".


   procedure Connect (From, To : in out Command);           -- Connects 'From's standard output to 'To's standard input via a pipe.
   procedure Connect (Commands : in out Command_Array);     -- Connects each command in a pipeline.


   function  Run (The_Command : in     Command;
                  Pipeline    : in     Boolean := False) return Process;
   procedure Run (The_Command : in     Command;
                  Pipeline    : in     Boolean := False);

   procedure Run (Commands    : in out Command_Array;
                  Pipeline    : in     Boolean      := True);
   function  Run (Commands    : in out Command_Array;
                  Pipeline    : in     Boolean      := True) return Process_Array;


   function  Command_Output (The_Command : in     Command) return String;


   type Command_Results is limited private;

   function  Results_Of (The_Command : in     Command) return Command_Results;

   function  Output_Of  (The_Results : in     Command_Results) return String;
   function  Errors_Of  (The_Results : in     Command_Results) return String;


private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;
   subtype File_Descriptor  is POSIX.IO.File_Descriptor;
   subtype Process_ID       is POSIX.Process_Identification.Process_ID;

   Nil_String  : constant Unbounded_String := Unbounded_String (Ada.Strings.Unbounded.Null_Unbounded_String);
   Nil_Strings : constant String_Array     := (1 .. 0 => <>);

   Max_Commands_In_Pipeline : constant := 50;     -- Arbitrary.


   type Command (Argument_Count : Argument_Range := 0) is
      record
         Name        : Unbounded_String;
         Arguments   : String_Array (1 .. Argument_Count);

         Input_Pipe  : Pipe := Standard_Input;
         Output_Pipe : Pipe := Standard_Output;
         Error_Pipe  : Pipe := Standard_Error;
      end record;


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

   use Ada.Streams;

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


   type String_Access is access all String;

   type Command_Results is new Ada.Finalization.Limited_Controlled with
      record
         Output : String_Access;
         Errors : String_Access;
      end record;

   overriding
   procedure Finalize (Results : in out Command_Results);

end Shell;
