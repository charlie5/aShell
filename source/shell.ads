with
     Ada.Strings.Unbounded;

private
with
     POSIX.IO,
     Posix.Process_Identification,
     POSIX.Process_Primitives;

package Shell
is

   -- Strings
   --
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in String) return Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "+" (Item : in Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   type String_Array is array (Positive range <>) of Unbounded_String;

   Nil_String : Unbounded_String
                renames Ada.Strings.Unbounded.Null_Unbounded_String;

   Nil_Strings : constant String_Array;


   -- Pipes
   --
   type Pipe is private;

   function  to_Pipe return Pipe;
   function  To_String (The_Pipe : in     Pipe) return String;     -- Returns available output from the 'read end' as a string.
   procedure Close     (The_Pipe : in     Pipe);

   Standard_Input  : constant Pipe;
   Standard_Output : constant Pipe;
   Standard_Error  : constant Pipe;


   -- Processes
   --
   type Process       is private;
   type Process_Array is array (Positive range <>) of Process;

   function Start (Program   : in     String;
                   Arguments : in     String_Array := Nil_Strings;
                   Input     : in     Pipe         := Standard_Input;                 --  We should probably distinguish
                   Output    : in     Pipe         := Standard_Output;                --  between the two ends of a pipe.
                   Errors    : in     Pipe         := Standard_Error) return Process;

   function Image (Process : in Shell.Process) return String;


   -- Commands
   --
   Max_Arguments : constant := 32;     -- Arbitrary.

   subtype Argument_Range is Natural        range 0 .. Max_Arguments;
   subtype Argument_Id    is Argument_Range range 1 .. Argument_Range'Last;

   type Command (Argument_Count : Argument_Range := 0)  is private;
   type Command_Array is array (Positive range <>) of Command;

   function to_Command  (Command_Line : in    String) return Command;           -- An example 'Command_Line' is "ps -A".
   function to_Commands (Pipeline     : in    String) return Command_Array;     -- An example 'Pipeline'     is "ps -A | grep bash | wc".


   procedure Connect (From, To : in out Command);           -- Connects 'From's standard output to 'To's standard input via a pipe.
   procedure Connect (Commands : in out Command_Array);     -- Connects each command in a pipeline.


   function  Run (The_Command : in     Command) return Process;
   procedure Run (The_Command : in     Command);

   procedure Run (Commands    : in out Command_Array;
                  Piped       : in     Boolean      := True);
   function  Run (Commands    : in out Command_Array;
                  Piped       : in     Boolean      := True) return Process_Array;

   function  Command_Output (The_Command : in     Command) return String;


private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;
   subtype File_Descriptor  is POSIX.IO.File_Descriptor;
   subtype Process_ID       is Posix.Process_Identification.Process_ID;

   Nil_Strings : constant String_Array := (1 .. 0 => <>);

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

   type Process is
      record
         Id : Process_ID;
      end record;


end Shell;
