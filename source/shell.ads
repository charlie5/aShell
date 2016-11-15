with
     Ada.Strings.Unbounded;

private
with
     POSIX.IO,
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



   -- Commands
   --
   type Command       is private;
   type Command_Array is array (Positive range <>) of Command;

   function to_Command  (Command_Line : in    String) return Command;           -- An example 'Command_Line' is "ps -A".
   function to_Commands (Pipeline     : in    String) return Command_Array;     -- An example 'Pipeline'     is "ps -A | grep bash | wc".


   procedure Connect (From, To    : in out Command);     -- Connects 'From's standard output to 'To's standard input via a pipe.
   procedure Run     (The_Command : in     Command);
   procedure Run     (Commands    : in out Command_Array;
                      Piped       : in     Boolean      := True);



   -- Pipes
   --
   type Pipe is private;

   function  to_Pipe return Pipe;
   procedure Close (The_Pipe : in out Pipe);


   -- Processes
   --
--   type Process (<>)  is private;
   type Process is private;

   function Start (Program   : in     String;
                   Arguments : in     String_Array;
                   Input     : in     Pipe;                   --  We should probably distinguish
                   Output    : in     Pipe;                   --  between the two ends of a pipe.
                   Errors    : in     Pipe) return Process;



private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;

   Max_Commands_In_Pipeline : constant := 50;     -- Arbitrary.
   Max_Arguments            : constant := 32;     -- Arbitrary.

   subtype Argument_Range is Natural        range 0 .. Max_Arguments;
   subtype Argument_Id    is Argument_Range range 1 .. Argument_Range'Last;


   type Command is
      record
         Name      : Unbounded_String;
         Arguments : Unbounded_String;

         Input_Pipe,
         Output_Pipe : Pipe;

         Template    : access Process_Template;
      end record;


   Null_File_Descriptor : constant POSIX.IO.File_Descriptor := POSIX.IO.File_Descriptor'Last;     -- TODO: How best to define a null file descriptor ?

   type Pipe is
      record
         Write_End,
         Read_End : POSIX.IO.File_Descriptor := Null_File_Descriptor;
      end record;


--     type Process (Valid : Boolean) is
   type Process is
      record
         null;
      end record;


end Shell;
