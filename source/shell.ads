with
     Ada.Strings.Unbounded,
     Ada.Streams;

private
with
     POSIX.IO,
     POSIX.Process_Identification,
     POSIX.Process_Primitives,
     Ada.Containers.Vectors;

package Shell
--
-- Provides processes and pipes.
--
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

   Nil_Strings : constant String_Array;


   -- Conversion

   function To_String (From : in Data)   return String;
   function To_Data   (From : in String) return Data;

   function "+"       (From : in Data)   return String renames To_String;
   function "+"       (From : in String) return Data   renames To_Data;


   --- Pipes
   --
   type Pipe is private;

   function  To_Pipe (Blocking : in Boolean := True) return Pipe;

   function  Image   (Pipe : in Shell.Pipe) return String;

   function  Is_Readable  (Pipe    : in Shell.Pipe) return Boolean;
   function  Is_Writeable (Pipe    : in Shell.Pipe) return Boolean;
   function  Is_Empty     (Pipe    : in Shell.Pipe;
                           Timeout : in Duration := 0.0) return Boolean;   -- A timeout of '0.0' will block until the pipe is not empty.

   No_Output_Error   : exception;
   Pipe_Not_Readable : exception;

   function  Output_Of (Pipe : in Shell.Pipe)       return Data;   -- Returns available output from the 'read end'.
   procedure Write_To  (Pipe : in Shell.Pipe;   Input : in Data);

   procedure Close (Pipe           : in Shell.Pipe;
                    Only_Write_End : in Boolean := False;
                    Only_Read_End  : in Boolean := False);

   procedure Close_Write_End (Pipe : in Shell.Pipe);
   function  Close_Write_End (Pipe : in Shell.Pipe) return Boolean;

   Standard_Input  : constant Pipe;
   Standard_Output : constant Pipe;
   Standard_Error  : constant Pipe;

   type Pipe_Stream is new Ada.Streams.Root_Stream_Type with private;
   function  Stream (Pipe : in Shell.Pipe) return Pipe_Stream;


   --- Processes
   --

   type Process       is private;
   type Process_Array is array (Positive range <>) of Process;

   -- For 'Start', when pipeline is true, closing the write ends of any
   -- non-standard 'Output' and 'Errors' pipes becomes the callers responsibility.

   function Start (Program           : in String;
                   Arguments         : in String_Array := Nil_Strings;
                   Working_Directory : in String  := ".";
                   Input             : in Pipe    := Standard_Input;
                   Output            : in Pipe    := Standard_Output;
                   Errors            : in Pipe    := Standard_Error;
                   Pipeline          : in Boolean := False) return Process;

   function Start (Command           : in String;
                   Working_Directory : in String  := ".";
                   Input             : in Pipe    := Standard_Input;
                   Output            : in Pipe    := Standard_Output;
                   Errors            : in Pipe    := Standard_Error;
                   Pipeline          : in Boolean := False) return Process;

   procedure Wait_On        (Process : in out Shell.Process);
   function  Has_Terminated (Process : in out Shell.Process) return Boolean;
   function  Normal_Exit    (Process : in     Shell.Process) return Boolean;

   function  Image     (Process : in Shell.Process) return String;

   procedure Kill      (Process : in Shell.Process);
   procedure Interrupt (Process : in Shell.Process);
   procedure Pause     (Process : in Shell.Process);
   procedure Resume    (Process : in Shell.Process);


   --- Debugging
   --

   procedure Open_Log (Name : in String);
   procedure Close_Log;



private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;
   subtype File_Descriptor  is POSIX.IO.File_Descriptor;
   subtype Process_ID       is POSIX.Process_Identification.Process_ID;

   Null_File_Descriptor : constant File_Descriptor := File_Descriptor'Last;

   No_Data     : constant Data (1 .. 0) := (others => <>);
   Nil_Strings : constant String_Array  := (1 .. 0 => <>);

   package String_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_String);
   subtype String_Vector  is String_Vectors.Vector;


   --- Pipes
   --

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

   protected Safe_Pipes
   is
      procedure Open  (Pipe           : out Shell.Pipe);
      procedure Close (Pipe           : in  Shell.Pipe;
                       Only_Write_End : in  Boolean := False;
                       Only_Read_End  : in  Boolean := False);
   end Safe_Pipes;


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

   --- Processes
   --

   type Process is
      record
         Id     : Process_ID := POSIX.Process_Identification.Null_Process_ID;
         Status : POSIX.Process_Primitives.Termination_Status;
      end record;


   --- Debugging
   --

   procedure Log (Message : in String);
   function  Log (Message : in String) return Boolean;     -- Allow for logging in a declarative region.

end Shell;
