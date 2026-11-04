pragma Ada_2022;     -- Allows client code to be compiled under any Ada version.

with
     Ada.Strings.Unbounded,
     Ada.Streams;

private
with
     POSIX.IO,
     POSIX.Process_Identification,
     POSIX.Process_Primitives,
     Ada.Containers.Vectors,
     Ada.Containers.Ordered_Maps;


package Shell
--
-- Provides processes and pipes.
--
is
   --------
   --- Data
   --
   use Ada.Streams;

   subtype Data        is Stream_Element_Array;
   subtype Data_Offset is Stream_Element_Offset;

   No_Data : constant Data;


   -----------
   --- Strings
   --
   type Unbounded_String is new Ada.Strings.Unbounded.Unbounded_String;

   function "+" (Item : in String)           return Unbounded_String;
   function "+" (Item : in Unbounded_String) return String;

   type String_Array is array (Positive range <>) of Unbounded_String;

   Nil_Strings : constant String_Array;


   -------------
   -- Conversion
   --
   function To_String (From : in Data)   return String;
   function To_Data   (From : in String) return Data;

   function "+"       (From : in Data)   return String renames To_String;
   function "+"       (From : in String) return Data   renames To_Data;


   ---------
   --- Pipes
   --
   type Pipe is private;

   Too_Many_Pipes_Error : exception;
   Null_Pipe_Error      : exception;

   function  To_Pipe (Blocking : in Boolean := True) return Pipe;

   function  Image   (Pipe : in Shell.Pipe) return String;

   function  Is_Readable  (Pipe    : in Shell.Pipe) return Boolean;
   function  Is_Writeable (Pipe    : in Shell.Pipe) return Boolean;
   function  Is_Empty     (Pipe    : in Shell.Pipe;
                           Timeout : in Duration := 0.0) return Boolean;   -- A timeout of '0.0' will block until the pipe is not empty.
   --
   -- Is_Empty raises Pipe_Not_Readable when the 'read end' is closed.

   No_Output_Error   : exception;
   Pipe_Not_Readable : exception;

   procedure Write_To  (Pipe : in Shell.Pipe;   Input : in Data);
   function  Output_Of (Pipe : in Shell.Pipe)       return Data;   -- Returns available output from the 'read end'.
   --
   -- Write_To raises Pipe_Error when the 'write end' is closed.


   Pipe_Error : exception;

   procedure Close (Pipe           : in Shell.Pipe;
                    Only_Write_End : in Boolean := False;
                    Only_Read_End  : in Boolean := False);
   --
   -- Only_Write_End and Only_Read_End are mutually exclusive.
   -- Pipe_Error is raised when both are set to True.
   --
   -- A pipe is a value which may be copied. Closing an end closes it for all copies. Closing a closed end, or
   -- a standard pipe, does nothing. So does 'Close_Write_End'.

   procedure Close_Write_End (Pipe : in Shell.Pipe);
   function  Close_Write_End (Pipe : in Shell.Pipe) return Boolean;

   Standard_Input  : constant Pipe;
   Standard_Output : constant Pipe;
   Standard_Error  : constant Pipe;
   Null_Pipe       : constant Pipe;

   type Pipe_Stream is new Ada.Streams.Root_Stream_Type with private;
   function  Stream (Pipe : in Shell.Pipe) return Pipe_Stream;


   -------------
   --- Processes
   --

   type Process       is private;
   type Process_Array is array (Positive range <>) of Process;

   type Process_State is (Not_Started,
                          Running,
                          Paused,
                          Normal_Exit,
                          Failed_Exit,
                          Interrupted,
                          Killed);

   subtype  Terminated is Process_State range Normal_Exit .. Killed;

   function Status (Process : in out Shell.Process) return Process_State;


   -- For 'Start', when pipeline is true, closing the write ends of any
   -- non-standard 'Output' and 'Errors' pipes becomes the callers responsibility.
   -- A 'Process_Error' is raised if 'Start' fails, including when either end of
   -- a non-standard 'Input', 'Output' or 'Errors' pipe is not open.

   Process_Error            : exception;
   Process_Already_Started  : exception;
   Process_Already_Paused   : exception;
   Process_Not_Started      : exception;
   Process_Has_Terminated   : exception;
   Too_Many_Processes_Error : exception;

   function  Start (Program           : in String;
                    Arguments         : in String_Array;
                    Working_Directory : in String  := ".";
                    Input             : in Pipe    := Standard_Input;
                    Output            : in Pipe    := Standard_Output;
                    Errors            : in Pipe    := Standard_Error;
                    Pipeline          : in Boolean := False) return Process;

   function  Start (Command           : in String;
                    Working_Directory : in String  := ".";
                    Input             : in Pipe    := Standard_Input;
                    Output            : in Pipe    := Standard_Output;
                    Errors            : in Pipe    := Standard_Error;
                    Pipeline          : in Boolean := False) return Process;

   procedure Start (Process           : in out Shell.Process;
                    Program           : in     String;
                    Arguments         : in     String_Array;
                    Working_Directory : in     String  := ".";
                    Input             : in     Pipe    := Standard_Input;
                    Output            : in     Pipe    := Standard_Output;
                    Errors            : in     Pipe    := Standard_Error;
                    Pipeline          : in     Boolean := False);

   procedure Start (Process           : in out Shell.Process;
                    Command           : in     String;
                    Working_Directory : in     String  := ".";
                    Input             : in     Pipe    := Standard_Input;
                    Output            : in     Pipe    := Standard_Output;
                    Errors            : in     Pipe    := Standard_Error;
                    Pipeline          : in     Boolean := False);


   procedure Wait_On        (Process : in out Shell.Process);                    -- Raises Process_Not_Started if it has not been started.
   function  Has_Terminated (Process : in out Shell.Process) return Boolean;     -- False for a process which has not been started.
   function  Normal_Exit    (Process : in     Shell.Process) return Boolean;

   function  Image     (Process : in     Shell.Process) return String;

   procedure Kill      (Process : in out Shell.Process);
   procedure Interrupt (Process : in out Shell.Process);
   procedure Pause     (Process : in out Shell.Process);
   procedure Resume    (Process : in out Shell.Process);


   -----------
   --- Logging
   --

   procedure  Open_Log (Name : in String);
   procedure Close_Log;



private

   subtype Process_Template is POSIX.Process_Primitives.Process_Template;
   subtype Process_ID       is POSIX.Process_Identification.Process_ID;
   subtype File_Descriptor  is POSIX.IO.File_Descriptor;

   Null_Process_ID      : constant Process_ID      := POSIX.Process_Identification.Null_Process_ID;
   Null_File_Descriptor : constant File_Descriptor := File_Descriptor'Last;

   No_Data     : constant Data (1 .. 0) := [others => <>];
   Nil_Strings : constant String_Array  := [1 .. 0 => <>];

   package String_Vectors is new Ada.Containers.Vectors (Positive, Unbounded_String);
   subtype String_Vector  is String_Vectors.Vector;


   ---------
   --- Pipes
   --

   type Pipe_Id is mod 2**64;

   No_Pipe_Id : constant Pipe_Id := 0;     -- For the standard and null pipes, which are not tracked by 'Safe_Pipes'.

   type Pipe is
      record
         Write_End,
         Read_End : File_Descriptor := Null_File_Descriptor;
         Id       : Pipe_Id         := No_Pipe_Id;
      end record;

   Standard_Input  : constant Pipe := (Write_End => Null_File_Descriptor,
                                       Read_End  => POSIX.IO.Standard_Input,
                                       Id        => No_Pipe_Id);

   Standard_Output : constant Pipe := (Write_End => POSIX.IO.Standard_Output,
                                       Read_End  => Null_File_Descriptor,
                                       Id        => No_Pipe_Id);

   Standard_Error  : constant Pipe := (Write_End => POSIX.IO.Standard_Error,
                                       Read_End  => Null_File_Descriptor,
                                       Id        => No_Pipe_Id);

   Null_Pipe       : constant Pipe := (Write_End => Null_File_Descriptor,
                                       Read_End  => Null_File_Descriptor,
                                       Id        => No_Pipe_Id);

   package Descriptor_Owner_Maps is new Ada.Containers.Ordered_Maps (File_Descriptor, Pipe_Id, POSIX.IO."<");

   -- The OS reuses the number of a closed descriptor, and copies of a pipe outlive the closing of its ends. So each
   -- pipe has a unique id, and only the pipe which owns an open descriptor may use it.
   --
   protected Safe_Pipes
   is
      procedure Open  (Pipe           :    out Shell.Pipe);
      procedure Close (Pipe           : in     Shell.Pipe;
                       Only_Write_End : in     Boolean := False;
                       Only_Read_End  : in     Boolean := False);

      -- Gives a close-on-exec copy of the write end, if it is open. Checking first and duplicating
      -- afterwards would risk copying whichever descriptor has since reused that number.
      --
      procedure Duplicate_Write_End (Pipe       : in     Shell.Pipe;
                                     Descriptor :    out File_Descriptor;
                                     Is_Open    :    out Boolean);

      function  Owns  (Pipe           : in     Shell.Pipe;
                       Descriptor     : in     File_Descriptor) return Boolean;
   private
      Next_Id : Pipe_Id := No_Pipe_Id + 1;
      Owners  : Descriptor_Owner_Maps.Map;
   end Safe_Pipes;


   -- Queues the input to be written to the pipe by a background task, in the order given, so the caller is never blocked
   -- by a full pipe. Closing the write end takes effect once all queued input has been written.
   --
   -- If the reader goes, the rest of the input is discarded and the write end is closed, so a later call raises Pipe_Error.
   -- The writer gives up if the main subprogram has completed and the pipe has stopped accepting input.
   --
   -- As is most of this package, this is not task safe.
   --
   procedure Write_In_Background (Pipe  : in Shell.Pipe;
                                  Input : in Data);

   type Pipe_Array is array (Positive range <>) of Pipe;

   -- Waits until any of the pipes has output, or is at its end, or until the timeout has passed. A null pipe, or one
   -- whose read end is closed, is ignored. This finishes early for a pipe at its end, so a caller who loops should
   -- also wait a little when it finds nothing to read.
   --
   procedure Wait_For_Output (Pipes   : in Pipe_Array;
                              Timeout : in Duration);

   -- As for 'Output_Of', though without any wait, and giving no data for a pipe whose read end is closed.
   --
   function Available_Output_Of (Pipe : in Shell.Pipe) return Data;

   -- Moves this program's standard input to a new descriptor, which programs it starts don't inherit, and returns a pipe
   -- to read it. The standard input becomes empty, so a program started without any input sees the end of its input.
   --
   function Move_Standard_Input return Pipe;


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

   -------------
   --- Processes
   --

   type Process is
      record
         Id     : Process_ID    := Null_Process_ID;
         Status : POSIX.Process_Primitives.Termination_Status;
         State  : Process_State := Not_Started;
      end record;


   -- Sends the signal, without waiting to see whether the process ends. See 'Interrupt'.
   --
   procedure Send_Interrupt (Process : in out Shell.Process);


   -------------
   --- Debugging
   --

   procedure Log (Message : in String);
   function  Log (Message : in String) return Boolean;      -- Allow for logging in a declarative region.
                                                            -- Always returns 'True'.
end Shell;
