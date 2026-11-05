private
with
     Ada.Containers.Vectors,
     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Indefinite_Holders,
     Ada.Finalization;


package Shell.Commands
--
-- For task safe 'Run' commands and pipelines,        see the 'Safe'   child package.
-- For commands and pipelines run from a single task, see the 'Unsafe' child package.
--
is
   type Command is abstract tagged private;

   Command_Error : exception;


   function  Image     (The_Command : in Command)       return String;
   function  Name      (The_Command : in Command)       return String;
   function  Arguments (The_Command : in Command)       return String;
   function  Failed    (The_Command : in out Command'Class) return Boolean;     -- A command which hasn't ended, or hasn't started, hasn't failed.


   -------------------
   --- Command Results
   --

   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is private;

   function  Results_Of (The_Command : in out Command'Class)   return Command_Results;
   function  Output_Of  (The_Results : in     Command_Results) return Data;
   function  Errors_Of  (The_Results : in     Command_Results) return Data;


   ---------
   --- Start ~ Commands return before the process completes.
   --

   procedure Start (The_Command   : in out Command;
                    Input         : in     Data    := No_Data;
                    Accepts_Input : in     Boolean := False;
                    Pipeline      : in     Boolean := False);

   procedure Send  (To    : in Command;
                    Input : in Data) is abstract;
   --
   -- Queues the input to be written in the background, so this doesn't wait for the program to read it. Input which
   -- the program never reads stays in memory until the program ends, or its input is closed and the main program ends.

   procedure Close_Input (The_Command : in Command) is abstract;
   --
   -- Lets a command started with 'Accepts_Input' see the end of its input, once all sent input has been written.
   -- Does nothing if there is no input to close.


   -------
   --- Run - Commands block until process completes.
   --

   procedure Run (The_Command  : in out Command;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) is abstract;

   function  Run (The_Command  : in out Command'Class;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False) return Command_Results;


   type State is (Not_Started,
                  Running,
                  Paused,
                  Normal_Exit,
                  Failed_Exit,
                  Killed);

   function  Status         (The_Command : in out Command) return State;

   procedure Wait_On        (The_Command : in out Command)                is abstract;
   function  Has_Terminated (The_Command : in out Command) return Boolean is abstract;
   function  Normal_Exit    (The_Command : in     Command) return Boolean is abstract;

   procedure Kill      (The_Command : in out Command);
   procedure Interrupt (The_Command : in out Command) is abstract;
   procedure Pause     (The_Command : in out Command);
   procedure Resume    (The_Command : in out Command);



private

   type Count        is new Natural;
   type Count_Access is access all Count;

   subtype Data_Index   is Data_Offset range 1 .. Data_Offset'Last;
   package Data_Vectors is new Ada.Containers.Indefinite_Vectors (Data_Index, Data);
   subtype Data_Vector  is Data_Vectors.Vector;

   -----------
   --- Command
   --
   type Command is abstract new Ada.Finalization.Controlled with
      record
         Name       : Unbounded_String;
         Arguments  : String_Vector;
         Copy_Count : Count_Access := new Count' (1);

         Output     : Data_Vector;
         Errors     : Data_Vector;

         Status     : State := Not_Started;
      end record;

   overriding
   procedure Adjust   (The_Command : in out Command);

   overriding
   procedure Finalize (The_Command : in out Command);

   procedure Define   (The_Command :    out Command;   Command_Line : in String);

   procedure Gather_Results (The_Command : in out Command) is null;

   function  Failure_Message (The_Command : in Command'Class) return String;
   --
   -- Includes any gathered errors, which remain available via 'Results_Of'.

   -- Raises 'Command_Error', when the command has not been started. The action is part of the message.
   --
   procedure Check_Is_Started (The_Command : in Command'Class;
                               Action      : in String);


   -------------------
   --- Command_Results
   --
   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is
      record
         Output  : Data (1 .. Output_Size);
         Errors  : Data (1 ..  Error_Size);
      end record;

   function With_Earlier_Errors (The_Results : in Command_Results;
                                 Earlier     : in Data_Vector) return Command_Results;
   --
   -- Puts the errors of a pipeline's earlier commands ahead of those of its last command.


   --------------------
   --- String Utilities
   --
   function To_String_Array  (Strings  : in String_Vector) return String_Array;
   function To_Command_Lines (Pipeline : in String)        return String_Array;
   --
   -- Split a pipeline into separate command strings. Raises 'Command_Error' if any would be empty.


   -----------------------
   -- Spawn Server Support
   --

   type Command_Id is new Positive;

   Null_Id : constant Command_Id := Command_Id'Last;

   function Hash (Id : in Command_Id) return Ada.Containers.Hash_Type;


   -- A pipe can hold more than a single read returns, so output is read until none is left. This is limited since another
   -- process which inherited the pipe, such as a background child of the command, may write to it for ever.
   --
   Most_Reads_Per_Gather : constant := 64;

   package Data_Holders is new Ada.Containers.Indefinite_Holders (Element_Type => Data);
   subtype Data_Holder  is     Data_Holders.Holder;


   -- The server writes this before anything else, as it is a separate program which may be older or newer than the
   -- client. Increase it with any change to the actions below. How it is written must never change.
   --
   Protocol_Version : constant Integer := 2;


   ------------------
   --- Server Actions
   --
   -- A command's name and arguments are sent as they are, as joining them into one line to be split again would lose
   -- what was escaped, and an argument containing a '|' would become another command.
   --
   type Command_Line is
      record
         Name      : Unbounded_String;
         Arguments : String_Vector;
      end record;

   package Command_Line_Vectors is new Ada.Containers.Vectors (Positive, Command_Line);
   subtype Command_Lines        is Command_Line_Vectors.Vector;

   function To_Command_Line (The_Command : in Command'Class) return Command_Line;


   type Server_Action_Kind is (Nil,
                               New_Command, New_Pipeline, New_Input,
                               Kill, Interrupt, Pause, Resume, Stop,
                               Shutdown,
                               Close_Input);

   type Server_Action (Kind : Server_Action_Kind := Nil) is
      record
         Id : Command_Id := Null_Id;

         case Kind
         is
         when New_Command =>
            The_Line      : Command_Line;
            Command_Input : Data_Holder;
            Accepts_Input : Boolean;

         when New_Pipeline =>
            The_Lines      : Command_Lines;     -- Their commands have ids from 'Id' onwards.
            Pipeline_Input : Data_Holder;

         when New_Input =>
            Data : Data_Holder;

         when Nil | Kill | Interrupt | Pause | Resume | Stop | Shutdown | Close_Input =>
            null;
         end case;
      end record;


   ------------------
   --- Client Actions
   --
   type Client_Action_Kind is (New_Outputs, Command_Done, Server_Done);

   type Client_Action (Kind : Client_Action_Kind) is
      record
         Id : Command_Id := Null_Id;

         case Kind
         is
            when New_Outputs =>
               Output : Data_Holder;
               Errors : Data_Holder;

            when Command_Done =>
               Normal_Exit  : Boolean;
               Not_At_Fault : Boolean;     -- It only ended as another command of its pipeline couldn't start.
               Was_Killed   : Boolean;     -- The server killed it, rather than it having ended by itself first.

            when Server_Done =>
               null;
         end case;
      end record;



   --- Ensure mutual exclusion of 'Safe' and 'Unsafe' commands.
   --

     Safe_Commands_Are_Withed : Boolean := False;
   UnSafe_Commands_Are_Withed : Boolean := False;

   Halt_Spawn_Client : access procedure;     -- Used during elaboration of safe and unsafe command packages
                                             -- to allow elaboration of the unsafe commands package to stop
                                             -- safe commands 'Spawn_Client' task, if neccessary.


end Shell.Commands;
