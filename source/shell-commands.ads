private
with
     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Indefinite_Holders;

package Shell.Commands
--
-- Any open pipes attached to a command will be automatically closed when the command goes out of scope.
--
-- For task safe 'Run' commands, see the 'Safe' child package.
--
is
   Command_Error : exception;

   type Command       is tagged private;
   type Command_Array is array (Positive range <>) of Command;

   package Forge
   is
      function To_Command  (Command_Line  : in String) return Command;           -- An example 'Command_Line' is "ps -A".
      function To_Commands (Pipeline      : in String) return Command_Array;     -- An example 'Pipeline' is "ps -A | grep bash | wc".

      function "+"         (Command_Line  : in String) return Command       renames To_Command;
      function "+"         (Pipeline      : in String) return Command_Array renames To_Commands;
   end Forge;

   function  Image   (The_Command : in Command) return String;

   procedure Connect (From, To : in out Command);           -- Connects 'From's output to 'To's input via a pipe.
   procedure Connect (Commands : in out Command_Array);     -- Connects each command in a pipeline.

   function   Input_Pipe (The_Command : in     Command) return Pipe;
   function  Output_Pipe (The_Command : in     Command) return Pipe;
   function   Error_Pipe (The_Command : in     Command) return Pipe;

   function  Name      (The_Command : in     Command) return String;
   function  Arguments (The_Command : in     Command) return String;

   function  Process (The_Command : in out Command) return access Shell.Process;
   function  Failed  (The_Command : in     Command) return Boolean;


   --- The Start subprograms return before the process completes.
   --

   procedure Start (The_Command : in out Command;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := False);

   procedure Start (Commands    : in out Command_Array;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := True);

   procedure Send (To    : in Command;
                   Input : in Data);

   --- Command Results
   --

   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is private;

   function  Results_Of (The_Command : in out Command'Class) return Command_Results;

   function  Output_Of  (The_Results : in     Command_Results) return Data;
   function  Errors_Of  (The_Results : in     Command_Results) return Data;


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
                  Input        : in     Data  := No_Data) return Command_Results;
   --
   -- Takes a command line and runs a Command or a Pipeline, as appropriate.

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

   procedure Wait_On        (The_Command : in out Command);
   function  Has_Terminated (The_Command : in out Command) return Boolean;
   function  Normal_Exit    (The_Command : in     Command) return Boolean;

   procedure Kill      (The_Command : in     Command);
   procedure Interrupt (The_Command : in     Command);
   procedure Pause     (The_Command : in out Command);
   procedure Resume    (The_Command : in out Command);
   function  Is_Paused (The_Command : in     Command) return Boolean;



private

   type Count        is new Natural;
   type Count_Access is access all Count;

   subtype Data_Index   is Data_Offset range 1 .. Data_Offset'Last;
   package Data_Vectors is new Ada.Containers.Indefinite_Vectors (Data_Index, Data);
   subtype Data_Vector  is Data_Vectors.Vector;

   type Command is new Ada.Finalization.Controlled with
      record
         Name        : Unbounded_String;
         Arguments   : String_Vector;

         Input_Pipe  : Pipe := Standard_Input;
         Output_Pipe : Pipe := Null_Pipe;
         Error_Pipe  : Pipe := Null_Pipe;

         Owns_Output_Pipe : Boolean := False;
         Owns_Input_Pipe  : Boolean := False;

         Process     : aliased Shell.Process;
         Copy_Count  :         Count_Access;

         Output      : Data_Vector;
         Errors      : Data_Vector;

         Paused      : Boolean := False;
      end record;

   overriding
   procedure Adjust   (The_Command : in out Command);
   overriding
   procedure Finalize (The_Command : in out Command);

   procedure Define   (The_Command :    out Command;   Command_Line : in String);

   procedure Gather_Results (The_Command : in out Command);
   procedure Stop           (The_Command : in out Command);

   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is
      record
         Output  : Data (1 .. Output_Size);
         Errors  : Data (1 ..  Error_Size);
      end record;



   function To_String_Array (Strings : String_Vector) return String_Array;


   -----------------------
   -- Spawn Server Support
   --

   type Command_Id is new Positive;

   Null_Id : constant Command_Id := Command_Id'Last;

   function Hash (Id : in Command_Id) return Ada.Containers.Hash_Type;


   package Data_Holders is new Ada.Containers.Indefinite_Holders (Element_Type => Data);
   subtype Data_Holder  is     Data_Holders.Holder;


   type Server_Action_Kind is (Nil, New_Command, New_Input, Kill, Interrupt, Pause, Resume, Stop);

   type Server_Action (Kind : Server_Action_Kind := Nil) is
      record
         Id : Command_Id := Null_Id;

         case Kind
         is
         when New_Command =>
            Command_Line  : Unbounded_String;
            Command_Input : Data_Holder;

         when New_Input =>
            Data : Data_Holder;

         when Nil | Kill | Interrupt | Pause | Resume | Stop =>
            null;
         end case;
      end record;


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
               Normal_Exit : Boolean;

            when Server_Done =>
               null;
         end case;
      end record;


end Shell.Commands;
