private
with
     Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Indefinite_Holders;


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
   function  Failed    (The_Command : in Command'Class) return Boolean;


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
         Copy_Count : Count_Access;

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


   -------------------
   --- Command_Results
   --
   type Command_Results (Output_Size : Data_Offset;
                         Error_Size  : Data_Offset) is
      record
         Output  : Data (1 .. Output_Size);
         Errors  : Data (1 ..  Error_Size);
      end record;


   --------------------
   --- String Utilities
   --
   function To_String_Array  (Strings  : in String_Vector) return String_Array;
   function To_Command_Lines (Pipeline : in String)        return String_Array;
   --
   -- Split a pipeline into separate command strings.


   -----------------------
   -- Spawn Server Support
   --

   type Command_Id is new Positive;

   Null_Id : constant Command_Id := Command_Id'Last;

   function Hash (Id : in Command_Id) return Ada.Containers.Hash_Type;


   package Data_Holders is new Ada.Containers.Indefinite_Holders (Element_Type => Data);
   subtype Data_Holder  is     Data_Holders.Holder;


   ------------------
   --- Server Actions
   --
   type Server_Action_Kind is (Nil,
                               New_Command, New_Pipeline, New_Input,
                               Kill, Interrupt, Pause, Resume, Stop,
                               Shutdown);

   type Server_Action (Kind : Server_Action_Kind := Nil) is
      record
         Id : Command_Id := Null_Id;

         case Kind
         is
         when New_Command =>
            Command_Line  : Unbounded_String;
            Command_Input : Data_Holder;
            Accepts_Input : Boolean;

         when New_Pipeline =>
            Pipeline       : Unbounded_String;
            Pipeline_Input : Data_Holder;

         when New_Input =>
            Data : Data_Holder;

         when Nil | Kill | Interrupt | Pause | Resume | Stop | Shutdown =>
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
               Normal_Exit : Boolean;

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
