with
     GNAT.OS_Lib,

     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.IO_Exceptions,
     Ada.Unchecked_Conversion,

     POSIX.Process_Primitives.Extensions;

package body Shell
is
   --- Strings
   --

   function "+" (Item : in String) return Unbounded_String
   is
   begin
      return To_Unbounded_String (Item);
   end "+";


   function "+" (Item : in Unbounded_String) return String
   is
   begin
      return To_String (Item);
   end "+";


   function To_String_Vector (Strings : String_Array) return String_Vector
   is
      use String_Vectors;
      Vector : String_Vector;
   begin
      for Each of Strings
      loop
         Vector.Append (Each);
      end loop;

      return Vector;
   end To_String_Vector;


   function To_String_Array (Strings : String_Vector) return String_Array
   is
      use String_Vectors;
      The_Array : String_Array (1 .. Natural (Strings.Length));
   begin
      for I in The_Array'Range
      loop
         The_Array (I) := Strings.Element (I);
      end loop;

      return The_Array;
   end To_String_Array;


   function To_String (From : in Data) return String
   is
      subtype  My_Data   is Data   (From'Range);
      subtype  My_String is String (1 .. From'Length);
      function Convert   is new Ada.Unchecked_Conversion (My_Data, My_String);
   begin
      return Convert (From);
   end To_String;


   function To_Stream (From : in String) return Data
   is
      subtype  My_String is String (From'Range);
      subtype  My_Data   is Data   (0 .. From'Length - 1);
      function Convert   is new Ada.Unchecked_Conversion (My_String, My_Data);
   begin
      return Convert (From);
   end To_Stream;


   --- Commands
   --

   function To_Arguments (All_Arguments : in String) return String_Array
   is
      use GNAT.OS_Lib;
      Command_Name : constant String      := "Command_Name";     -- Argument_String_To_List expects the command name to be the 1st piece
                                                                 -- of the string, so we provide a dummy name.
      Arguments    : Argument_List_Access := Argument_String_To_List (  Command_Name
                                                                       & " "
                                                                       & All_Arguments);
      Result       : String_Array (1 .. Arguments'Length - 1);
   begin
      for i in Result'Range
      loop
         Result (i) := +Arguments (i + 1).all;
      end loop;

      Free (Arguments);
      return Result;
   end To_Arguments;


   procedure define (The_Command : out Command;   Command_Line : in String)
   is
      use Ada.Strings.Fixed;

      I : constant Natural := Index (Command_Line, " ");
   begin
      if I = 0
      then
         The_Command.Name := +Command_Line;
         return;
      end if;

      declare
         Name      : constant String       :=               Command_Line (Command_Line'First .. I - 1);
         Arguments : constant String_Array := To_Arguments (Command_Line (I + 1              .. Command_Line'Last));
      begin
         The_Command.Name      := +(Name);
         The_Command.Arguments := To_String_Vector (Arguments);
      end;
   end define;


   function To_Command (Command_Line : in String) return Command
   is
      use Ada.Strings.Fixed;

      I : constant Natural := Index (Command_Line, " ");
   begin
      if I = 0
      then
         return Result : Command
         do
            Result.Name := +Command_Line;
         end return;
      end if;

      declare
         Name      : constant String       :=               Command_Line (Command_Line'First .. I - 1);
         Arguments : constant String_Array := To_Arguments (Command_Line (I + 1              .. Command_Line'Last));
      begin
         return Result : Command
         do
            Result.Name      := +(Name);
            Result.Arguments := To_String_Vector (Arguments);
         end return;
      end;
   end to_Command;


   function To_Commands (Pipeline : in String) return Command_Array
   is
      use Ada.Strings.Fixed;

      Cursor : Positive := Pipeline'First;
      First,
      Last   : Positive;
      Count  : Natural := 0;

      All_Commands : String_Array (1 .. Max_Commands_In_Pipeline);
   begin
      loop
         Find_Token (Source => Pipeline,
                     Set    => Ada.Strings.Maps.To_Set ('|'),
                     From   => Cursor,
                     Test   => Ada.Strings.Outside,
                     First  => First,
                     Last   => Last);
         declare
            Full_Command : constant String := Trim (Pipeline (First .. Last),
                                                    Ada.Strings.Both);
         begin
            Count                :=  Count + 1;
            All_Commands (Count) := +Full_Command;
         end;

         exit when Last = Pipeline'Last;

         Cursor := Last + 1;
      end loop;

      return Result : Command_Array (1 .. Count)
      do
         for I in 1 .. Count
         loop
            define (Result (I), +All_Commands (I));
         end loop;
      end return;
   end To_Commands;


   procedure Connect (From, To : in out Command)
   is
      Pipe : constant Shell.Pipe := to_Pipe;
   begin
      From.Output_Pipe := Pipe;
      To.Input_Pipe    := Pipe;
   end Connect;


   procedure Connect (Commands : in out Command_Array)
   is
   begin
      for I in Commands'First .. Commands'Last - 1
      loop
         Connect (From => Commands (I),
                  To   => Commands (I + 1));
      end loop;
   end Connect;


   procedure Close_Pipe_Write_Ends (Command : in Shell.Command)
   is
   begin
      if Command.Output_Pipe /= Standard_Output
      then
         Close_Write_End (Command.Output_Pipe);
      end if;

      if Command.Error_Pipe /= Standard_Error
      then
         Close_Write_End (Command.Error_Pipe);
      end if;
   end Close_Pipe_Write_Ends;


   function Input_Pipe (The_Command : in Command) return Pipe
   is
   begin
      return The_Command.Input_Pipe;
   end Input_Pipe;


   function Output_Pipe (The_Command : in Command) return Pipe
   is
   begin
      return The_Command.Output_Pipe;
   end Output_Pipe;


   function Error_Pipe (The_Command : in Command) return Pipe
   is
   begin
      return The_Command.Error_Pipe;
   end Error_Pipe;


   function Process_of (The_Command : in out Command) return access Process
   is
   begin
      return The_Command.Process'Unchecked_Access;
   end Process_of;


   --- Start
   --

   procedure Start (The_Command : in out Command;
                    Input       : in     Data    := No_Data;
                    Pipeline    : in     Boolean := False)
   is
      Input_Pipe : constant Shell.Pipe := (if Input = No_Data then The_Command.Input_Pipe else To_Pipe);
   begin
      The_Command.Input_Pipe := Input_Pipe;

      if Input_Pipe /= Standard_Input
      then
         Write_To (Input_Pipe, Input);
      end if;

      The_Command.Process := Start (Program   => +The_Command.Name,
                                    Arguments =>  To_String_Array (The_Command.Arguments),
                                    Input     =>  The_Command.Input_Pipe,
                                    Output    =>  The_Command.Output_Pipe,
                                    Errors    =>  The_Command.Error_Pipe,
                                    Pipeline  =>  Pipeline);
   end Start;


   procedure Start (Commands : in out Command_Array;
                    Input    : in     Data    := No_Data;
                    Pipeline : in     Boolean := True)
   is
      First_Command :          Command renames Commands (Commands'First);
      Input_Pipe    : constant Shell.Pipe   := (if Input = No_Data then First_Command.Input_Pipe else To_Pipe);
   begin
      First_Command.Input_Pipe := Input_Pipe;

      if Input_Pipe /= Standard_Input
      then
         Write_To (Input_Pipe, Input);
      end if;

      if not Pipeline
      then
         for I in Commands'Range
         loop
            Start (Commands (I));
         end loop;

         return;
      end if;

      Connect (Commands);

      for I in Commands'Range
      loop
         Start (Commands (I),
                Pipeline => True);

         -- Since we are making a pipeline, we need to close the write ends of
         -- the Output & Errors pipes ourselves.
         --
         if I /= Commands'First
         then
            Close_Pipe_Write_Ends (Commands (I - 1));          -- Close ends for the prior command.
         end if;

         if I = Commands'Last
         then
            Close_Pipe_Write_Ends (Commands (Commands'Last));  -- Close ends for the final command.
         end if;
      end loop;

   end Start;


   --- Run
   --

   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False)
   is
   begin
      The_Command.Output_Pipe := To_Pipe;
      The_Command. Error_Pipe := To_Pipe;

      Start (The_Command, Input);

      if     not Wait_On_Normal_Exit (The_Command.Process)   -- This waits til command completion.
        and then Raise_Error
      then
         declare
            Error : constant String := +Output_Of (The_Command.Error_Pipe);
         begin
            raise Command_Error with Error;
         end;
      end if;
   end Run;


   function Run (The_Command : in out Command;
                 Input       : in     Data    := No_Data;
                 Raise_Error : in     Boolean := False) return Command_Results
   is
   begin
      Run (The_Command, Input, Raise_Error);

      return Results_Of (The_Command);
   end Run;


   procedure Run (The_Pipeline : in out Command_Array;
                  Input        : in     Data    := No_Data;
                  Raise_Error  : in     Boolean := False)
   is
      Last_Command : Shell.Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Last_Command.Output_Pipe := To_Pipe;
      Last_Command. Error_Pipe := To_Pipe;

      Start (The_Pipeline, Input);

      for i in The_Pipeline'Range
      loop
         if not Wait_On_Normal_Exit (The_Pipeline (i).Process)   -- This waits til command completion.
         then
            if Raise_Error
            then
               declare
                  Error : constant String :=   "Pipeline command" & Integer'Image (i)
                                             & " '" & (+The_Pipeline (i).Name) & "' failed.";
               begin
                  raise Command_Error with Error;
               end;
            end if;

            return;
         end if;
      end loop;

   end Run;


   function Run (The_Pipeline : in out Command_Array;
                 Input        : in     Data    := No_Data;
                 Raise_Error  : in     Boolean := False) return Command_Results
   is
      Last_Command : Shell.Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Run (The_Pipeline, Input, Raise_Error);

      return Results_Of (Last_Command);
   end Run;


   function Run (Command_Line : in String;
                 Input        : in Data  := No_Data) return Command_Results
   is
      use Ada.Strings.Fixed;
      The_Index   : constant Natural := Index (Command_Line, " | ");
      Is_Pipeline : constant Boolean := (if The_Index = 0 then False else True);
   begin
      if Is_Pipeline
      then
         declare
            The_Commands : Command_Array := To_Commands (Command_Line);
         begin
            return Run (The_Commands, Input);
         end;
      else
         declare
            The_Command : Command := To_Command (Command_Line);
         begin
            return Run (The_Command, Input);
         end;
      end if;
   end Run;


   procedure Run (Command_Line : in String;
                  Input        : in Data  := No_Data)
   is
      Results : Command_Results := Run (Command_Line, Input) with Unreferenced;
   begin
      null;
   end Run;


   function Failed (The_Pipeline : in Command_Array) return Boolean
   is
   begin
      for Each of The_Pipeline
      loop
         if not Normal_Exit (Each.Process)
         then
            return True;
         end if;
      end loop;

      return False;
   end Failed;


   function Which_Failed (The_Pipeline : in Command_Array) return Natural
   is
   begin
      for i in The_Pipeline'Range
      loop
         if not Normal_Exit (The_Pipeline (i).Process)
         then
            return i;
         end if;
      end loop;

      return 0;
   end Which_Failed;


   -- Command Results
   --

   function Results_Of (The_Command : in out Command) return Command_Results
   is
      Output : constant Data := Output_Of (The_Command.Output_Pipe);
      Error  : constant Data := Output_Of (The_Command. Error_Pipe);
   begin
      return (Output_Size => Output'Length,
              Error_Size  => Error 'Length,
              Output      => Output,
              Errors      => Error);
   end Results_Of;


   overriding
   procedure Finalize (The_Command : in out Command)
   is
   begin
      Close (The_Command. Input_Pipe);
      Close (The_Command.Output_Pipe);
      Close (The_Command. Error_Pipe);
   end Finalize;


   function Output_Of (The_Results : in Command_Results) return Data
   is
   begin
      return The_Results.Output;
   end Output_Of;


   function Errors_Of (The_Results : in Command_Results) return Data
   is
   begin
      return The_Results.Errors;
   end Errors_Of;


   --- Pipes
   --

   function To_Pipe return Pipe
   is
      The_Pipe : Pipe;
   begin
      POSIX.IO.Create_Pipe (Read_End  => The_Pipe.Read_End,
                            Write_End => The_Pipe.Write_End);
      return The_Pipe;
   end To_Pipe;


   function Output_Of (The_Pipe : in Pipe) return Data

   is
      use POSIX;
      Max_Process_Output : constant := 200 * 1024;

      Buffer : Data (1 .. Max_Process_Output);
      Last   : Stream_Element_Offset;
   begin
      IO.Read (File   => The_Pipe.Read_End,
               Buffer => Buffer,
               Last   => Last);
      return Buffer (1 .. Last);

   exception
      when Ada.IO_Exceptions.End_Error =>
         return No_Data;
   end Output_Of;


   procedure Write_To (The_Pipe : in Pipe;   Input : in Data)
   is
      subtype   My_Data is Data (Input'Range);
      procedure Write   is new POSIX.IO.Generic_Write (My_Data);
   begin
      Write (The_Pipe.Write_End, Input);
   end Write_To;


   procedure Close (The_Pipe : in Pipe)
   is
      use POSIX.IO;
   begin
      if    The_Pipe /= Standard_Input
        and The_Pipe /= Standard_Output
        and The_Pipe /= Standard_Error
      then
         if Is_Open (The_Pipe.Read_End) then
            Close (File => The_Pipe.Read_End);
         end if;

         if Is_Open (The_Pipe.Write_End) then
            Close (File => The_Pipe.Write_End);
         end if;
      end if;
   end Close;


   procedure Close_Write_End (The_Pipe : in Pipe)
   is
   begin
      POSIX.IO.Close (The_Pipe.Write_End);
   end Close_Write_End;


   function Close_Write_End (The_Pipe : in Pipe) return Boolean
   is
   begin
      Close_Write_End (The_Pipe);
      return True;
   end Close_Write_End;


   --- Pipe Streams
   --

   overriding
   procedure Read (Stream : in out Pipe_Stream;
                   Item   :    out Stream_Element_Array;
                   Last   :    out Stream_Element_Offset)
   is
   begin
      POSIX.IO.Read (File   => Stream.Pipe.Read_End,
                     Buffer => Item,
                     Last   => Last);
   end Read;


   overriding
   procedure Write (Stream : in out Pipe_Stream;
                    Item   : in     Stream_Element_Array)
   is
      Last : Ada.Streams.Stream_Element_Offset;
      pragma Unreferenced (Last);
   begin
      POSIX.IO.Write (File   => Stream.Pipe.Write_End,
                      Buffer => Item,
                      Last   => Last);
   end Write;


   --- Processes
   --

   function Start (Program           : in String;
                   Arguments         : in String_Array := Nil_Strings;
                   Working_Directory : in String       := ".";
                   Input             : in Pipe         := Standard_Input;
                   Output            : in Pipe         := Standard_Output;
                   Errors            : in Pipe         := Standard_Error;
                   Pipeline          : in Boolean      := False) return Process
   is
      use POSIX,
          POSIX.Process_Primitives,
          POSIX.Process_Primitives.Extensions;

      The_Template   : Process_Template;
      The_Process    : Process;
      The_Process_Id : Process_Id;

      Args  :          POSIX_String_List;
      Name  : constant POSIX_String     := To_POSIX_String (Program);

   begin
      Open_Template (The_Template);

      if Errors /= Standard_Error
      then
         Set_File_Action_To_Close     (The_Template, Errors.Read_End);
         Set_File_Action_To_Duplicate (The_Template, POSIX.IO.Standard_Error,
                                                     Errors.Write_End);
         Set_File_Action_To_Close     (The_Template, Errors.Write_End);
      end if;

      if Output /= Standard_Output
      then
         Set_File_Action_To_Close     (The_Template, Output.Read_End);
         Set_File_Action_To_Duplicate (The_Template, POSIX.IO.Standard_Output,
                                                     Output.Write_End);
         Set_File_Action_To_Close     (The_Template, Output.Write_End);
      end if;

      if Input /= Standard_Input
      then
         Set_File_Action_To_Close     (The_Template, Input.Write_End);
         Set_File_Action_To_Duplicate (The_Template, POSIX.IO.Standard_Input,
                                                     Input.Read_End);
         Set_File_Action_To_Close     (The_Template, Input.Read_End);
      end if;

      Append (Args, Name);

      for I in Arguments'Range
      loop
         Append (Args, To_POSIX_String (+Arguments (I)));
      end loop;

      Start_Process_Search (The_Process_Id,
                            Name,
                            Working_Directory,
                            The_Template,
                            Args);

      Close_Template (The_Template);
      Make_Empty (Args);

      if Input /= Standard_Input
      then
         POSIX.IO.Close (Input.Read_End);
      end if;

      -- When in a pipeline of processes, the write ends of The_Process's 'Output' & 'Errors' pipes must remain open, in
      -- the main process, until the next process in the pipeline (which uses the pipe as 'Input') is started (spawned).
      --
      if not Pipeline
      then
         if Output /= Standard_Output
         then
            POSIX.IO.Close (Output.Write_End);
         end if;

         if Errors /= Standard_Error
         then
            POSIX.IO.Close (Errors.Write_End);
         end if;
      end if;

      The_Process.Id := The_Process_Id;
      return The_Process;
   end Start;


   function Start (Command           : in String;
                   Working_Directory : in String  := ".";
                   Input             : in Pipe    := Standard_Input;
                   Output            : in Pipe    := Standard_Output;
                   Errors            : in Pipe    := Standard_Error;
                   Pipeline          : in Boolean := False) return Process
   is
   begin
      return Start (Program           => "/bin/sh",
                    Arguments         => (+"-c",
                                          +Command),
                    Working_Directory => Working_Directory,
                    Input             => Input,
                    Output            => Output,
                    Errors            => Errors,
                    Pipeline          => Pipeline);
   end Start;


   procedure Wait_On (Process : in out Shell.Process)
   is
      use POSIX.Process_Primitives;
   begin
      Wait_For_Child_Process (Status => Process.Status,
                              Child  => Process.Id,
                              Block  => True);
   end Wait_On;


   function Has_Terminated (Process : in out Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives;
   begin
      Wait_For_Child_Process (Status => Process.Status,
                              Child  => Process.Id,
                              Block  => False);

      return Status_Available (Process.Status);
   end Has_Terminated;


   function Wait_On_Normal_Exit (Process : in out Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives;
   begin
      Wait_For_Child_Process (Status => Process.Status,
                              Child  => Process.Id,
                              Block  => True);

      return Normal_Exit (Process);
   end Wait_On_Normal_Exit;


   function Normal_Exit (Process : in Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives;
   begin
      if not Status_Available (Process.Status) then
         return False;
      end if;

      if Exit_Status_Of (Process.Status) = POSIX.Process_Primitives.Normal_Exit then
         return True;
      end if;

      return False;
   end Normal_Exit;


   function Image (Process : in Shell.Process) return String
   is
      use POSIX.Process_Identification;
   begin
      return Image (Process.Id);
   end Image;


end Shell;
