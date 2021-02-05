with
     GNAT.OS_Lib,

     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.IO_Exceptions,
     Ada.Unchecked_Deallocation,

     POSIX.Process_Primitives.Extensions;

package body Shell
is
   -- Strings
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


   -- Commands
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


   function To_Command (Command_Line : in String) return Command
   is
      use Ada.Strings.Fixed;

      I : constant Natural := Index (Command_Line, " ");     -- TODO: Check for other legal whitespace.
   begin
      if I = 0
      then
         declare
            Result : Command;
         begin
            Result.Name := +Command_Line;
            return Result;
         end;
      end if;

      declare
         Name      : constant String       :=               Command_Line (Command_Line'First .. I - 1);
         Arguments : constant String_Array := To_Arguments (Command_Line (I + 1              .. Command_Line'Last));

         Result    : Command (Argument_Count => Arguments'Length);
      begin
         Result.Name      := +(Name);
         Result.Arguments := Arguments;
         return Result;
      end;
   end to_Command;


   function To_Commands (Pipeline : in String) return Command_Array
   is
      use Ada.Strings.Fixed;

      Cursor : Positive := Pipeline'First;

      First,
      Last   : Positive;

      Result : Command_Array (1 .. Max_Commands_In_Pipeline);
      Count  : Natural := 0;
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
            Count          := Count + 1;
            Result (Count) := to_Command (Full_Command);
         end;

         exit when Last = Pipeline'Last;

         Cursor := Last + 1;
      end loop;

      return Result (1 .. Count);
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
      for I in Commands'Range
      loop
         exit when I + 1 > Commands'Last;

         Connect (From => Commands (I),
                  To   => Commands (I + 1));
      end loop;
   end Connect;


   function Run (The_Command : in Command;
                 Pipeline    : in Boolean := False) return Process
   is
      Process : Shell.Process;
   begin
      Process := Start (Program   => +The_Command.Name,
                        Arguments =>  The_Command.Arguments,
                        Input     =>  The_Command.Input_Pipe,
                        Output    =>  The_Command.Output_Pipe,
                        Errors    =>  The_Command.Error_Pipe,
                        Pipeline  =>  Pipeline);
      return Process;
   end Run;


   procedure Run (The_Command : in Command;
                  Pipeline    : in Boolean := False)
   is
      Process : Shell.Process := Run (The_Command, Pipeline);     -- Work is done here.
      pragma Unreferenced (Process);                              -- We don't care about the returned process.
   begin
      null;
   end Run;


   function Run (Commands : in out Command_Array;
                 Pipeline : in     Boolean      := True) return Process_Array
   is
      Processes : Process_Array (Commands'Range);
   begin
      if not Pipeline
      then
         for I in Commands'Range
         loop
            Processes (I) := Run (Commands (I));
         end loop;

         return Processes;
      end if;

      Connect (Commands);

      for I in Commands'Range
      loop
         declare
            procedure Close_Pipe_Write_Ends (Command : in Shell.Command)
            is
            begin
               if Command.Output_Pipe /= Standard_Output
               then
                  POSIX.IO.Close (Command.Output_Pipe.Write_End);
               end if;

               if Command.Error_Pipe /= Standard_Error
               then
                  POSIX.IO.Close (Command.Error_Pipe.Write_End);
               end if;
            end Close_Pipe_Write_Ends;

         begin
            Processes (I) := Run (Commands (I),
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
         end;
      end loop;

      return Processes;
   end Run;


   procedure Run (Commands : in out Command_Array;
                  Pipeline : in     Boolean      := True)
   is
      Processes : Process_Array := Run (Commands, Pipeline);     -- Work is done here.
      pragma Unreferenced (Processes);                           -- Not interested in Processes.
   begin
      null;
   end Run;


   function Command_Output (The_Command : in out Command) return String
   is
      Output_Pipe : constant Shell.Pipe   := To_Pipe;
      Error_Pipe  : constant Shell.Pipe   := To_Pipe;
      Process     :          Shell.Process;
   begin
      The_Command.Output_Pipe := Output_Pipe;
      The_Command. Error_Pipe :=  Error_Pipe;

      Process := Run (The_Command);
      Wait_On (Process);

      if Normal_Exit (Process)
      then
         declare
            Output : constant String := Output_Of (Output_Pipe);
         begin
            close (Output_Pipe);
            close ( Error_Pipe);
            return Output;
         end;
      else
         declare
            Error : constant String := Output_Of (Error_Pipe);
         begin
            close (Output_Pipe);
            close ( Error_Pipe);
            raise Command_Error with Error;
         end;
      end if;
   end Command_Output;


   function Pipeline_Output (The_Commands : in out Command_Array) return String
   is
      Last_Command :          Shell.Command renames The_Commands (The_Commands'Last);
      Output_Pipe  : constant Shell.Pipe         := To_Pipe;
      Error_Pipe   : constant Shell.Pipe         := To_Pipe;
   begin
      Last_Command.Output_Pipe := Output_Pipe;
      Last_Command. Error_Pipe := Error_Pipe;

      declare
         Process_List : constant Shell.Process_Array := Run (The_Commands);
         Last_Process :          Shell.Process  renames Process_List (Process_List'Last);
      begin
         Wait_On (Last_Process);

         if Normal_Exit (Last_Process)
         then
            declare
               Output : constant String := Output_Of (Output_Pipe);
            begin
               close (Output_Pipe);
               close ( Error_Pipe);
               return Output;
            end;
         else
            declare
               Error : constant String := Output_Of (Error_Pipe);
            begin
               close (Output_Pipe);
               close ( Error_Pipe);
               raise Command_Error with Error;
            end;
         end if;
      end;
   end Pipeline_Output;


   function Output_Of (Command_Line : in String) return String
   is
      use Ada.Strings.Fixed;
      The_Index   : constant Natural := Index (Command_Line, " | ");
      Is_Pipeline : constant Boolean := (if The_Index = 0 then True else False);
   begin
      if Is_Pipeline
      then
         declare
            The_Commands : Command_Array := To_Commands (Command_Line);
         begin
            return Pipeline_Output (The_Commands);
         end;
      else
         declare
            The_Command : Command := To_Command (Command_Line);
         begin
            return Command_Output (The_Command);
         end;
      end if;
   end Output_Of;


   procedure Run (Command_Line : in String)
   is
      Output : String := Output_Of (Command_Line) with Unreferenced;
   begin
      null;
   end Run;


   -- Command Results
   --

   function Results_Of (The_Command : in out Command) return Command_Results
   is
      Output_Pipe : constant Shell.Pipe    := To_Pipe;
      Error_Pipe  : constant Shell.Pipe    := To_Pipe;
      Process     :          Shell.Process;
   begin
      The_Command.Output_Pipe := Output_Pipe;
      The_Command. Error_Pipe :=  Error_Pipe;

      Process := Run (The_Command);
      Wait_On (Process);

      return (Ada.Finalization.Limited_Controlled with
                Output => new String' (Output_Of (Output_Pipe)),
                Errors => new String' (Output_Of (Error_Pipe)));
   end Results_Of;


   overriding
   procedure Finalize (Results : in out Command_Results)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (String, String_Access);
   begin
      Deallocate (Results.Output);
      Deallocate (Results.Errors);
   end Finalize;


   function Output_Of (The_Results : in Command_Results) return String
   is
   begin
      return The_Results.Output.all;
   end Output_Of;


   function Errors_Of (The_Results : in Command_Results) return String
   is
   begin
      return The_Results.Errors.all;
   end Errors_Of;


   -- Pipes
   --

   function To_Pipe return Pipe
   is
      The_Pipe : Pipe;
   begin
      POSIX.IO.Create_Pipe (Read_End  => The_Pipe.Read_End,
                            Write_End => The_Pipe.Write_End);
      return The_Pipe;
   end To_Pipe;


   function Output_Of (The_Pipe : in Pipe) return String
   is
      use POSIX;
      Max_Process_Output : constant := 20 * 1024;

      Buffer : Stream_Element_Array (1 .. Max_Process_Output);
      Last   : Stream_Element_Offset;
   begin
      IO.Read (File   => The_Pipe.Read_End,
               Buffer => Buffer,
               Last   => Last);
      return To_String (To_POSIX_String (Buffer (1 .. Last)));

   exception
      when Ada.IO_Exceptions.End_Error =>
         return "";
   end Output_Of;


   procedure Close (The_Pipe : in Pipe)
   is
      use POSIX.IO;
   begin
      if Is_Open (The_Pipe.Read_End) then
         Close (File => The_Pipe.Read_End);
      end if;

      if Is_Open (The_Pipe.Write_End) then
         Close (File => The_Pipe.Write_End);
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


   -- Pipe Streams
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


   -- Processes
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


   procedure Wait_On (Process : in Shell.Process)
   is
      use POSIX.Process_Primitives;

      Status : Termination_Status with Unreferenced;
   begin
      Wait_For_Child_Process (Status => Status,
                              Child  => Process.Id);
   end Wait_On;


   function Has_Terminated (Process : in Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives;

      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status,
                              Child  => Process.Id,
                              Block  => False);

      return Status_Available (Status);
   end Has_Terminated;


   function Normal_Exit (Process : in Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives;

      Status : Termination_Status;
   begin
      Wait_For_Child_Process (Status => Status,
                              Child  => Process.Id,
                              Block  => False);

      if not Status_Available (Status) then
         return False;
      end if;

      if Exit_Status_Of (Status) = POSIX.Process_Primitives.Normal_Exit then
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
