with
     GNAT.OS_Lib,

     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Text_IO,
     Ada.Unchecked_Conversion;

package body Shell
is

   procedure log (Message : in String)
   is
   begin
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => Message);
   end log;


   -- Commands
   --

   function To_Arguments (All_Arguments : in String) return String_Array
   is
      use GNAT.OS_Lib;
      Command_Name  : constant String      := "Command_Name";     -- Argument_String_To_List expects the command name to be the 1st piece
                                                                  -- of the string, so we provide a dummy name.
      Arguments     : Argument_List_Access := Argument_String_To_List (  Command_Name
                                                                       & " "
                                                                       & All_Arguments);
      Result        : String_Array (1 .. Arguments'Length - 1);
   begin
      for i in Result'Range
      loop
         Result (i) := +Arguments (i + 1).all;
      end loop;

      Free (Arguments);
      return Result;
   end To_Arguments;


   function to_Command (Command_Line : in    String) return Command
   is
      use POSIX.Process_Primitives,
          Ada.Strings.Fixed;

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



   function To_Commands (Pipeline : in    String) return Command_Array
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
            Full_Command  : constant String := Trim (Pipeline (First .. Last),
                                                     Ada.Strings.Both);
         begin
            log ("'" & Full_Command & "'");

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



   function  Run (The_Command : in     Command;
                  Pipeline    : in     Boolean := False) return Process
   is
      use POSIX,
          POSIX.Process_Primitives,
          POSIX.Process_Identification;

      Name   : constant POSIX_String := To_POSIX_String (+The_Command.Name);
      Result :          Process;
   begin
      log ("Running command named '" & To_String (Name) & "'");

      Result := Start (Program   => +The_Command.Name,
                       Arguments =>  The_Command.Arguments,
                       Input     =>  The_Command.Input_Pipe,
                       Output    =>  The_Command.Output_Pipe,
                       Errors    =>  The_Command.Error_Pipe,
                       Pipeline  =>  Pipeline);
      return Result;
   end Run;



   procedure Run (The_Command : in     Command;
                  Pipeline    : in     Boolean := False)
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
               Close_Pipe_Write_Ends (Commands (I - 1));             -- Close ends for the prior command.
            end if;

            if I = Commands'Last
            then
               Close_Pipe_Write_Ends (Commands (Commands'Last));     -- Close ends for the final command.
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



   function Command_Output (The_Command : in     Command) return String
   is
      Command :          Shell.Command := The_Command;
      Pipe    : constant Shell.Pipe    := To_Pipe;
   begin
      Command.Output_Pipe := Pipe;
      Run (Command);

      -- TODO: Wait for command to complete.
      return To_String (Pipe);
   end Command_Output;



   -- Pipes
   --

   function to_Pipe return Pipe
   is
      The_Pipe : Pipe;
   begin
      POSIX.IO.Create_Pipe (Read_End  => The_Pipe.Read_End,
                            Write_End => The_Pipe.Write_End);
      return The_Pipe;
   end to_Pipe;


   function  To_String (The_Pipe : in     Pipe) return String
   is
      Max_Process_Output : constant := 20 * 1024;
      Buffer : POSIX.IO.IO_Buffer (1 .. Max_Process_Output);
      Last   : POSIX.IO_Count;
   begin
      POSIX.IO.Read (File   => The_Pipe.Read_End,
                     Buffer => Buffer,
                     Last   => Last);
      return POSIX.To_String (Buffer (1 .. Integer (Last)));
   end To_String;



   procedure Close (The_Pipe : in     Pipe)
   is
   begin
      POSIX.IO.Close (File => The_Pipe.Read_End);
      POSIX.IO.Close (File => The_Pipe.Write_End);
   end Close;



   -- Pipe Streams
   --

   overriding
   procedure Read  (Stream : in out Pipe_Stream;
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

   function Start (Program   : in     String;
                   Arguments : in     String_Array := Nil_Strings;
                   Input     : in     Pipe         := Standard_Input;
                   Output    : in     Pipe         := Standard_Output;
                   Errors    : in     Pipe         := Standard_Error;
                   Pipeline  : in     Boolean      := False) return Process
   is
      use POSIX,
          POSIX.Process_Primitives,
          Ada.Strings.Unbounded;

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
         Append (Args,  To_POSIX_String (+Arguments (I)));
      end loop;

      Start_Process_Search (The_Process_Id,
                            Name,
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



   function Image (Process : in Shell.Process) return String
   is
      use POSIX.Process_Identification;
   begin
      return Image (Process.Id);
   end Image;


end Shell;
