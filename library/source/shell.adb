with
     POSIX.Signals,
     POSIX.Process_Primitives.Extensions,
     POSIX.Event_Management,

     Gnat.OS_Lib,

     Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Exceptions,
     Ada.IO_Exceptions,
     Ada.Unchecked_Conversion;

with Ada.Text_IO;
--  use  Ada.Text_IO;


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



   --- Data
   --

   function To_String (From : in Data) return String
   is
      subtype  My_Data   is Data   (From'Range);
      subtype  My_String is String (1 .. From'Length);
      function Convert   is new Ada.Unchecked_Conversion (My_Data, My_String);
   begin
      return Convert (From);
   end To_String;



   function To_Data (From : in String) return Data
   is
      subtype  My_String is String (From'Range);
      subtype  My_Data   is Data   (0 .. From'Length - 1);
      function Convert   is new Ada.Unchecked_Conversion (My_String, My_Data);
   begin
      return Convert (From);
   end To_Data;



   --- Pipes
   --

   protected body Safe_Pipes
   is
      procedure Open (Pipe : out Shell.Pipe)
      is
         use POSIX.IO;
      begin
         Create_Pipe (Read_End  => Pipe.Read_End,
                      Write_End => Pipe.Write_End);
      exception
         when POSIX.POSIX_Error =>
            raise Too_Many_Pipes_Error;
      end Open;



      procedure Close (Pipe           : in Shell.Pipe;
                       Only_Write_End : in Boolean := False;
                       Only_Read_End  : in Boolean := False)
      is
         use POSIX.IO;
      begin
         if         not Only_Write_End
           and then Pipe.Read_End /= Null_File_Descriptor
           and then Is_Open (Pipe.Read_End)
         then
            Close (File => Pipe.Read_End);
         end if;

         if         not Only_Read_End
           and then Pipe.Write_End /= Null_File_Descriptor
           and then Is_Open (Pipe.Write_End)
         then
            Close (File => Pipe.Write_End);
         end if;
      end Close;

   end Safe_Pipes;



   function To_Pipe (Blocking : in Boolean := True) return Pipe
   is
      use POSIX.IO;
      The_Pipe : Pipe;
   begin
      Safe_Pipes.Open (The_Pipe);

      if not Blocking
      then
         Set_File_Control (The_Pipe.Read_End,
                           Non_Blocking);
      end if;

      return The_Pipe;
   end To_Pipe;



   procedure Check_Not_Null (Pipe : in Shell.Pipe)
   is
   begin
      if Pipe = Null_Pipe
      then
         raise Null_Pipe_Error;
      end if;
   end Check_Not_Null;



   procedure Close (Pipe           : in Shell.Pipe;
                    Only_Write_End : in Boolean := False;
                    Only_Read_End  : in Boolean := False)
   is
      use POSIX.IO;
   begin
      Check_Not_Null (Pipe);

      if    Only_Write_End
        and Only_Read_End
      then
         raise Pipe_Error with "When closing a pipe, the 'Only_Write_End' and 'Only_Read_End' options are mutually exclusive.";
      end if;

      if    Pipe /= Standard_Input
        and Pipe /= Standard_Output
        and Pipe /= Standard_Error
      then
         Safe_Pipes.Close (Pipe, Only_Write_End, Only_Read_End);     -- TODO: Should 'write end' and 'read end' be set to Null_File_Descriptor when closed ?
      end if;

   end Close;



   function Image (Pipe : in Shell.Pipe) return String
   is
   begin
      if Pipe = Null_Pipe
      then
         return "Null_Pipe";
      end if;

      if Pipe = Standard_Input
      then
         return "Standard_Input";
      end if;

      if Pipe = Standard_Output
      then
         return "Standard_Output";
      end if;

      if Pipe = Standard_Error
      then
         return "Standard_Error";
      end if;

      return "(Write_End =>"
           & Pipe.Write_End'Image
           & ", Read_End =>"
           & Pipe.Read_End'Image
           & ")";
   end Image;



   function Is_Readable (Pipe : in Shell.Pipe) return Boolean
   is
      use POSIX.IO;
   begin
      Check_Not_Null (Pipe);
      return Is_Open (Pipe.Read_End);
   end Is_Readable;



   function Is_Writeable (Pipe : in Shell.Pipe) return Boolean
   is
      use POSIX.IO;
   begin
      Check_Not_Null (Pipe);
      return Is_Open (Pipe.Write_End);
   end Is_Writeable;



   function Is_Empty (Pipe    : in Shell.Pipe;
                      Timeout : in Duration  := 0.0) return Boolean
   is
      use POSIX.Event_Management,
          POSIX.IO;
      FDS_R : File_Descriptor_Set;
      FDS_W : File_Descriptor_Set;
      FDS_E : File_Descriptor_Set;
      Count : Natural;
   begin
      Check_Not_Null (Pipe);

      Make_Empty (FDS_R);
      Make_Empty (FDS_W);
      Make_Empty (FDS_E);

      add (FDS_R, Pipe.Read_End);

      Select_File (Read_Files     => FDS_R,
                   Write_Files    => FDS_W,
                   Except_Files   => FDS_E,
                   Files_Selected => Count,
                   Timeout        => Timeout);

      return Count = 0;
   end Is_Empty;



   function Output_Of (Pipe : in Shell.Pipe) return Data
   is
      use POSIX.IO;

      Max_Process_Output : constant := 200 * 1024;

      Buffer : Data (1 .. Max_Process_Output);
      Last   : Stream_Element_Offset := 0;

   begin
      if not Is_Readable (Pipe)
      then
         log ("In Output_Of: pipe not readable " & Image (Pipe));
         return No_Data;
      end if;

      if not Is_Empty (Pipe, Timeout => 0.01)
      then
         Read (File   => Pipe.Read_End,
               Buffer => Buffer,
               Last   => Last);
      end if;

      return Buffer (1 .. Last);

   exception
      when Ada.IO_Exceptions.End_Error =>
         return No_Data;
   end Output_Of;



   procedure Write_To (Pipe : in Shell.Pipe;   Input : in Data)
   is
   begin
      Check_Not_Null (Pipe);

      if Input'Length > 0
      then
         declare
            subtype   My_Data is Data (Input'Range);
            procedure Write   is new POSIX.IO.Generic_Write (My_Data);
         begin
            Write (Pipe.Write_End, Input);
         end;
      end if;
   end Write_To;



   procedure Close_Write_End (Pipe : in Shell.Pipe)
   is
   begin
      Check_Not_Null   (Pipe);
      Safe_Pipes.Close (Pipe, Only_Write_End => True);
   end Close_Write_End;



   function Close_Write_End (Pipe : in Shell.Pipe) return Boolean
   is
   begin
      Check_Not_Null   (Pipe);
      Safe_Pipes.Close (Pipe, Only_Write_End => True);
      return True;
   end Close_Write_End;



   --- Pipe Streams
   --

   function Stream (Pipe : in Shell.Pipe) return Pipe_Stream
   is
   begin
      Check_Not_Null (Pipe);
      return (Root_Stream_Type with Pipe => Pipe);
   end Stream;



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
   begin
      POSIX.IO.Write (File   => Stream.Pipe.Write_End,
                      Buffer => Item,
                      Last   => Last);
   end Write;



   --- Processes
   --

   procedure Update_Status (Process      : in out Shell.Process;
                            POSIX_Status : in     POSIX.Process_Primitives.Termination_Status)
   is
      use POSIX.Process_Primitives;
   begin
      if Status_Available (POSIX_Status)     -- A state change has occurred.
      then
         Process.Status := POSIX_Status;

         declare
            Cause : constant Termination_Cause := Termination_Cause_Of (POSIX_Status);
         begin
            case Process.State
            is
            when Not_Started =>
               raise Program_Error with "Shell.Update_Status: Status is available for unstarted process.";


            when Running =>
               case Cause
               is
                  when Exited =>
                     if Exit_Status_Of (POSIX_Status) = POSIX.Process_Primitives.Normal_Exit
                     then
                        Process.State := Normal_Exit;
                     else
                        Process.State := Failed_Exit;
                     end if;

                  when Terminated_By_Signal =>
                     declare
                        use POSIX.Signals;
                        Signal : constant POSIX.Signals.Signal := Termination_Signal_Of (POSIX_Status);
                     begin
                        if    Signal = Signal_Interrupt then Process.State := Interrupted;
                        elsif Signal = Signal_Kill      then Process.State := Killed;
                        else
                           raise Program_Error with "Shell.Update_Status: Unhandled termination signal (" & Signal'Image & ") while running.";
                        end if;
                     end;

                  when Stopped_By_Signal =>
                     declare
                        use POSIX.Signals;
                        Signal : constant POSIX.Signals.Signal := Stopping_Signal_Of (POSIX_Status);
                     begin
                        if Signal = Signal_Stop
                        --  if Signal = Signal_Terminal_Stop
                        then
                           Process.State := Paused;
                        else
                           raise Program_Error with "Shell.Update_Status: Unhandled stopping signal (" & Signal'Image & ") while running.";
                        end if;
                     end;
               end case;


            when Paused =>
               case Cause
               is
                  when Exited =>
                     raise Program_Error with "Shell.Update_Status: Paused process has exited.";

                  when Terminated_By_Signal =>
                     declare
                        use POSIX.Signals;
                        Signal : constant POSIX.Signals.Signal := Termination_Signal_Of (POSIX_Status);
                     begin
                        if    Signal = Signal_Interrupt then Process.State := Interrupted;
                        elsif Signal = Signal_Kill      then Process.State := Killed;
                        else
                           raise Program_Error with "Shell.Update_Status: Unhandled termination signal (" & Signal'Image &") while paused.";
                        end if;
                     end;

                  when Stopped_By_Signal =>
                     declare
                        use POSIX.Signals;
                        Signal : constant POSIX.Signals.Signal := Stopping_Signal_Of (POSIX_Status);
                     begin
                        raise Program_Error with "Shell.Update_Status: Unhandled stopping signal (" & Signal'Image &") while paused.";
                     end;
               end case;

            -- The following cases should never occur.
            --
            when Normal_Exit =>
               raise Program_Error with "Shell.Update_Status: Process has already exited normally.";


            when Failed_Exit =>
               raise Program_Error with "Shell.Update_Status: Process has already exited due to failure.";


            when Interrupted =>
               raise Program_Error with "Shell.Update_Status: Process has already been interrupted.";


            when Killed =>
               raise Program_Error with "Shell.Update_Status: Process has already been killed.";
            end case;
         end;
      end if;
   end Update_Status;



   function Status (Process : in out Shell.Process) return Process_State
   is
      use POSIX.Process_Primitives;

      POSIX_Status : POSIX.Process_Primitives.Termination_Status;
   begin
      if Process.State not in Not_Started | Terminated
      then
         Wait_For_Child_Process (Status => POSIX_Status,
                                 Child  => Process.Id,
                                 Block  => False);

         Update_Status (Process, POSIX_Status);
      end if;

      return Process.State;
   end Status;



   function Start (Program           : in String;
                   Arguments         : in String_Array;
                   Working_Directory : in String  := ".";
                   Input             : in Pipe    := Standard_Input;
                   Output            : in Pipe    := Standard_Output;
                   Errors            : in Pipe    := Standard_Error;
                   Pipeline          : in Boolean := False) return Process
   is
      use POSIX,
          POSIX.Process_Primitives,
          POSIX.Process_Primitives.Extensions,
          Gnat.OS_Lib;

      The_Template   : Process_Template;
      The_Process    : Process;
      The_Process_Id : Process_Id;

      Args     :          POSIX_String_List;
      Name     : constant POSIX_String     := To_POSIX_String (Program);
      Pathname :          String_Access    := Locate_Exec_On_Path (Program);
   begin
      if Pathname = null
      then
         raise Process_Error with "Program '" & Program & "' not found on PATH";
      else
         Free (Pathname);
      end if;

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

      for i in Arguments'Range
      loop
         Append (Args, To_POSIX_String (+Arguments (i)));
      end loop;


      Start_Process:
      begin
         Start_Process_Search (The_Process_Id,
                               Name,
                               Working_Directory,
                               The_Template,
                               Args);
      exception
         when E : POSIX.POSIX_Error =>
            Close_Template (The_Template);

            if Ada.Exceptions.Exception_Message (E) = "RESOURCE_TEMPORARILY_UNAVAILABLE"
            then
               raise Too_Many_Processes_Error;
            else
               raise;
            end if;

         when others =>
            Close_Template (The_Template);
            raise;
      end Start_Process;


      Close_Template (The_Template);
      Make_Empty     (Args);

      if Input /= Standard_Input
      then
         Safe_Pipes.Close (Input, Only_Read_End => True);
      end if;

      -- When in a pipeline of processes, the write ends of The_Process's 'Output' & 'Errors' pipes must remain open, in
      -- the main process, until the next process in the pipeline (which uses the pipe as 'Input') is started (spawned).
      --
      if not Pipeline
      then
         if Output /= Standard_Output
         then
            Safe_Pipes.Close (Output, Only_Write_End => True);
         end if;

         if Errors /= Standard_Error
         then
            Safe_Pipes.Close (Errors, Only_Write_End => True);
         end if;
      end if;

      The_Process.Id    := The_Process_Id;
      The_Process.State := Running;

      return The_Process;
   end Start;



   function Start (Command           : in String;
                   Working_Directory : in String  := ".";
                   Input             : in Pipe    := Standard_Input;
                   Output            : in Pipe    := Standard_Output;
                   Errors            : in Pipe    := Standard_Error;
                   Pipeline          : in Boolean := False) return Process
   is
      use Ada.Strings.Fixed,
          Gnat.OS_Lib;

      I        : constant Natural := Index (Command, " ");
      Program  : constant String  := (if I = 0 then Command else Command (Command'First .. I - 1));
      Pathname : String_Access    := Locate_Exec_On_Path (Program);
   begin
      if Pathname = null
      then
         raise Process_Error with "Program '" & Program & "' not found on PATH";
      else
         Free (Pathname);
      end if;

      return Start (Program           => "/bin/sh",
                    Arguments         => (+"-c",
                                          +Command),
                    Working_Directory => Working_Directory,
                    Input             => Input,
                    Output            => Output,
                    Errors            => Errors,
                    Pipeline          => Pipeline);
   end Start;



   procedure Start (Process           : in out Shell.Process;
                    Program           : in     String;
                    Arguments         : in     String_Array;
                    Working_Directory : in     String  := ".";
                    Input             : in     Pipe    := Standard_Input;
                    Output            : in     Pipe    := Standard_Output;
                    Errors            : in     Pipe    := Standard_Error;
                    Pipeline          : in     Boolean := False)
   is
   begin
      if Process.State /= Not_Started
      then
         raise Process_Already_Started;
      end if;

      Process := Start (Program, Arguments, Working_Directory, Input, Output, Errors, Pipeline);
   end Start;



   procedure Start (Process           : in out Shell.Process;
                    Command           : in     String;
                    Working_Directory : in     String  := ".";
                    Input             : in     Pipe    := Standard_Input;
                    Output            : in     Pipe    := Standard_Output;
                    Errors            : in     Pipe    := Standard_Error;
                    Pipeline          : in     Boolean := False)
   is
   begin
      if Process.State /= Not_Started
      then
         raise Process_Already_Started;
      end if;

      Process := Start (Command, Working_Directory, Input, Output, Errors, Pipeline);
   end Start;



   procedure Wait_On (Process : in out Shell.Process)
   is
      use POSIX.Process_Primitives;
      POSIX_Status : POSIX.Process_Primitives.Termination_Status;
   begin
      Wait_For_Child_Process (Status => POSIX_Status,
                              Child  => Process.Id,
                              Block  => True);

      Update_Status (Process, POSIX_Status);

   end Wait_On;



   function Has_Terminated (Process : in out Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives,
          Ada.Characters.Handling,
          Ada.Exceptions;
   begin
      Wait_For_Child_Process (Status => Process.Status,
                              Child  => Process.Id,
                              Block  => False);
      return Status_Available (Process.Status);
   exception
      when E : POSIX.POSIX_Error =>
         if To_Upper (Exception_Message (E)) = "NO_CHILD_PROCESS"
         then
            Log ("Has_Terminated ~ Child process is already dead (" & Image (Process) & ")");
            return True;
         else
            raise;
         end if;
   end Has_Terminated;



   function Normal_Exit (Process : in Shell.Process) return Boolean
   is
      use POSIX.Process_Primitives;
   begin
      if not Status_Available (Process.Status)
      then
         return False;     -- TODO: Should this raise an exception ?
      end if;

      if Exit_Status_Of (Process.Status) = POSIX.Process_Primitives.Normal_Exit
      then
         return True;
      end if;

      return False;
   end Normal_Exit;



   function Image (Process : in Shell.Process) return String
   is
      use POSIX.Process_Identification;
   begin
      return
        Image (Process.Id)
        & " "
        & Process.State'Image;
   end Image;



   procedure Kill (Process : in out Shell.Process)
   is
      use POSIX.Signals;

      the_Status : Process_State := Status (Process);
   begin
      Send_Signal (Process.Id, Signal_Kill);

      while the_Status /= Killed
      loop
         delay Duration'Small;
         the_Status := Status (Process);
      end loop;
   end Kill;



   procedure Interrupt (Process : in out Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Interrupt);

      while Status (Process) /= Interrupted
      loop
         delay Duration'Small;
      end loop;
   end Interrupt;



   procedure Pause (Process : in out Shell.Process)
   is
      use POSIX.Signals;
   begin
      case Process.State
      is
         when Not_Started =>
            raise Process_Not_Started;

         when Running =>
            Send_Signal (Process.Id, Signal_Stop);

            while Status (Process) /= Paused
            loop
               delay Duration'Small;
            end loop;

         when Paused =>
            raise Process_Already_Paused;

         when Normal_Exit
            | Failed_Exit
            | Interrupted
            | Killed =>
            raise Process_Has_Terminated with "Status => " & Process.State'Image;

      end case;

   end Pause;



   procedure Resume (Process : in out Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Continue);
      Process.State := Running;
   end Resume;



   --- Debugging
   --

   protected Logger
   is
      procedure Open  (Name : in String);
      procedure Close;

      procedure Log (Message : in String);
   private
      Log_File    : Ada.Text_IO.File_Type;
      Log_Enabled : Boolean := False;
   end Logger;



   protected body Logger
   is
      procedure Open (Name : in String)
      is
         use Ada.Text_IO;
      begin
         if Log_Enabled
         then
            raise Program_Error with "Log is already open.";
         end if;

         Log_Enabled := True;
         Create (Log_File, Out_File, Name);
      end Open;



      procedure Close
      is
         use Ada.Text_IO;
      begin
         if not Log_Enabled
         then
            raise Program_Error with "Log has not been opened.";
         end if;

         Log_Enabled := False;
         Close (Log_File);
      exception
         when Device_Error =>
           null;
      end Close;



      procedure Log (Message : in String)
      is
         use Ada.Text_IO;
      begin
         if Log_Enabled
         then
            Put_Line (Log_File, Message);
            Flush (Log_File);
         end if;
      end Log;

   end Logger;



   procedure Open_Log (Name : in String)
   is
   begin
      Logger.Open (Name);
   end Open_Log;



   procedure Close_Log
   is
   begin
      Logger.Close;
   end Close_Log;



   procedure Log (Message : in String)
   is
   begin
      Logger.Log (Message);
   end Log;



   function Log (Message : in String) return Boolean
   is
   begin
      Log (Message);
      return True;
   end Log;


end Shell;
