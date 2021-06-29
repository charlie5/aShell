with
     POSIX.Signals,
     POSIX.Process_Primitives.Extensions,
     POSIX.Event_Management,

     Ada.Characters.Handling,
     Ada.Exceptions,
     Ada.IO_Exceptions,
     Ada.Unchecked_Conversion,
     Ada.Text_IO;

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
      end Open;


      procedure Close (Pipe           : in Shell.Pipe;
                       Only_Write_End : in Boolean := False;
                       Only_Read_End  : in Boolean := False)
      is
         use POSIX.IO;
      begin
         if    not  Only_Write_End
           and then Pipe.Read_End /= Null_File_Descriptor
           and then Is_Open (Pipe.Read_End)
         then
            Close (File => Pipe.Read_End);
         end if;

         if    not  Only_Read_End
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


   procedure Close (Pipe           : in Shell.Pipe;
                    Only_Write_End : in Boolean := False;
                    Only_Read_End  : in Boolean := False)
   is
      use POSIX.IO;
   begin
      if    Only_Write_End
        and Only_Read_End
      then
         raise Program_Error with "Closing pipe: 'Only_Write_End' and 'Only_Read_End' options are mutually exclusive";
      end if;

      if    Pipe /= Standard_Input
        and Pipe /= Standard_Output
        and Pipe /= Standard_Error
      then
         Safe_Pipes.Close (Pipe, Only_Write_End, Only_Read_End);
      end if;

   end Close;


   function Image (Pipe : in Shell.Pipe) return String
   is
   begin
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
      return Is_Open (Pipe.Read_End);
   end Is_Readable;


   function Is_Writeable (Pipe : in Shell.Pipe) return Boolean
   is
      use POSIX.IO;
   begin
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
      use Ada.Exceptions,
          POSIX.IO;

      Max_Process_Output : constant := 200 * 1024;

      Buffer : Data (1 .. Max_Process_Output);
      Last   : Stream_Element_Offset := 0;

   begin
      if not Is_Readable (Pipe)
      then
         log ("In Output_Of: pipe not readable " & Image (Pipe));
         return No_Data;
      end if;

      if not Is_Empty (Pipe, Timeout => Duration'Small)
      then
         Read (File   => Pipe.Read_End,
               Buffer => Buffer,
               Last   => Last);
      end if;

      return Buffer (1 .. Last);

   exception
      when E : POSIX.POSIX_Error =>
         declare
            use Ada.Characters.Handling;
            Message : constant String := To_Upper (Exception_Message (E));
         begin
            if Message = "BAD_FILE_DESCRIPTOR"
            then
               return No_Data;
            end if;

            return No_Data;  -- raise No_Output_Error with Image (Current_Task) & " " & Message & " ~ pipe read end =>" & Pipe.Read_End'Image;
         end;

      when Ada.IO_Exceptions.End_Error =>
         return No_Data;
   end Output_Of;


   procedure Write_To (Pipe : in Shell.Pipe;   Input : in Data)
   is
   begin
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
      Safe_Pipes.Close (Pipe, Only_Write_End => True);
   end Close_Write_End;


   function Close_Write_End (Pipe : in Shell.Pipe) return Boolean
   is
   begin
      Safe_Pipes.Close (Pipe, Only_Write_End => True);
      return True;
   end Close_Write_End;


   --- Pipe Streams
   --

   function Stream (Pipe : in Shell.Pipe) return Pipe_Stream
   is
   begin
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
                   Working_Directory : in String  := ".";
                   Input             : in Pipe    := Standard_Input;
                   Output            : in Pipe    := Standard_Output;
                   Errors            : in Pipe    := Standard_Error;
                   Pipeline          : in Boolean := False) return Process
   is
      use POSIX,
          POSIX.Process_Primitives,
          POSIX.Process_Primitives.Extensions;

      The_Template   : Process_Template;
      The_Process    : Process;
      The_Process_Id : Process_Id;

      Args :          POSIX_String_List;
      Name : constant POSIX_String     := To_POSIX_String (Program);

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
      use POSIX.Process_Primitives,
          Ada.Characters.Handling,
          Ada.Exceptions,
          Ada.Text_IO;
   begin
      Wait_For_Child_Process (Status => Process.Status,
                              Child  => Process.Id,
                              Block  => False);

      return Status_Available (Process.Status);
   exception
      when E : POSIX.POSIX_Error =>
         if To_Upper (Exception_Message (E)) = "NO_CHILD_PROCESS"
         then
            Put_Line ("Child process is dead (" & Image (Process) & ")");
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
         return False;
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
      return Image (Process.Id);
   end Image;


   procedure Kill  (Process : in Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Kill);
   end Kill;


   procedure Interrupt (Process : in Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Interrupt);
   end Interrupt;


   procedure Pause (Process : in Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Stop);
   end Pause;


   procedure Resume (Process : in Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Continue);
   end Resume;



   --- Debugging
   --

   Log_File    : Ada.Text_IO.File_Type;
   Log_Enabled : Boolean := False;


   procedure Open_Log (Name : in String)
   is
      use Ada.Text_IO;
   begin
      if Log_Enabled
      then
         raise Program_Error with "Log is already open.";
      end if;

      Log_Enabled := True;
      Create (Log_File, Ada.Text_IO.Out_File, Name);
   end Open_Log;


   procedure Close_Log
   is
      use Ada.Text_IO;
   begin
      if not Log_Enabled
      then
         raise Program_Error with "Log has not been opened.";
      end if;

      Log_Enabled := False;
      Close (Log_File);
   end Close_Log;


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


   function Log (Message : in String) return Boolean
   is
   begin
      Log (Message);
      return True;
   end Log;


end Shell;
