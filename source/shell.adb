with
     Ada.IO_Exceptions,
     Ada.Unchecked_Conversion,

     POSIX.Signals,
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


   function To_Stream (From : in String) return Data
   is
      subtype  My_String is String (From'Range);
      subtype  My_Data   is Data   (0 .. From'Length - 1);
      function Convert   is new Ada.Unchecked_Conversion (My_String, My_Data);
   begin
      return Convert (From);
   end To_Stream;


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


   procedure Kill  (Process : in Shell.Process)
   is
      use POSIX.Signals;
   begin
      Send_Signal (Process.Id, Signal_Kill);
   end Kill;

end Shell;
