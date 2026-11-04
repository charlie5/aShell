with
     POSIX.Signals,
     POSIX.Process_Primitives.Extensions,
     POSIX.Event_Management,

     Gnat.OS_Lib,

     Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Exceptions,
     Ada.IO_Exceptions,
     Ada.Task_Identification,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation;

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

         -- Otherwise each program started would inherit every other open pipe, and a reader could
         -- never see the end of its input, nor a writer that its reader has gone. A pipe which is
         -- given to a program is duplicated onto the program's standard descriptors, which do stay open.
         --
         POSIX.IO.Set_Close_On_Exec (Pipe.Read_End);
         POSIX.IO.Set_Close_On_Exec (Pipe.Write_End);

         Pipe.Id := Next_Id;
         Next_Id := Next_Id + 1;

         Owners.Include (Pipe.Read_End,  Pipe.Id);
         Owners.Include (Pipe.Write_End, Pipe.Id);
      exception
         when POSIX.POSIX_Error =>
            raise Too_Many_Pipes_Error;
      end Open;



      procedure Close (Pipe           : in Shell.Pipe;
                       Only_Write_End : in Boolean := False;
                       Only_Read_End  : in Boolean := False)
      is
         use POSIX.IO;

         procedure Close_End (Descriptor : in File_Descriptor)
         is
         begin
            if Owns (Pipe, Descriptor)
            then
               Owners.Delete (Descriptor);

               if Is_Open (Descriptor)
               then
                  Close (File => Descriptor);
               end if;

               if         Descriptor = Pipe.Write_End
                 and then Writers.Contains (Pipe.Id)
               then
                  Writers.Element (Pipe.Id).all;     -- It has more to do, now that the write end has closed.
               end if;
            end if;
         end Close_End;

      begin
         if not Only_Write_End
         then
            Close_End (Pipe.Read_End);
         end if;

         if not Only_Read_End
         then
            Close_End (Pipe.Write_End);
         end if;
      end Close;



      procedure Duplicate_Write_End (Pipe       : in     Shell.Pipe;
                                     Descriptor :    out File_Descriptor;
                                     Is_Open    :    out Boolean)
      is
      begin
         Descriptor := Null_File_Descriptor;
         Is_Open    := Owns (Pipe, Pipe.Write_End);

         if Is_Open
         then
            Descriptor := POSIX.IO.Duplicate (Pipe.Write_End);
            POSIX.IO.Set_Close_On_Exec (Descriptor);
         end if;
      end Duplicate_Write_End;



      procedure Note_Writer (Pipe : in Shell.Pipe;
                             Wake : in Wake_Action)
      is
      begin
         Writers.Include (Pipe.Id, Wake);
      end Note_Writer;



      procedure Forget_Writer (Pipe : in Shell.Pipe)
      is
      begin
         Writers.Exclude (Pipe.Id);
      end Forget_Writer;



      function Owns (Pipe       : in Shell.Pipe;
                     Descriptor : in File_Descriptor) return Boolean
      is
         use Descriptor_Owner_Maps;
         Owner : constant Cursor := Owners.Find (Descriptor);
      begin
         return     Pipe.Id /= No_Pipe_Id
           and then Has_Element (Owner)
           and then Element (Owner) = Pipe.Id;
      end Owns;

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
         Safe_Pipes.Close (Pipe, Only_Write_End, Only_Read_End);
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



   function End_Is_Open (Pipe       : in Shell.Pipe;
                         Descriptor : in File_Descriptor) return Boolean
   is (if Pipe.Id = No_Pipe_Id then POSIX.IO.Is_Open (Descriptor)
                               else Safe_Pipes.Owns (Pipe, Descriptor));



   function Is_Readable (Pipe : in Shell.Pipe) return Boolean
   is
   begin
      Check_Not_Null (Pipe);
      return End_Is_Open (Pipe, Pipe.Read_End);
   end Is_Readable;



   function Is_Writeable (Pipe : in Shell.Pipe) return Boolean
   is
   begin
      Check_Not_Null (Pipe);
      return End_Is_Open (Pipe, Pipe.Write_End);
   end Is_Writeable;



   procedure Check_Is_Readable (Pipe : in Shell.Pipe)
   is
   begin
      if not Is_Readable (Pipe)
      then
         raise Pipe_Not_Readable with "The read end of pipe " & Image (Pipe) & " is closed.";
      end if;
   end Check_Is_Readable;



   procedure Check_Is_Writeable (Pipe : in Shell.Pipe)
   is
   begin
      if not Is_Writeable (Pipe)
      then
         raise Pipe_Error with "The write end of pipe " & Image (Pipe) & " is closed.";
      end if;
   end Check_Is_Writeable;



   Max_Process_Output : constant := 200 * 1024;


   -- 'Poll' rather than 'select', which can't handle a descriptor numbered 1024 or more.
   --
   function Any_Are_Ready (Files   : in out POSIX.Event_Management.Poll_FD_Array;
                           Timeout : in     Duration) return Boolean
   is
      use type POSIX.Error_Code;
      Count : Natural;
   begin
      POSIX.Event_Management.Poll (Files,
                                   Response_Count => Count,
                                   Timeout        => Timeout);
      return Count /= 0;
   exception
      when POSIX.POSIX_Error =>
         if POSIX.Get_Error_Code /= POSIX.Interrupted_Operation
         then
            raise;
         end if;

         return False;
   end Any_Are_Ready;



   function Is_Empty (Pipe    : in Shell.Pipe;
                      Timeout : in Duration  := 0.0) return Boolean
   is
      use POSIX.Event_Management;
      Files : Poll_FD_Array (1 .. 1);
   begin
      Check_Is_Readable (Pipe);

      Set_File   (Files (1), Pipe.Read_End);
      Set_Events (Files (1), Read_Not_High);

      return not Any_Are_Ready (Files, Timeout);
   end Is_Empty;



   function Output_Of (Pipe : in Shell.Pipe) return Data
   is
      use POSIX.IO;

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



   procedure Wait_For_Output (Pipes   : in Pipe_Array;
                              Timeout : in Duration)
   is
      use POSIX.Event_Management;

      Files : Poll_FD_Array (1 .. Pipes'Length);
      Last  : Natural := 0;
   begin
      for Each of Pipes
      loop
         if         Each /= Null_Pipe
           and then Is_Readable (Each)
         then
            Last := Last + 1;

            Set_File   (Files (Last), Each.Read_End);
            Set_Events (Files (Last), Read_Not_High);
         end if;
      end loop;

      if Last = 0
      then
         delay Timeout;
         return;
      end if;

      declare
         Unused : constant Boolean := Any_Are_Ready (Files (1 .. Last), Timeout);
      begin
         null;
      end;
   end Wait_For_Output;



   function Available_Output_Of (Pipe : in Shell.Pipe) return Data
   is
      use type POSIX.Error_Code;

      No_Wait : constant Duration := 0.000_001;     -- A timeout of '0.0' would block.
   begin
      if         Pipe = Null_Pipe
        or else not Is_Readable (Pipe)
        or else Is_Empty (Pipe, Timeout => No_Wait)
      then
         return No_Data;
      end if;

      declare
         Buffer : Data (1 .. Max_Process_Output);
         Last   : Stream_Element_Offset;
      begin
         POSIX.IO.Read (File   => Pipe.Read_End,
                        Buffer => Buffer,
                        Last   => Last);

         return Buffer (1 .. Last);
      end;

   exception
      when Ada.IO_Exceptions.End_Error =>
         Safe_Pipes.Close (Pipe, Only_Read_End => True);
         return No_Data;

      when POSIX.POSIX_Error =>
         if POSIX.Get_Error_Code /= POSIX.Resource_Temporarily_Unavailable
         then
            raise;
         end if;

         return No_Data;
   end Available_Output_Of;



   procedure Write_To (Pipe : in Shell.Pipe;   Input : in Data)
   is
   begin
      Check_Not_Null (Pipe);

      if Input'Length > 0
      then
         Check_Is_Writeable (Pipe);

         declare
            subtype   My_Data is Data (Input'Range);
            procedure Write   is new POSIX.IO.Generic_Write (My_Data);
         begin
            Write (Pipe.Write_End, Input);
         end;
      end if;
   end Write_To;



   type Data_Access is access Data;
   procedure Free is new Ada.Unchecked_Deallocation (Data, Data_Access);

   package Data_Access_Vectors is new Ada.Containers.Vectors (Positive, Data_Access);


   protected type Input_Queue
   is
      procedure Add  (Input    : in     Data_Access;
                      Accepted :    out Boolean);     -- False once shut, as the input would never be written.
      procedure Take (Input    :    out Data_Access);     -- Gives null when the queue is empty.
      procedure Shut;
      procedure Wake;
      entry     Wait_For_Work;                            -- Until there is input, or a call of 'Wake'.
   private
      Items   : Data_Access_Vectors.Vector;
      Is_Shut : Boolean := False;
      Is_Woken : Boolean := False;
   end Input_Queue;

   type Input_Queue_Access is access Input_Queue;
   procedure Free is new Ada.Unchecked_Deallocation (Input_Queue, Input_Queue_Access);


   protected body Input_Queue
   is
      procedure Add (Input    : in     Data_Access;
                     Accepted :    out Boolean)
      is
      begin
         Accepted := not Is_Shut;

         if Accepted
         then
            Items.Append (Input);
         end if;
      end Add;


      procedure Shut
      is
      begin
         Is_Shut := True;
      end Shut;


      procedure Take (Input : out Data_Access)
      is
      begin
         if Items.Is_Empty
         then
            Input := null;
         else
            Input := Items.First_Element;
            Items.Delete_First;
         end if;
      end Take;


      procedure Wake
      is
      begin
         Is_Woken := True;
      end Wake;


      entry Wait_For_Work
        when not Items.Is_Empty
          or Is_Woken
      is
      begin
         Is_Woken := False;
      end Wait_For_Work;
   end Input_Queue;



   task type Input_Writer
   is
      entry Start (Pipe       : in Shell.Pipe;
                   Descriptor : in File_Descriptor;
                   Queue      : in Input_Queue_Access);
   end Input_Writer;


   task body Input_Writer
   is
      use POSIX,
          POSIX.Event_Management,
          POSIX.IO;

      -- A descriptor made by duplicating shares its options with the original, so the pipe's write end can't be made
      -- non-blocking without affecting other writers. Instead, a pipe which is ready always has room for this much.
      --
      Most_Per_Write : constant := 4096;

      The_Pipe       : Shell.Pipe;
      The_Descriptor : File_Descriptor;
      The_Queue      : Input_Queue_Access;

      The_Input      : Data_Access;
      Next           : Data_Offset := 1;

      Nothing_Will_Read : exception;


      function Main_Has_Completed return Boolean
      is (not Ada.Task_Identification.Is_Callable (Ada.Task_Identification.Environment_Task));


      procedure Write_Next_Part
      is
         Files : Poll_FD_Array (1 .. 1);
         Last  : Data_Offset;
      begin
         Set_File   (Files (1), The_Descriptor);
         Set_Events (Files (1), Write_Normal);

         if Any_Are_Ready (Files, Timeout => 1.0)
         then
            Write (File   => The_Descriptor,
                   Buffer => The_Input (Next .. Data_Offset'Min (Next + Most_Per_Write - 1,
                                                                 The_Input'Last)),
                   Last   => Last);
            Next := Last + 1;

         elsif Main_Has_Completed
         then
            raise Nothing_Will_Read;
         end if;

      exception
         when POSIX_Error =>
            if Get_Error_Code /= Interrupted_Operation
            then
               raise;
            end if;
      end Write_Next_Part;


      procedure Finish
      is
      begin
         The_Queue.Shut;     -- Any input arriving from now on is refused, rather than lost.
         Safe_Pipes.Forget_Writer (The_Pipe);
         Free (The_Input);

         loop
            The_Queue.Take (The_Input);
            exit when The_Input = null;
            Free (The_Input);
         end loop;

         if Is_Open (The_Descriptor)
         then
            Close (The_Descriptor);
         end if;

         Safe_Pipes.Close (The_Pipe, Only_Write_End => True);     -- Lets a later 'Write_In_Background' know the reader has gone.
      end Finish;

   begin
      accept Start (Pipe       : in Shell.Pipe;
                    Descriptor : in File_Descriptor;
                    Queue      : in Input_Queue_Access)
      do
         The_Pipe       := Pipe;
         The_Descriptor := Descriptor;
         The_Queue      := Queue;
      end Start;

      Writing:
      loop
         if The_Input = null
         then
            declare
               Closed : constant Boolean := not Is_Writeable (The_Pipe);     -- Check before taking, as input is queued before the close.
            begin
               The_Queue.Take (The_Input);

               if The_Input = null
               then
                  exit Writing when Closed or Main_Has_Completed;

                  select
                     The_Queue.Wait_For_Work;
                  or
                     delay 0.1;     -- To notice the main program completing.
                  end select;
               else
                  Next := The_Input'First;
               end if;
            end;

         else
            Write_Next_Part;

            if Next > The_Input'Last
            then
               Free (The_Input);
            end if;
         end if;
      end loop Writing;

      Finish;

   exception
      when Nothing_Will_Read =>
         Finish;

      when POSIX_Error =>
         if Get_Error_Code /= Broken_Pipe     -- The reader has gone, so the rest of the input isn't wanted.
         then
            Log ("Input writer failed: " & Image (Get_Error_Code));
         end if;

         Finish;

      when E : others =>
         Log ("Input writer failed: " & Ada.Exceptions.Exception_Information (E));
         Finish;
   end Input_Writer;


   type Input_Writer_Access is access Input_Writer;
   procedure Free is new Ada.Unchecked_Deallocation (Input_Writer, Input_Writer_Access);

   type Background_Writer is
      record
         Pipe_Of : Pipe_Id;
         Writer  : Input_Writer_Access;
         Queue   : Input_Queue_Access;
      end record;

   package Background_Writer_Vectors is new Ada.Containers.Vectors (Positive, Background_Writer);

   Background_Writers : Background_Writer_Vectors.Vector;



   procedure Write_In_Background (Pipe  : in Shell.Pipe;
                                  Input : in Data)
   is
      The_Queue : Input_Queue_Access;
      The_Input : Data_Access;
      Accepted  : Boolean;

      Write_End_Is_Closed : constant String := "The write end of pipe " & Image (Pipe) & " is closed.";
   begin
      Check_Not_Null (Pipe);

      if Input'Length = 0
      then
         return;
      end if;

      Check_Is_Writeable (Pipe);

      for i in reverse 1 .. Background_Writers.Last_Index
      loop
         declare
            Each : Background_Writer := Background_Writers.Element (i);
         begin
            if Each.Writer'Terminated     -- Rid finished writers.
            then
               Free (Each.Writer);
               Free (Each.Queue);
               Background_Writers.Delete (i);

            elsif Each.Pipe_Of = Pipe.Id
            then
               The_Queue := Each.Queue;
            end if;
         end;
      end loop;

      if The_Queue = null
      then
         -- The writer has its own descriptor, which nothing else can close. So the reader
         -- will see the end of its input only once the writer has finished with it.
         --
         declare
            Descriptor : File_Descriptor;
            Is_Open    : Boolean;
         begin
            Safe_Pipes.Duplicate_Write_End (Pipe, Descriptor, Is_Open);     -- An earlier writer may have just closed it.

            if not Is_Open
            then
               raise Pipe_Error with Write_End_Is_Closed;
            end if;

            declare
               New_Writer : constant Background_Writer := (Pipe_Of => Pipe.Id,
                                                           Writer  => new Input_Writer,
                                                           Queue   => new Input_Queue);
            begin
               Safe_Pipes.Note_Writer (Pipe, New_Writer.Queue.Wake'Access);

               The_Input := new Data'(Input);

               New_Writer.Queue.Add (The_Input, Accepted);
               New_Writer.Writer.Start (Pipe, Descriptor, New_Writer.Queue);
               Background_Writers.Append (New_Writer);
            end;
         end;
      else
         The_Input := new Data'(Input);
         The_Queue.Add (The_Input, Accepted);

         if not Accepted
         then
            Free (The_Input);
            raise Pipe_Error with Write_End_Is_Closed;
         end if;
      end if;
   end Write_In_Background;



   function Move_Standard_Input return Pipe
   is
      Moved : constant File_Descriptor := POSIX.IO.Duplicate (POSIX.IO.Standard_Input);
      Empty : constant File_Descriptor := POSIX.IO.Open      ("/dev/null", POSIX.IO.Read_Only);
   begin
      POSIX.IO.Set_Close_On_Exec (Moved);

      declare
         Unused : constant File_Descriptor := POSIX.IO.Duplicate_And_Close (Empty, Target => POSIX.IO.Standard_Input);
      begin
         POSIX.IO.Close (Empty);
      end;

      return (Write_End => Null_File_Descriptor,
              Read_End  => Moved,
              Id        => No_Pipe_Id);
   end Move_Standard_Input;



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
      use type POSIX.Error_Code;
   begin
      Check_Is_Readable (Stream.Pipe);

      Last := Item'First - 1;

      -- A single read may return only part of the item, or nothing at all from a non-blocking pipe,
      -- when the writer is yet to catch up. Stream attributes treat a short read as the end of the stream.
      --
      while Last < Item'Last
      loop
         begin
            POSIX.IO.Read (File   => Stream.Pipe.Read_End,
                           Buffer => Item (Last + 1 .. Item'Last),
                           Last   => Last);
         exception
            when Ada.IO_Exceptions.End_Error =>
               if Last < Item'First
               then
                  raise;
               end if;

               return;     -- The next read will raise End_Error.

            when POSIX.POSIX_Error =>
               if POSIX.Get_Error_Code not in POSIX.Resource_Temporarily_Unavailable
                                            | POSIX.Interrupted_Operation
               then
                  raise;
               end if;

               declare
                  Unused : constant Boolean := Is_Empty (Stream.Pipe, Timeout => 0.01);
               begin
                  null;
               end;
         end;
      end loop;
   end Read;



   overriding
   procedure Write (Stream : in out Pipe_Stream;
                    Item   : in     Stream_Element_Array)
   is
      Last : Ada.Streams.Stream_Element_Offset := Item'First - 1;
   begin
      Check_Is_Writeable (Stream.Pipe);

      while Last < Item'Last
      loop
         POSIX.IO.Write (File   => Stream.Pipe.Write_End,
                         Buffer => Item (Last + 1 .. Item'Last),
                         Last   => Last);
      end loop;
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


            when Running
               | Paused =>     -- Something else may have resumed a paused process.
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
                        Process.State := (if Signal = Signal_Interrupt then Interrupted else Killed);     -- Any other signal, such as SIGPIPE.
                     end;

                  when Stopped_By_Signal =>
                     Process.State := Paused;     -- Any stopping signal, such as from Ctrl-Z, rather than only from 'Pause'.
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


      -- The child process closes or duplicates each end of its pipes before it runs the program. If either end is
      -- not open, the child exits without running the program and the parent is not told, so detect that here.
      --
      procedure Check_Ends_Are_Open (The_Pipe : in Shell.Pipe;
                                     Name     : in String;
                                     Hint     : in String := "")
      is
         function Failure (Pipe_End : in String) return String
         is ("Cannot start '" & Program & "': the " & Pipe_End & " end of its '" & Name & "' pipe is not open." & Hint);
      begin
         if not End_Is_Open (The_Pipe, The_Pipe.Read_End)
         then
            raise Process_Error with Failure ("read");
         end if;

         if not End_Is_Open (The_Pipe, The_Pipe.Write_End)
         then
            raise Process_Error with Failure ("write");
         end if;
      end Check_Ends_Are_Open;

   begin
      if Pathname = null
      then
         raise Process_Error with "Program '" & Program & "' not found on PATH";
      else
         Free (Pathname);
      end if;

      if Errors /= Standard_Error
      then
         Check_Ends_Are_Open (Errors, "Errors");
      end if;

      if Output /= Standard_Output
      then
         Check_Ends_Are_Open (Output, "Output");
      end if;

      if Input /= Standard_Input
      then
         Check_Ends_Are_Open (Input, "Input",
                              Hint => " A process which feeds this pipe must be started with 'Pipeline => True',"
                                    & " and the write end closed after this process has started.");
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
                    Arguments         => [+"-c",
                                          +Command],
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
      if Process.State = Not_Started     -- Its null id would wait on any child, and take another process's status.
      then
         raise Process_Not_Started;
      end if;

      if Process.State in Terminated
      then
         return;     -- The process has already been waited on.
      end if;

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

      POSIX_Status : POSIX.Process_Primitives.Termination_Status;
   begin
      if Process.State = Not_Started     -- Its null id would wait on any child, and take another process's status.
      then
         return False;
      end if;

      if Process.State in Terminated
      then
         return True;
      end if;

      Wait_For_Child_Process (Status => POSIX_Status,
                              Child  => Process.Id,
                              Block  => False);

      Update_Status (Process, POSIX_Status);     -- This wait reaps the process, so a later wait would fail.

      return Process.State in Terminated;
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

      -- An exit status is only available for a process which has exited, rather than one ended by a signal.
      --
      return     Termination_Cause_Of (Process.Status) = Exited
        and then Exit_Status_Of       (Process.Status) = POSIX.Process_Primitives.Normal_Exit;
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
   begin
      if Process.State = Not_Started
      then
         raise Process_Not_Started;
      end if;

      if Has_Terminated (Process)
      then
         return;
      end if;

      Send_Signal (Process.Id, Signal_Kill);

      while Status (Process) not in Terminated     -- The process may end some other way before the signal arrives.
      loop
         delay Duration'Small;
      end loop;
   end Kill;



   procedure Send_Interrupt (Process : in out Shell.Process)
   is
      use POSIX.Signals;
   begin
      if Process.State = Not_Started
      then
         raise Process_Not_Started;
      end if;

      if not Has_Terminated (Process)
      then
         Send_Signal (Process.Id, Signal_Interrupt);
      end if;
   end Send_Interrupt;



   procedure Interrupt (Process : in out Shell.Process)
   is
      Most_Waits : constant := 1000;     -- About a second.
   begin
      Send_Interrupt (Process);

      -- The program may end some other way, or catch the signal and carry on.
      --
      for Each in 1 .. Most_Waits
      loop
         exit when Status (Process) in Terminated;
         delay 0.001;
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

            while Status (Process) not in Paused | Terminated     -- The process may end before the signal arrives.
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
      if Process.State = Not_Started
      then
         raise Process_Not_Started;
      end if;

      if Process.State in Terminated
      then
         raise Process_Has_Terminated with "Status => " & Process.State'Image;
      end if;

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

         Create (Log_File, Out_File, Name);
         Log_Enabled := True;     -- Only now, as the file may not be creatable.
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
