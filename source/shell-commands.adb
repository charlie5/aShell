with
     GNAT.OS_Lib,
     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Unchecked_Deallocation,
     Ada.Unchecked_Conversion,
     Ada.Text_IO,
     Ada.Task_Identification,
     Ada.Characters.Handling,
     Ada.Exceptions;

package body Shell.Commands
is
   --- Strings
   --

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
      for i in The_Array'Range
      loop
         The_Array (i) := Strings.Element (i);
      end loop;

      return The_Array;
   end To_String_Array;


   function To_Arguments (All_Arguments : in String) return String_Array
   is
      use GNAT.OS_Lib;
      Command_Name : constant String      := "Command_Name";     -- 'Argument_String_To_List' expects the command name to be
                                                                 -- the 1st piece of the string, so we provide a dummy name.
      Arguments    : Argument_List_Access := Argument_String_To_List (Command_Name & " " & All_Arguments);
      Result       : String_Array (1 .. Arguments'Length - 1);
   begin
      for i in Result'Range
      loop
         Result (i) := +Arguments (i + 1).all;
      end loop;

      Free (Arguments);
      return Result;
   end To_Arguments;


   --- Commands
   --

   function Image (The_Command : in Command) return String
   is
      use String_Vectors;
      Result : Unbounded_String;
   begin
      Append (Result, "(" & The_Command.Name & ", (");

      for Each of The_Command.Arguments
      loop
         Append (Result, Each);

         if Each = Last_Element (The_Command.Arguments)
         then
            Append (Result, ")");
         else
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ")");
      return To_String (Result);
   end Image;


   procedure Define (The_Command : out Command;   Command_Line : in String)
   is
      use Ada.Strings.Fixed;

      I : constant Natural := Index (Command_Line, " ");
   begin
      The_Command.Copy_Count  := new Count' (1);

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
   end Define;


   function To_Command (Command_Line : in String;
                        Input        : in Pipe  := Standard_Input;
                        Output       : in Pipe  := Standard_Output;
                        Errors       : in Pipe  := Standard_Error) return Command
   is
      use Ada.Strings.Fixed;

      I : constant Natural := Index (Command_Line, " ");
   begin
      if I = 0
      then
         return Result : Command
         do
            Result.Name := +Command_Line;

            Result. Input_Pipe := Input;
            Result.Output_Pipe := Output;
            Result. Error_Pipe := Errors;

            Result.Copy_Count := new Count' (1);
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

            Result. Input_Pipe := Input;
            Result.Output_Pipe := Output;
            Result. Error_Pipe := Errors;

            Result.Copy_Count := new Count' (1);
         end return;
      end;
   end to_Command;


   function "+" (Command_Line : in String) return Command
   is
   begin
      return To_Command (Command_Line);
   end "+";


   function To_Commands (Pipeline      : in String;
                         Expect_Output : in Boolean := True) return Command_Array
   is
      use Ada.Strings.Fixed;

      Cursor : Positive := Pipeline'First;
      First,
      Last   : Positive;
      Count  : Natural := 0;

      Max_Commands_In_Pipeline : constant := 50;     -- Arbitrary.

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
         Result (Count).Expect_Output := Expect_Output;

         for i in 1 .. Count
         loop
            Define ( Result (i),
                    +All_Commands (i));
         end loop;
      end return;
   end To_Commands;


   function "+" (Pipeline : in String) return Command_Array
   is
   begin
      return To_Commands (Pipeline, Expect_Output => True);
   end "+";


   procedure Connect (From, To : in out Command)
   is
      Pipe : constant Shell.Pipe := To_Pipe;
   begin
      From.Output_Pipe := Pipe;
      To.   Input_Pipe := Pipe;

      From.Owns_Input_Pipe  := True;
      To.  Owns_Output_Pipe := True;
   end Connect;


   procedure Connect (Commands : in out Command_Array)
   is
   begin
      for i in Commands'First .. Commands'Last - 1
      loop
         Connect (From => Commands (i),
                  To   => Commands (i + 1));
      end loop;
   end Connect;


   procedure Close_Pipe_Write_Ends (The_Command : in out Command)
   is
   begin
      if The_Command.Output_Pipe /= Standard_Output
      then
         Close_Write_End (The_Command.Output_Pipe);
      end if;

      if The_Command.Error_Pipe /= Standard_Error
      then
         Close_Write_End (The_Command.Error_Pipe);
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


   function Name_of (The_Command : in out Command) return String
   is
   begin
      return +The_Command.Name;
   end Name_of;


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
      Input_Pipe : constant Shell.Pipe := (if Input = No_Data then The_Command.Input_Pipe
                                                              else To_Pipe);
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
      Input_Pipe    : constant Shell.Pipe   := (if Input = No_Data then First_Command.Input_Pipe
                                                                   else To_Pipe);
   begin
      First_Command.Input_Pipe := Input_Pipe;

      if Input_Pipe /= Standard_Input
      then
         Write_To (Input_Pipe, Input);
      end if;

      if not Pipeline
      then
         for Each of Commands
         loop
            Start (Each);
         end loop;

         return;
      end if;

      Connect (Commands);

      for i in Commands'Range
      loop
         Start (Commands (i),
                Pipeline => True);

         -- Since we are making a pipeline, we need to close the write ends of
         -- the Output & Errors pipes ourselves.
         --
         if i /= Commands'First
         then
            Close_Pipe_Write_Ends (Commands (i - 1));    -- Close ends for the prior command.
         end if;

      end loop;

      Close_Pipe_Write_Ends (Commands (Commands'Last));  -- Close ends for the final command.
   end Start;


   --- Run
   --

   procedure Gather_Results (The_Command : in out Command)
   is
   begin
      begin
         declare
            The_Output : constant Data := Output_Of (The_Command.Output_Pipe);
         begin
            if The_Output'Length /= 0
            then
               The_Command.Output.Append (The_Output);
            end if;
         end;
      exception
         when No_Output_Error =>
            The_Command.Error_Count := The_Command.Error_Count + 1;
      end;

      begin
         declare
            The_Errors : constant Data := Output_Of (The_Command.Error_Pipe);
         begin
            if The_Errors'Length /= 0
            then
               The_Command.Errors.Append (The_Errors);
            end if;
         end;
      exception
         when No_Output_Error =>
            The_Command.Error_Count := The_Command.Error_Count + 1;
      end;
   end Gather_Results;


   procedure Run (The_Command : in out Command;
                  Input       : in     Data    := No_Data;
                  Raise_Error : in     Boolean := False)
   is
   begin
      The_Command.Output_Pipe := To_Pipe (Blocking => False);
      The_Command. Error_Pipe := To_Pipe (Blocking => False);

      Start (The_Command, Input);
      loop
         Gather_Results (The_Command);
         exit when Has_Terminated (The_Command.Process);
      end loop;

      if not Normal_Exit (The_Command.Process)
         and Raise_Error
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
                  Retry        : in     Natural := 0;
                  Raise_Error  : in     Boolean := False)
   is
      use Ada.Characters.Handling,
          Ada.Exceptions;
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Last_Command.Output_Pipe := To_Pipe;
      Last_Command. Error_Pipe := To_Pipe;

      Start (The_Pipeline, Input);

      declare
         use Data_Vectors,
             Ada.Task_Identification,
             Ada.Text_IO;
         Restart_Pipeline : Boolean  := False;
         Retry_Count      : Natural  := 0;
         i                : Positive := 1;
      begin
         loop
            begin
               Gather_Results (Last_Command);   -- Gather on-going results.

               if   Last_Command.Error_Count > 3
                 or Restart_Pipeline
               then
                  Retry_Count              := Retry_Count + 1;
                  Restart_Pipeline         := False;
                  Last_Command.Error_Count := 0;

                  Clear (Last_Command.Output);
                  Clear (Last_Command.Errors);

                  for Each of The_Pipeline
                  loop
                     begin
                        Close (Each. Input_Pipe);
                        Close (Each.Output_Pipe);
                        Close (Each. Error_Pipe);

                        begin
                           Kill (Each);
                        exception
                           when E : POSIX.POSIX_Error =>
                              if To_Upper (Exception_Message (E)) /= "NO_SUCH_PROCESS"
                              then
                                 Put_Line (Image (Current_Task) & " ~ Unable to kill process" & Image (Each.Process));
                                 raise;
                              end if;
                        end;

                        begin
                           Wait_On (Each.Process);   -- Reap zombies.
                        exception
                           when E : POSIX.POSIX_Error =>
                              if To_Upper (Exception_Message (E)) /= "NO_CHILD_PROCESS"
                              then
                                 Put_Line (Image (Current_Task) & " ~ Unable to wait on process" & Image (Each.Process));
                                 raise;
                              end if;
                        end;
                     end;
                  end loop;

                  Last_Command.Output_Pipe := To_Pipe;
                  Last_Command. Error_Pipe := To_Pipe;

                  Start (The_Pipeline, Input);

                  i := 1;
               end if;

               if Has_Terminated (The_Pipeline (i).Process)
               then
                  if Normal_Exit (The_Pipeline (i).Process)
                  then
                     i := i + 1;

                     if i > The_Pipeline'Last
                     then
                        if (    Last_Command.Expect_Output
                            and Is_Empty (Last_Command.Output))
                          or not Is_Readable (Last_Command.Output_Pipe)
                          or not Is_Readable (Last_Command.Error_Pipe)
                        then
                           Restart_Pipeline := True;
                        else
                           Gather_Results (Last_Command);   -- Gather any final results.
                           exit;
                        end if;
                     end if;

                  else
                     if Retry_Count < Retry
                     then
                        Restart_Pipeline := True;

                     elsif Raise_Error
                     then
                        declare
                           Error : constant String :=   "Pipeline command" & Integer'Image (i)
                                                      & " '" & (+The_Pipeline (i).Name) & "' failed.";
                        begin
                           raise Command_Error with Error;
                        end;

                     else
                        exit;
                     end if;
                  end if;

               end if;

            exception
               when E : POSIX.POSIX_Error =>
                  if To_Upper (Exception_Message (E)) = "INVALID_ARGUMENT"
                  then
                     Restart_Pipeline := True;
                  else
                     raise;
                  end if;

               when E : others =>
                  Put_Line (Image (Current_Task) & "   "  & Exception_Information (E));
                  raise;
            end;
         end loop;

      end;
   end Run;


   function Run (The_Pipeline : in out Command_Array;
                 Input        : in     Data    := No_Data;
                 Retry        : in     Natural := 0;
                 Raise_Error  : in     Boolean := False) return Command_Results
   is
      Last_Command : Command renames The_Pipeline (The_Pipeline'Last);
   begin
      Run (The_Pipeline, Input, Retry, Raise_Error);

      return Results_Of (Last_Command);
   end Run;


   function Run (Command_Line : in String;
                 Input        : in Data  := No_Data) return Command_Results
   is
      use Ada.Strings.Fixed;
      The_Index   : constant Natural := Index (Command_Line, " | ");
      Is_Pipeline : constant Boolean := The_Index /= 0;
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


   function Failed (The_Command : in Command) return Boolean
   is
   begin
      return not Normal_Exit (The_Command.Process);
   end Failed;


   function Failed (The_Pipeline : in Command_Array) return Boolean
   is
   begin
      for Each of The_Pipeline
      loop
         if Failed (Each)
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
   begin
      if    The_Command.Output_Pipe = Standard_Output
      then
         raise Command_Error with "Attempt to read the Standard_Ouput pipe.";

      elsif The_Command.Error_Pipe  = Standard_Error
      then
         raise Command_Error with "Attempt to read the Standard_Error pipe.";
      end if;

      Gather_Results (The_Command);

      declare
         use Data_Vectors;

         Output_Size : Data_Offset := 0;
         Errors_Size : Data_Offset := 0;
      begin
         for Each of The_Command.Output
         loop
            Output_Size := Output_Size + Each'Length;
         end loop;

         for Each of The_Command.Errors
         loop
            Errors_Size := Errors_Size + Each'Length;
         end loop;

         declare
            Output : Data (1 .. Output_Size);
            Errors : Data (1 .. Errors_Size);

            procedure Set_Data (From : in out Data_Vector;
                                To   :    out Data)
            is
               First : Data_Index := 1;
               Last  : Data_Index;
            begin
               for Each of From
               loop
                  Last  := First + Each'Length - 1;
                  To (First .. Last) := Each;
                  First := Last + 1;
               end loop;

               From.Clear;
            end Set_Data;

         begin
            Set_Data (The_Command.Output, Output);
            Set_Data (The_Command.Errors, Errors);

            return (Output_Size => Output_Size,
                    Error_Size  => Errors_Size,
                    Output      => Output,
                    Errors      => Errors);
         end;

      exception
         when Storage_Error =>
            raise Command_Error with "Command output exceeds stack capacity. "
                                   & "Increase the stack limit via 'ulimit -s'.";
      end;
   end Results_Of;


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


   procedure Kill (The_Command : in Command)
   is
   begin
      Kill (The_Command.Process);
   end Kill;


   procedure Interrupt (The_Command : in Command)
   is
   begin
      Interrupt (The_Command.Process);
   end Interrupt;


   procedure Pause (The_Command : in Command)
   is
   begin
      Pause (The_Command.Process);
   end Pause;


   procedure Resume (The_Command : in Command)
   is
   begin
      Resume (The_Command.Process);
   end Resume;


   --- Controlled
   --

   overriding
   procedure Adjust (The_Command : in out Command)
   is
   begin
      The_Command.Copy_Count.all := The_Command.Copy_Count.all + 1;
   end Adjust;


   overriding
   procedure Finalize (The_Command : in out Command)
   is
      procedure Deallocate is new Ada.Unchecked_Deallocation (Count, Count_Access);
   begin
      The_Command.Copy_Count.all := The_Command.Copy_Count.all - 1;

      if The_Command.Copy_Count.all = 0
      then
         if The_Command.Owns_Input_Pipe
         then
            Close (The_Command.Input_Pipe);
         end if;

         if The_Command.Owns_Output_Pipe
         then
            Close (The_Command.Output_Pipe);
         end if;

         Close      (The_Command.Error_Pipe);
         Deallocate (The_Command.Copy_Count);
      end if;

   end Finalize;


end Shell.Commands;
