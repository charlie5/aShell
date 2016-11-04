with
     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Command_Line,
     Ada.Text_IO,

     Interfaces.C.Strings,

     POSIX.IO,
     POSIX.Process_Primitives,

     Fork,
     Wait;


package body Shell
is

   procedure log (Message : in String)
   is
   begin
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => Message);
   end log;



   procedure Exec (Program : in String)
   is
      procedure Exec_No_Arguments (Command : in     Interfaces.C.Strings.chars_ptr;
                                   Name    : in     Interfaces.C.Strings.chars_ptr;
                                   The_End : in     Interfaces.C.Strings.chars_ptr);
      pragma Import (C, Exec_No_Arguments, "execlp");

      Command : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Program);
   begin
      Exec_No_Arguments (Command, Command, Interfaces.C.Strings.Null_Ptr);
   end Exec;



   -- Commands
   --

   function to_Command (Name      : in    String;
                        Arguments : in    String := "") return Command
   is
      Result : Command; -- (Argument_Count => 0);
   begin
      Result.Name      := +Name;
      Result.Arguments := +Arguments;

      return Result;
   end to_Command;



   function to_Commands (Pipeline  : in    String) return Command_Array
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
            Full_Command  : String   := Trim (Pipeline (First .. Last),
                                              Ada.Strings.Both);
            Space_Index   : Natural  := Index (Full_Command, " ");
            Has_Arguments : Boolean  := Space_Index /= 0;
         begin
            log ("'" & Full_Command & "'");

            if Has_Arguments
            then
               declare
                  Name      : String := Full_Command (Full_Command'First .. Space_Index - 1);
                  Arguments : String := Full_Command (Space_Index + 1    .. Full_Command'Last);
               begin
                  log ("'" & Name      & "'");
                  log ("'" & Arguments & "'");

                  Count          := Count + 1;
                  Result (Count) := to_Command (Name, Arguments);
               end;
            else
               declare
                  Name : String := Full_Command;
               begin
                  log ("'" & Name & "'");

                  Count          := Count + 1;
                  Result (Count) := to_Command (Name);
               end;
            end if;
         end;

         exit when Last = Pipeline'Last;

         Cursor := Last + 1;
      end loop;

      return Result (1 .. Count);
   end;



   procedure Connect (From, To : in out Command)
   is
      Pipe : Shell.Pipe := to_Pipe;
   begin
      From.Output_Pipe := Pipe;
      To.Input_Pipe    := Pipe;
   end Connect;



   procedure Run (The_Command : in     Command)
   is
   begin
      Exec (+The_Command.Name);
   end Run;



   procedure Run (Commands : in out Command_Array;
                  Piped    : in     Boolean      := True)
   is
   begin
      if not Piped
      then
         for I in Commands'Range
         loop
            log ("Running " & (+Commands (I).Name));
            Run (Commands (I));
         end loop;

         return;
      end if;


      declare
         From : Positive;
         To   : Positive;
      begin
         log ("Connecting via pipes.");

         for I in Commands'Range
         loop
            exit when I + 1 > Commands'Last;

            Connect (From => Commands (I),
                     To   => Commands (I + 1));
         end loop;
      end;


      declare
         use type Interfaces.C.int;
         Process_ID : Interfaces.C.int;
         New_File   : POSIX.IO.File_Descriptor;
      begin
         log ("Spawning child processes.");

         for I in Commands'Range
         loop
            --  Parent
            Process_ID := Fork;

            if Process_ID = -1 then
               log ("Calling fork() failed.");
               POSIX.Process_Primitives.Exit_Process (Status => 2);

            elsif Process_ID = 0 then
               --  Child

               if I /= Commands'First
               then
                  POSIX.IO.Close (File => Commands (I).Input_Pipe.Write_End);
                  New_File := POSIX.IO.Duplicate_And_Close (Commands (I).Input_Pipe.Read_End, POSIX.IO.Standard_Input);
                  POSIX.IO.Close (File => Commands (I).Input_Pipe.Read_End);
               end if;

               if I /= Commands'Last
               then
                  POSIX.IO.Close (File => Commands (I).Output_Pipe.Read_End);
                  New_File := POSIX.IO.Duplicate_And_Close (Commands (I).Output_Pipe.Write_End, POSIX.IO.Standard_Output);
                  POSIX.IO.Close (File => Commands (I).Output_Pipe.Write_End);
               end if;

               Run (Commands (I));
            end if;

            if I /= Commands'First
            then
               close (Commands (I - 1).Output_Pipe);
            end if;
         end loop;
      end;

   end Run;



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



   procedure Close (The_Pipe : in out Pipe)
   is
   begin
      POSIX.IO.Close (File => The_Pipe.Read_End);
      POSIX.IO.Close (File => The_Pipe.Write_End);

      The_Pipe := (Null_File_Descriptor,
                   Null_File_Descriptor);
   end;



   -- Processes
   --

   function Start (Program   : in     String;
                   Arguments : in     String_Array;
                   Input     : in     Pipe;
                   Output    : in     Pipe;
                   Errors    : in     Pipe) return Process
   is
--      The_Process : Process (Valid => True);
      The_Process : Process;
   begin
      return The_Process;
   end Start;


end Shell;
