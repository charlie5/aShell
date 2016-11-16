with
     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Command_Line,
     Ada.Text_IO,

     Interfaces.C.Strings,

     GNAT.OS_Lib,

     POSIX.IO;

with POSIX.Process_Environment;
with POSIX.Process_Identification,
     POSIX.Unsafe_Process_Primitives,
     posix.c,
     Posix.Implementation;
with ada.Unchecked_Conversion;
with Ada.Exceptions; use Ada.Exceptions;


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

   function to_Command (Command_Line : in    String) return Command
   is
      use POSIX.Process_Primitives,
          Ada.Strings.Fixed;

      Result : Command;
      I      : Natural := Index (Command_Line, " ");     -- TODO: Check for other legal whitespace.

   begin
      if I = 0
      then
         Result.Name      := +Command_Line;
         Result.Arguments := Nil_String;
      else
         Result.Name      := +(Command_Line (Command_Line'First .. I - 1));
         Result.Arguments := +(Command_Line (I + 1              .. Command_Line'Last));
      end if;

      Result.Template := new Process_Template;     -- TDOD: Plug this leak.
      Open_Template (Result.Template.all);

      return Result;
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
            Full_Command  : String   := Trim (Pipeline (First .. Last),
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
   end;



   procedure Connect (From, To : in out Command)
   is
      Pipe : Shell.Pipe := to_Pipe;
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



   function  Run (The_Command : in     Command) return Process
   is
      use POSIX,
          POSIX.Process_Environment,
          POSIX.Process_Primitives,
          POSIX.Process_Identification;

      Child : Process_Id;
      Args  : POSIX_String_List;
      Name  : POSIX_String := To_POSIX_String (+The_Command.Name);

      use GNAT.OS_Lib,
          Ada.Strings.Unbounded;

      Arguments : Argument_List_Access := Argument_String_To_List (+(  The_Command.Name
                                                                     & " "
                                                                     & The_Command.Arguments));
      Result : Process;
   begin
      log (To_String (Name));

      for I in Arguments'Range
      loop
         log ("   Arg: '" & Arguments (I).all & "'");

         Append (Args,
                 To_POSIX_String (Arguments (I).all));
      end loop;

      Start_Process_Search (Child,
                            Name,
                            The_Command.Template.all,
                            Args);
      Close_Template (The_Command.Template.all);

      Make_Empty (Args);
      Free (Arguments);

      Result.Id := Child;
      return Result;
   end Run;



   procedure Run (The_Command : in     Command)
   is
      Process : Shell.Process := Run (The_Command);     -- Work is done here.
      pragma Unreferenced (Process);                    -- We don't care about the returned process.
   begin
      null;
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

      Connect (Commands);

      declare
         use type Interfaces.C.int;
         Process_ID : Interfaces.C.int;
         New_File   : POSIX.IO.File_Descriptor;
      begin
         log ("Spawning child processes.");

         for I in Commands'Range
         loop
            log ("");

            declare
               use POSIX.Process_Primitives;
               Command : Shell.Command renames Commands (I);
            begin
               if I /= Commands'Last
               then
                  Set_File_Action_To_Close     (Command.Template.all, Command.Output_Pipe.Read_End);
                  Set_File_Action_To_Duplicate (Command.Template.all, POSIX.IO.Standard_Output,
                                                                      Command.Output_Pipe.Write_End);
                  Set_File_Action_To_Close     (Command.Template.all, Command.Output_Pipe.Write_End);
               end if;

               if I /= Commands'First
               then
                  Set_File_Action_To_Close     (Command.Template.all, Command.Input_Pipe.Write_End);
                  Set_File_Action_To_Duplicate (Command.Template.all, POSIX.IO.Standard_Input,
                                                                      Command.Input_Pipe.Read_End);
                  Set_File_Action_To_Close     (Command.Template.all, Command.Input_Pipe.Read_End);
               end if;

               Run (Commands (I));

               if I /= Commands'First
               then
                  Close (Commands (I - 1).Output_Pipe);
               end if;
            end;
         end loop;
      end;

   exception
      when E : others =>
         log ("Error in main process Run procedure");
         log (Exception_Information (E));
         raise;
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



   function Image (Process : in Shell.Process) return String
   is
      use POSIX.Process_Identification;
   begin
      return Image (Process.Id);
   end;


end Shell;
