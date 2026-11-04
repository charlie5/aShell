with
     Shell.Commands.Unsafe,
     Ada.Exceptions,
     Ada.Text_IO,
     Interfaces.C;


procedure Test_Start_Failure
--
-- A command which fails to start must go back to not being started, and leak no descriptors. Sending to a program
-- as it ends must raise only 'Pipe_Error', and a command need not be made by 'Forge'.
-- Any failure raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Unsafe;


   procedure Check (Test : in String;
                    Ok   : in Boolean)
   is
   begin
      if not Ok
      then
         raise Program_Error with Test & " failed.";
      end if;

      Ada.Text_IO.Put_Line (Test & " passed.");
   end Check;


   type Limits is
      record
         Soft,
         Hard : Interfaces.C.unsigned_long;
      end record
     with Convention => C;

   Open_Files_Limit : constant Interfaces.C.int := 7;     -- RLIMIT_NOFILE on Linux.

   function Get_Limit (Resource : in Interfaces.C.int;   Limit : access Limits) return Interfaces.C.int
     with Import, Convention => C, External_Name => "getrlimit";

   function Set_Limit (Resource : in Interfaces.C.int;   Limit : access Limits) return Interfaces.C.int
     with Import, Convention => C, External_Name => "setrlimit";


   -- Limit the descriptors, so that using them all up doesn't take long.
   --
   procedure Lower_Limit
   is
      use type Interfaces.C.int;
      Limit : aliased Limits;
   begin
      if Get_Limit (Open_Files_Limit, Limit'Access) /= 0
      then
         raise Program_Error with "Cannot read the limit on open files.";
      end if;

      Limit.Soft := 64;

      if Set_Limit (Open_Files_Limit, Limit'Access) /= 0
      then
         raise Program_Error with "Cannot lower the limit on open files.";
      end if;
   end Lower_Limit;

begin
   -- A command which isn't made by 'Forge'.
   --
   declare
      Unmade      : Unsafe.Command;
      Copy        : Unsafe.Command := Unmade;
      Many        : Command_Array (1 .. 3);
      Assigned_To : Unsafe.Command;
   begin
      Assigned_To := Forge.To_Command ("echo Assigned");
      Many (2)    := Forge.To_Command ("echo Assigned");
      Copy        := Assigned_To;

      Check ("Command made by assignment runs", +Output_Of (Run (Assigned_To)) = "Assigned" & ASCII.LF);
   end;

   Check ("Command which isn't made by 'Forge' can be finalized", True);


   -- Sending to a program as it ends. Its input writer closes the write end when it finds the reader has gone.
   --
   declare
      Large : constant Data (1 .. 70_000) := [others => 1];
   begin
      for Each in 1 .. 200
      loop
         declare
            The_Command : Unsafe.Command := Forge.To_Command ("true");
         begin
            The_Command.Start (Accepts_Input => True);

            for Send in 1 .. 2_000
            loop
               The_Command.Send (Large);
            end loop;

            raise Program_Error with "Sending to an ended program never failed.";
         exception
            when Pipe_Error =>
               null;
         end;
      end loop;

      Check ("Sending to a program as it ends raises only Pipe_Error", True);
   end;


   -- Running out of descriptors.
   --
   Lower_Limit;

   declare
      Held      : array (1 .. 64) of Pipe;
      Held_Last : Natural := 0;
      Spare     : Ada.Text_IO.File_Type;

      Large     : constant Data (1 .. 5_000) := [others => 1];
   begin
      begin
         loop
            Held (Held_Last + 1) := To_Pipe;
            Held_Last            := Held_Last + 1;
         end loop;
      exception
         when Too_Many_Pipes_Error =>
            null;
      end;

      begin
         Ada.Text_IO.Open (Spare, Ada.Text_IO.In_File, "/dev/null");     -- Any odd descriptor left over.
      exception
         when others =>
            null;
      end;

      Close (Held (Held_Last));     -- Enough for the input pipe, but not the output pipe.
      Held_Last := Held_Last - 1;

      declare
         The_Command : Unsafe.Command := Forge.To_Command ("cat");
      begin
         for Attempt in 1 .. 3
         loop
            begin
               The_Command.Start (Accepts_Input => True);
               Check ("Start with too few descriptors raises", False);
            exception
               when Too_Many_Pipes_Error =>
                  null;
            end;

            Check ("Failed start leaves the command not started", State'(The_Command.Status) = Not_Started);
         end loop;

         -- The pipe it made must have been closed, so there is still room for exactly one.
         --
         declare
            Room : constant Pipe := To_Pipe;
         begin
            begin
               Close (To_Pipe);
               Check ("Failed start leaks no descriptors", False);
            exception
               when Too_Many_Pipes_Error =>
                  Check ("Failed start leaks no descriptors", True);
            end;

            Close (Room);
         end;

         -- With room, the same command can then be started.
         --
         for Each in 1 .. 8
         loop
            Close (Held (Held_Last));
            Held_Last := Held_Last - 1;
         end loop;

         The_Command.Start (Input => Large);
         The_Command.Wait_On;

         Check ("Command starts once there is room", The_Command.Normal_Exit);
         Check ("Command is given its input once there is room", Output_Of (Results_Of (The_Command))'Length = 5_000);
      end;

      if Ada.Text_IO.Is_Open (Spare)
      then
         Ada.Text_IO.Close (Spare);
      end if;

      for Each in 1 .. Held_Last
      loop
         Close (Held (Each));
      end loop;
   end;

   Ada.Text_IO.Put_Line ("End test.");

exception
   when E : others =>
      Ada.Text_IO.Put_Line ("Failed: " & Ada.Exceptions.Exception_Information (E));
      raise;
end Test_Start_Failure;
