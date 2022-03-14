with
     Shell.Commands.Safe,
     Ada.Characters.Latin_1,
     Ada.Text_IO;


procedure Test_Command_Input
is
   procedure Log (Message : in String)
                  renames Ada.Text_IO.Put_Line;
   procedure NL  (Count : in Ada.Text_IO.Positive_Count := 1)
                  renames Ada.Text_IO.New_Line;
begin
   Shell.Open_Log ("Shell.log");

   Log ("Begin Test_Command_Input test.");
   NL  (2);

   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Safe,
          Shell.Commands.Safe.Forge;
      The_Command : Safe.Command := To_Command ("cat -");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Send  (+"Hello world.");
      delay 0.5;
      Log ("Output: '" & (+Output_of (Results_Of (The_Command))) & "'");

      for i in 1 .. 5
      loop
         The_Command.Send (+i'Image);
         delay 0.5;
         Log ("Output: '" & (+Output_of (Results_Of (The_Command))) & "'");
      end loop;

      --  delay 1.0;

      The_Command.Send  (+"Goodbye world.");
      --  The_Command.Send  ((1 => Character'Pos (Ada.Characters.Latin_1.CR)));
      --  The_Command.Send  ((1 => Character'Pos (Ada.Characters.Latin_1.EOT)));
      --  The_Command.Send  ((1 => 4));

      --  delay 1.0;
      --  The_Command.Kill;

      --  delay 1.0;
      --  The_Command.Interrupt;

      while not The_Command.Has_Terminated
      loop
         --  The_Command.Send  ((1 => 4));
         declare
            Output : constant String := +Output_of (Results_Of (The_Command));
         begin
            if Output /= ""
            then
               Log ("Output: '" & Output & "'");
            end if;

            delay 0.5;
         end;
      end loop;
   end;

   Log ("Stop_Spawn_Client");
   Shell.Commands.Safe.Stop_Spawn_Client;

   --  delay 1.0;

   NL (2);
   Log ("End Test_Command_Input test.");
end Test_Command_Input;
