with
     Shell.Commands.Unsafe,
     Ada.Text_IO;


procedure Test_Unsafe_Command_Input
is
   procedure Log (Message : in String)
                  renames Ada.Text_IO.Put_Line;
   procedure NL  (Count : in Ada.Text_IO.Positive_Count := 1)
                  renames Ada.Text_IO.New_Line;
begin
   Log ("Begin Test_Command_Input test.");
   NL  (2);

   declare
      use Shell,
          Shell.Commands,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      The_Command : Unsafe.Command := To_Command ("cat -");
   begin
      The_Command.Start (Accepts_Input => True);
      The_Command.Send  (+"Hello world.");
      Log ("Output: '" & (+Output_of (Results_Of (The_Command))) & "'");

      for i in 1 .. 5
      loop
         The_Command.Send (+i'Image);
         Log ("Output: '" & (+Output_of (Results_Of (The_Command))) & "'");
      end loop;

      The_Command.Send  (+"Goodbye world.");
      Log ("Output: '" & (+Output_of (Results_Of (The_Command))) & "'");
   end;

   NL (2);
   Log ("End Test_Command_Input test.");
end Test_Unsafe_Command_Input;
