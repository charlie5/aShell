with
     Shell.Commands.Unsafe,
     Ada.Text_IO;

procedure Test_Pipeline_Error
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Pipeline_Error' test.");
   New_Line (2);

   Put_Line ("Test 1 =>");

   declare
      use Shell,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      Commands : Command_Array := To_Commands ("ls /non_existent_file | cat");
   begin
      Run (Commands);

      if Failed (Commands)
      then
         declare
            use Shell.Commands;

            Which : constant Natural :=  Which_Failed (Commands);
            Error : constant String  := +Errors_Of (Results_Of (Commands (Which)));
         begin
            Put_Line (  "Pipeline failed as expected.");
            Put_Line (  "Failed on command" & Natural'Image (Which)
                      & " '" & Commands (Which).Name & "'.");
            Put_Line (  "Error message => '" & Error & "'");
         end;
      end if;
   end;

   New_Line (2);
   Put_Line ("Test 2 =>");

   declare
      use Shell,
          Shell.Commands.Unsafe,
          Shell.Commands.Unsafe.Forge;
      Commands : Command_Array := To_Commands ("ls /non_existent_file | cat");
   begin
      Run (Commands, Raise_Error => True);

   exception
      when Command_Error =>
         declare
            use Shell.Commands;

            Which : constant Natural :=  Which_Failed (Commands);
            Error : constant String  := +Errors_Of (Results_Of (Commands (Which)));
         begin
            Put_Line (  "Pipeline failed and raised an exception, as expected.");
            Put_Line (  "Failed on command" & Natural'Image (Which)
                      & " '" & Commands (Which).Name & "'.");
            Put_Line (  "Error message => '" & Error & "'");
         end;
   end;

   New_Line (2);
   Put_Line ("End 'Pipeline_Error' test.");
end Test_Pipeline_Error;
