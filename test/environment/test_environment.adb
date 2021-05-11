with
     Shell.Commands,
     Ada.Environment_Variables,
     Ada.Text_IO;

procedure Test_Environment
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Test_Environment' tests.");

   New_Line (2);

   Test_1:
   declare
      Name     : constant String := "aShell_Test_Variable";
      Value    : constant String := "Working";
      Commands : constant String := "env | grep " & Name;
      Expected : constant String := Name & "=" & Value;

      use Shell,
          Shell.Commands,
          Shell.Commands.Forge;
      Piped_Commands : Command_Array := To_Commands (Commands);
   begin
      Put_Line ("Test 1 ~ Run piped commands => '" & Commands & "'");

      Ada.Environment_Variables.Set (Name  => Name,
                                     Value => Value);
      Put_Line ("Expected output:");
      Put_Line (Expected);
      Put_Line ("Actual output:");
      Start (Piped_Commands);
      delay 1.0;

      for i in Piped_Commands'Range
      loop
         Wait_On (Piped_Commands (i));
      end loop;

      Put_Line ("End test 1");
   end Test_1;

   New_Line (2);

   Test_2:
   declare
      Name     : constant String := "aShell_Test_Variable";
      Value    : constant String := "Changed";
      Commands : constant String := "env | grep " & Name;
      Expected : constant String := Name & "=" & Value;

      use Shell,
          Shell.Commands,
          Shell.Commands.Forge;
      Piped_Commands : Command_Array := To_Commands (Commands);
   begin
      Put_Line ("Test 2 ~ Run piped commands => '" & Commands & "'");

      Ada.Environment_Variables.Set (Name  => Name,
                                     Value => Value);
      Put_Line ("Expected output:");
      Put_Line (Expected);
      Put_Line ("Actual output:");
      Start (Piped_Commands);
      delay 1.0;

      for i in Piped_Commands'Range
      loop
         Wait_On (Piped_Commands (i));
      end loop;

      Put_Line ("End test 2");
   end Test_2;

   New_Line (2);
   Put_Line ("End 'Test_Environment' tests.");
end Test_Environment;
