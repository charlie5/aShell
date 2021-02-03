with
     Shell,
     Ada.Text_IO;

procedure Test_Pipeline_Output
is
   use Ada.Text_IO;
begin
   Put_Line ("Start test.");
   New_Line (2);

   declare
      use Shell;
      Output : constant String := Shell.Pipeline_Output (To_Commands ("ps -A | grep bash | wc"));
   begin
      Put_Line ("'" & Output & "'");
   end;

   New_Line (2);
   Put_Line ("End test.");
end Test_Pipeline_Output;
