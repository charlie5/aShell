with
     Shell,
     Ada.Text_IO;

procedure Test_Pipeline_Output
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Pipeline_Output' test.");
   New_Line (2);

   declare
      use Shell;
      Commands : Shell.Command_Array :=  To_Commands ("ps -A | grep bash | wc");
      Output   : constant String     := +Pipeline_Output (Commands);
   begin
      Put_Line ("'" & Output & "'");
   end;

   New_Line (2);
   Put_Line ("End 'Pipeline_Output' test.");
end Test_Pipeline_Output;
