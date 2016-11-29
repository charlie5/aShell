with
     Shell,
     Ada.Text_IO;


procedure Test_Piped_Processes
is
   use Ada.Text_IO;
begin
   Put_Line ("Start test.");
   New_Line (2);

   declare
      use Shell;
      function CWE (Pipe : in Shell.Pipe) return Boolean
        renames Close_Write_End;

      Uniq_Pipe : constant Shell.Pipe := To_Pipe;
      Sort_Pipe : constant Shell.Pipe := To_Pipe;
      Head_Pipe : constant Shell.Pipe := To_Pipe;

      Sort_1    : Shell.Process := Start (Program   => "sort",
                                          Output    => Uniq_Pipe,
                                          Pipeline  => True);

      Uniq      : Shell.Process := Start (Program   => "uniq",
                                          Arguments => (1 => (+"-c")),
                                          Input     => Uniq_Pipe,
                                          Output    => Sort_Pipe,
                                          Pipeline  => True);
      CWE1      : Boolean       := CWE (Uniq_Pipe);

      Sort_2    : Shell.Process := Start (Program   => "sort",
                                          Arguments => (1 => (+"-nr")),
                                          Input     => Sort_Pipe,
                                          Output    => Head_Pipe,
                                          Pipeline  => True);
      CWE2      : Boolean       := CWE (Sort_Pipe);

      Head      : Shell.Process := Start (Program   => "head",
                                          Input     => Head_Pipe,
                                          Pipeline  => False);
      CWE3      : Boolean       := CWE (Head_Pipe);

      pragma Unreferenced (Sort_1, Uniq, Sort_2, Head,
                           CWE1,   CWE2, CWE3);
   begin
      delay 1.0;
   end;

   New_Line (2);
   Put_Line ("End test.");
end Test_Piped_Processes;
