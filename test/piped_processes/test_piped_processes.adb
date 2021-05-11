with
     Shell,
     Ada.Text_IO;

procedure Test_Piped_Processes
is
   use Shell,
       Ada.Text_IO;
begin
   Put_Line ("Begin 'Piped_Processes' test.");
   New_Line (2);

   declare
      function CWE (Pipe : in Shell.Pipe) return Boolean
        renames Close_Write_End;

      Sort_1_Pipe : constant Shell.Pipe := To_Pipe;
      Uniq_Pipe   : constant Shell.Pipe := To_Pipe;
      Sort_2_Pipe : constant Shell.Pipe := To_Pipe;
      Head_Pipe   : constant Shell.Pipe := To_Pipe;

      Echo   : Shell.Process    := Start (Program   => "echo",
                                          Arguments => (1 => +"-e",
                                                        2 => +"b\nc\na\nb\nc\nb"),
                                          Output    => Sort_1_Pipe,
                                          Pipeline  => True);

      Sort_1 : Shell.Process    := Start (Program   => "sort",
                                          Input     => Sort_1_Pipe,
                                          Output    => Uniq_Pipe,
                                          Pipeline  => True);
      CWE0   : constant Boolean := CWE (Sort_1_Pipe);

      Uniq   : Shell.Process    := Start (Program   => "uniq",
                                          Arguments => (1 => (+"-c")),
                                          Input     => Uniq_Pipe,
                                          Output    => Sort_2_Pipe,
                                          Pipeline  => True);
      CWE1   : constant Boolean := CWE (Uniq_Pipe);

      Sort_2 : Shell.Process    := Start (Program   => "sort",
                                          Arguments => (1 => (+"-nr")),
                                          Input     => Sort_2_Pipe,
                                          Output    => Head_Pipe,
                                          Pipeline  => True);
      CWE2   : constant Boolean := CWE (Sort_2_Pipe);

      Head   : Shell.Process    := Start (Program   => "head",
                                          Input     => Head_Pipe,
                                          Pipeline  => False);
      CWE3   : constant Boolean := CWE (Head_Pipe);

      pragma Unreferenced (CWE0, CWE1, CWE2, CWE3);
   begin
      Wait_On (Head);

      if Normal_Exit (Head)
      then
         Put_Line ("Success");
      else
         Put_Line ("Fail");
      end if;

      Wait_On (Echo);
      Wait_On (Sort_1);
      Wait_On (Uniq);
      Wait_On (Sort_2);

      close (Sort_1_Pipe);
      close (Uniq_Pipe);
      close (Sort_2_Pipe);
      close (Head_Pipe);
   end;

   New_Line (2);
   Put_Line ("End 'Piped_Processes' test.");
end Test_Piped_Processes;
