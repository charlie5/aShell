with
     Shell.Directory_Iteration,
     Ada.Directories,
     Ada.Text_IO;


procedure Test_Iterate_Directory
is
   use Ada.Text_IO;
begin
   Put_Line ("Start test.");
   New_Line (2);

   declare
      use Shell.Directory_Iteration,
          Ada  .Directories;
   begin
      for Each of To_Directory ("..", Recurse => True)
      loop
         Put_Line (Full_Name (Each));     -- Display the full name of each file.
      end loop;
   end;

   New_Line (2);
   Put_Line ("End test.");
end Test_Iterate_Directory;
