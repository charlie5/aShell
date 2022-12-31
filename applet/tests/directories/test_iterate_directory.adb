with
     Shell.Directories,
     Ada.Directories,
     Ada.Text_IO;


procedure Test_Iterate_Directory
is
   use Ada.Text_IO;
begin
   Put_Line ("Begin 'Iterate_Directory' test.");
   New_Line (2);

   declare
      use Shell.Directories,
          Ada  .Directories;
   begin
      for Each of To_Directory ("..", Recurse => True)
      loop
         Put_Line (Full_Name (Each));     -- Display the full name of each file.
      end loop;
   end;

   New_Line (2);
   Put_Line ("End 'Iterate_Directory' test.");
end Test_Iterate_Directory;
