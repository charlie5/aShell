with
     Shell.Directories,
     Ada.Containers,
     Ada.Containers.Indefinite_Ordered_Sets,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Text_IO;


procedure Test_Directory_Contents
--
-- Treating a directory as a container, on a tree of known contents. Any failure raises an exception.
--
is
   use Ada.Directories;
   use type Ada.Containers.Count_Type;

   package Name_Sets is new Ada.Containers.Indefinite_Ordered_Sets (String);

   Root : constant String := Current_Directory & "/tree";


   procedure Check (Test : in String;
                    Ok   : in Boolean)
   is
   begin
      if not Ok
      then
         raise Program_Error with Test & " failed.";
      end if;

      Ada.Text_IO.Put_Line (Test & " passed.");
   end Check;


   procedure Make_File (Name : in String)
   is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Name);
      Ada.Text_IO.Close  (File);
   end Make_File;


   function Names_In (Path    : in String;
                      Recurse : in Boolean) return Name_Sets.Set
   is
      Result : Name_Sets.Set;
   begin
      for Each of Shell.Directories.To_Directory (Path, Recurse)
      loop
         Result.Include (Simple_Name (Each));
      end loop;

      return Result;
   end Names_In;

   use Name_Sets;

begin
   if Exists (Root)
   then
      Delete_Tree (Root);
   end if;

   Create_Path (Root & "/sub/deeper");
   Create_Path (Root & "/empty");

   Make_File (Root & "/top.txt");
   Make_File (Root & "/.hidden");
   Make_File (Root & "/sub/middle.txt");
   Make_File (Root & "/sub/deeper/bottom.txt");


   declare
      Found : constant Set := Names_In (Root, Recurse => False);
   begin
      Check ("Files of a directory are found",   Found.Contains ("top.txt"));
      Check ("Hidden files are found",           Found.Contains (".hidden"));
      Check ("Folders of a directory are found", Found.Contains ("sub") and Found.Contains ("empty"));
      Check ("Lower levels aren't searched",     not Found.Contains ("middle.txt"));
      Check ("'.' and '..' are entries, as in a search by 'Ada.Directories'", Found.Contains (".") and Found.Contains (".."));
      Check ("Only those entries are found",     Found.Length = 6);
   end;

   declare
      Found : constant Set := Names_In (Root, Recurse => True);
   begin
      Check ("Recursing finds the next level",   Found.Contains ("middle.txt"));
      Check ("Recursing finds the lowest level", Found.Contains ("bottom.txt"));
      Check ("Recursing finds lower folders",    Found.Contains ("deeper"));
      Check ("Recursing keeps the top level",    Found.Contains ("top.txt"));
      Check ("Only those entries are found",     Found.Length = 9);
   end;

   Check ("An empty directory has no other entries",           Names_In (Root & "/empty", Recurse => False).Length = 2);
   Check ("An empty directory has no other entries, recursed", Names_In (Root & "/empty", Recurse => True) .Length = 2);


   -- Full names, which are what a caller will use the entries for.
   --
   for Each of Shell.Directories.To_Directory (Root & "/sub/deeper")
   loop
      if Simple_Name (Each) = "bottom.txt"
      then
         Check ("Entries have a full name", Full_Name (Each) = Root & "/sub/deeper/bottom.txt");
      end if;
   end loop;


   -- The other parts of the interface.
   --
   declare
      use Shell.Directories;
      The_Directory : constant Shell.Directories.Directory := To_Directory (Root & "/sub");
      Count         : Natural := 0;
   begin
      Check ("A directory knows its path", Path (The_Directory) = Root & "/sub");

      for Position in The_Directory.Iterate
      loop
         Check ("A cursor has an element", Has_Element (Position));

         declare
            Name : constant String := Simple_Name (The_Directory (Position));
         begin
            Check ("A cursor gives its entry", Name = "middle.txt" or Name = "deeper" or Name = "." or Name = "..");
         end;

         Count := Count + 1;
      end loop;

      Check ("Cursors visit each entry", Count = 4);

      Count := 0;

      for Each of The_Directory
      loop
         Count := Count + 1;
      end loop;

      Check ("A directory can be searched again", Count = 4);
   end;


   -- A directory which doesn't exist.
   --
   declare
      Raised : Boolean := False;
   begin
      begin
         for Each of Shell.Directories.To_Directory (Root & "/missing")
         loop
            Ada.Text_IO.Put_Line (Simple_Name (Each));
         end loop;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("Raised " & Ada.Exceptions.Exception_Name (E));
            Raised := True;
      end;

      Check ("A missing directory raises", Raised);
   end;


   Delete_Tree (Root);
   Ada.Text_IO.Put_Line ("End test.");
end Test_Directory_Contents;
