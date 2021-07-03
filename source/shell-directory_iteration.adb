with
     POSIX.File_Status,
     Ada.Unchecked_Deallocation,
     Ada.IO_Exceptions;

package body Shell.Directory_Iteration
is

   -- Cursor
   --

   function Has_Element (Pos : in Cursor) return Boolean
   is
   begin
      return Pos.Directory_Entry /= null;
   end Has_Element;



   -- Directory
   --

   function To_Directory (Path    : in String;
                          Recurse : in Boolean := False) return Directory
   is
   begin
      return Directory' (Path    => +Path,
                         Recurse =>  Recurse);
   end To_Directory;


   function Path (Container : in Directory) return String
   is
   begin
      return +Container.Path;
   end Path;


   function Iterate (Container : in Directory) return Directory_Iterators.Forward_Iterator'Class
   is
      use Ada.Directories,
          Ada.Finalization;
      V  : constant Directory_Access := Container'Unrestricted_Access;
   begin
      return It : constant Iterator := (Controlled with
                                        Container => V,
                                        Search    => new Search_Type,
                                        State     => new Iterator_State)
      do
         Start_Search (Search    => It.Search.all,
                       Directory => Path (Container),
                       Pattern   => "");
      end return;
   end Iterate;


   function Element_Value (Container : in Directory;
                           Pos       : in Cursor) return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
   begin
      return (Element => Pos.Directory_Entry);
   end Element_Value;


   procedure Get_Next_Directory_Entry (Object          : in Iterator;
                                       Directory_Entry : in Directory_Entry_Access)
   is
      use Ada.Directories,
          POSIX,
          POSIX.File_Status;
      Status : POSIX.File_Status.Status;
   begin
      Get_Next_Entry (Search          => Object.Search.all,
                      Directory_Entry => Directory_Entry.all);

      Status := Get_Link_Status (To_POSIX_String (Full_Name (Directory_Entry.all)));

      if Object.Container.Recurse
        and Kind        (Directory_Entry.all)  = Ada.Directories.Directory
        and Simple_Name (Directory_Entry.all) /= "."
        and Simple_Name (Directory_Entry.all) /= ".."
        and not Is_Symbolic_Link (Status)
      then
         Object.State.Subdirs.Append (+Full_Name (Directory_Entry.all));
      end if;

   end Get_Next_Directory_Entry;


   overriding
   function First (Object : in Iterator) return Cursor
   is
      C : Cursor;
   begin
      C := Cursor' (Container       => Object.Container,
                    Directory_Entry => new Directory_Entry_Type);

      Get_Next_Directory_Entry (Object, C.Directory_Entry);
      Object.State.Prior := C.Directory_Entry;

      return C;
   end First;


   overriding
   function Next (Object   : in Iterator;
                  Position : in Cursor) return Cursor
   is
      use Ada.Directories;
      procedure Free is new Ada.Unchecked_Deallocation (Directory_Entry_Type,
                                                        Directory_Entry_Access);
      function new_Cursor return Cursor
      is
         C : constant Cursor := Cursor' (Container       => Position.Container,
                                         Directory_Entry => new Ada.Directories.Directory_Entry_Type);
      begin
         Get_Next_Directory_Entry (Object, C.Directory_Entry);

         Free (Object.State.Prior);
         Object.State.Prior := C.Directory_Entry;

         return C;
      end new_Cursor;

   begin
      if Position.Container = null
      then
         return No_Element;
      end if;

      if Position.Container /= Object.Container
      then
         raise Program_Error with
           "Position cursor of Next designates wrong directory";
      end if;

      begin
         if More_Entries (Object.Search.all)
         then
            return new_Cursor;
         end if;
      exception
         when Ada.IO_Exceptions.Use_Error =>
            null;   -- The next entry cannot be accessed, so end this directories search.
      end;

      End_Search (Object.Search.all);

      -- No more entries left, so start a new search, if any subdirs remain.
      ---
      while not Object.State.Subdirs.Is_Empty
      loop
         declare
            Subdir : constant String := +Object.State.Subdirs.Last_Element;
         begin
            Object.State.Subdirs.Delete_Last;

            Start_Search (Search    => Object.Search.all,
                          Directory => Subdir,
                          Pattern   => "");

            if More_Entries (Object.Search.all)
            then
               return new_Cursor;
            end if;

         exception
            when Ada.IO_Exceptions.Use_Error =>
               null; -- A forbidden directory, so ignore.
         end;
      end loop;

      Free (Object.State.Prior);

      return No_Element;
   end Next;


   overriding
   procedure Finalize (Object : in out Iterator)
   is
      procedure Free is new Ada.Unchecked_Deallocation (Search_Type,
                                                        Search_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Iterator_State,
                                                        Iterator_State_Access);
   begin
      Free       (Object.Search);
      Free       (Object.State);
   end Finalize;


end Shell.Directory_Iteration;
