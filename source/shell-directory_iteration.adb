with
     Ada.Unchecked_Deallocation;


package body Shell.Directory_Iteration
is

   -- Cursor
   --

   function Has_Element (Pos : in Cursor) return Boolean
   is
      use Ada.Directories;
   begin
      return Pos.Directory_Entry /= null;
   end Has_Element;



   -- Directory
   --

   function To_Directory (Path : in String) return Directory
   is
   begin
      return Directory' (Path => +Path);
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



   overriding
   function First (Object : in Iterator) return Cursor
   is
      use Ada.Directories;
      C : Cursor;
   begin
      C := Cursor' (Container       => Object.Container,
                    Search          => Object.Search,
                    Directory_Entry => new Directory_Entry_Type);

      Get_Next_Entry (Search          => C.Search.all,
                      Directory_Entry => C.Directory_Entry.all);

      Object.State.Entries.Append (C.Directory_Entry);

      return C;
   end First;



   overriding
   function Next (Object   : in Iterator;
                  Position : in Cursor) return Cursor
   is
      use Ada.Directories;
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

      if More_Entries (Object.Search.all)
      then
         declare
            C : constant Cursor := Cursor' (Container       => Position.Container,
                                            Search          => Position.Search,
                                            Directory_Entry => new Ada.Directories.Directory_Entry_Type);
         begin
            Get_Next_Entry (Search          => Position.Search.all,
                            Directory_Entry => C.Directory_Entry.all);

            Object.State.Entries.Append (C.Directory_Entry);

            return C;
         end;
      end if;

      return No_Element;
   end Next;



   overriding
   procedure Finalize (Object : in out Iterator)
   is
      use Ada.Directories;
      procedure Free is new Ada.Unchecked_Deallocation (Directory_Entry_Type,
                                                        Directory_Entry_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Search_Type,
                                                        Search_Access);
      procedure Free is new Ada.Unchecked_Deallocation (Iterator_State,
                                                        Iterator_State_Access);
   begin
      for Each of Object.State.Entries
      loop
         Free (Each);
      end loop;

      End_Search (Object.Search.all);
      Free       (Object.Search);
      Free       (Object.State);
   end Finalize;


end Shell.Directory_Iteration;
