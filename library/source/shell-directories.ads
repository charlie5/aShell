with
     Ada.Iterator_Interfaces,
     Ada.Directories,
     Ada.Containers.Vectors;

private
with
     Ada.Finalization;


package Shell.Directories
is

   type Directory is tagged private
     with
       Default_Iterator  => Iterate,
       Iterator_Element  => Constant_Reference_Type,
       Constant_Indexing => Element_Value;

   function To_Directory (Path    : in String;
                          Recurse : in Boolean := False) return Directory;

   function Path (Container : in Directory) return String;


   type Cursor is private;
   function Has_Element (Pos : Cursor) return Boolean;


   subtype Directory_Entry_Type is Ada.Directories.Directory_Entry_Type;

   type Constant_Reference_Type (Element : not null access constant Directory_Entry_Type)
   is private
     with
       Implicit_Dereference => Element;


   package Directory_Iterators is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate       (Container : Directory) return Directory_Iterators.Forward_Iterator'Class;
   function Element_Value (Container : Directory;
                           Pos       : Cursor) return Constant_Reference_Type;


private

   type Directory is tagged
      record
         Path    : Unbounded_String;
         Recurse : Boolean;
      end record;


   type    Constant_Reference_Type (Element : not null access constant Directory_Entry_Type) is null record;
   subtype Search_Type            is Ada.Directories.Search_Type;
   type    Search_Access          is access all Search_Type;
   type    Directory_Access       is access all Directory;
   type    Directory_Entry_Access is access all Directory_Entry_Type;


   -- Entries
   --

   package Entry_Vectors is new ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Directory_Entry_Access);
   type Entries is new Entry_Vectors.Vector with null record;


   -- Cursor
   --

   type Cursor is
      record
         Container       : Directory_Access;
         Directory_Entry : Directory_Entry_Access;
      end record;

   No_Element : constant Cursor := Cursor' (Container       => null,
                                            Directory_Entry => null);

   -- String Vectors
   --

   package Strings_Vector is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                         Element_Type => Unbounded_String);

   -- Iterator
   --

   type Iterator_State is
      record
         Prior   : Directory_Entry_Access;         -- Used to free expired directory entries.
         Subdirs : Strings_Vector.Vector;          -- Used to recurse through sub-directories.
      end record;

   type Iterator_State_Access is access all Iterator_State;

   type Iterator is new Ada.Finalization.Controlled
                    and Directory_Iterators.Forward_Iterator with
      record
         Container : Directory_Access;
         Search    : Search_Access;                 -- Access is due to Search_Type being limited.
         State     : Iterator_State_Access;         -- Access allows modifying the Iterator state in functions.
      end record;

   overriding
   function First     (Object   : in     Iterator) return Cursor;

   overriding
   function Next      (Object   : in     Iterator;
                       Position : in     Cursor)   return Cursor;
   overriding
   procedure Finalize (Object   : in out Iterator);


end Shell.Directories;
