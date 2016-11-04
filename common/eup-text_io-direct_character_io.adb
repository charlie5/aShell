with Interfaces.C;

package body EUP.Text_IO.Direct_Character_IO is

   function putchar (c : Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, putchar, "putchar");

   function getchar return Interfaces.C.int;
   pragma Import (C, getchar, "getchar");

   procedure Put (Item : in     POSIX.POSIX_Character) is
      pragma Inline (Put);
      Result : Interfaces.C.int;
   begin
      Result := putchar (POSIX.POSIX_Character'Pos (Item));
   end Put;

   function Get return Character_Or_EOF is
      pragma Inline (Get);
   begin
      return Character_Or_EOF (Getchar);
   end Get;

   function Is_Character (Item : in     Character_Or_EOF) return Boolean is
      pragma Inline (Is_Character);
   begin
      return Item >= Character_Or_EOF (Character'Pos (Character'First)) or
             Item <= Character_Or_EOF (Character'Pos (Character'Last));
   end Is_Character;

   function End_Of_File (Item : in     Character_Or_EOF) return Boolean is
      pragma Inline (End_Of_File);
   begin
      return Item < Character_Or_EOF (Character'Pos (Character'First)) or
             Item > Character_Or_EOF (Character'Pos (Character'Last));
   end End_Of_File;

   function To_POSIX_Character (Item : in     Character_Or_EOF)
                               return POSIX.POSIX_Character is
      pragma Inline (To_POSIX_Character);
   begin
      return POSIX.POSIX_Character'Val (Item);
   end To_POSIX_Character;

   function To_Character (Item : in     Character_Or_EOF) return Character is
      pragma Inline (To_Character);
   begin
      return Character'Val (Item);
   end To_Character;

end EUP.Text_IO.Direct_Character_IO;
