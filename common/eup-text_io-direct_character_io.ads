with Interfaces.C;

package EUP.Text_IO.Direct_Character_IO is

   type Character_Or_EOF is private;

   procedure Put (Item : in     POSIX.POSIX_Character);

   function Get return Character_Or_EOF;

   function Is_Character (Item : in     Character_Or_EOF) return Boolean;

   function End_Of_File (Item : in     Character_Or_EOF) return Boolean;

   function To_POSIX_Character (Item : in     Character_Or_EOF)
                            return POSIX.POSIX_Character;

   function To_Character (Item : in     Character_Or_EOF) return Character;

private

   type Character_Or_EOF is new Interfaces.C.int;

end EUP.Text_IO.Direct_Character_IO;
