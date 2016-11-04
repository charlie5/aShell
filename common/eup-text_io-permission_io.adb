with Ada.Text_IO;

package body EUP.Text_IO.Permission_IO is

   function Image (Item : in     POSIX.Permissions.Permission_Set) return String is
      Buffer : String := "rwxrwxrwx";
   begin
      for Index in POSIX.Permissions.Others_Execute ..
                   POSIX.Permissions.Owner_Read loop
         if not Item (Index) then
            Buffer (Buffer'Last - POSIX.Permissions.Permission'Pos (Index)) :=
              '-';
         end if;
      end loop;
      return Buffer;
   end Image;

   procedure Put (Item : in     POSIX.Permissions.Permission_Set) is
   begin
      Ada.Text_IO.Put (Item => Image (Item));
   end Put;

end EUP.Text_IO.Permission_IO;
