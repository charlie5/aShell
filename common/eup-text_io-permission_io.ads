package EUP.Text_IO.Permission_IO is

   function Image (Item : in     POSIX.Permissions.Permission_Set) return String;

   procedure Put (Item : in     POSIX.Permissions.Permission_Set);

end EUP.Text_IO.Permission_IO;
