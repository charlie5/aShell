with POSIX.Process_Environment;

package body Shell.Environment is

   procedure Set (Name  : in     String;
                  Value : in     String)
   is
      use POSIX, POSIX.Process_Environment;
   begin
      Set_Environment_Variable (Name  => To_POSIX_String (Name),
                                Value => To_POSIX_String (Value));
   end Set;

end Shell.Environment;
