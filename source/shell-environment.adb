with POSIX.Process_Environment;

package body Shell.Environment is

   procedure Set (Name  : in     String;
                  Value : in     String)
   is
      use POSIX.Process_Environment;
   begin
      Set_Environment_Variable (Name  => Name,
                                Value => Value);
   end Set;

end Shell.Environment;
