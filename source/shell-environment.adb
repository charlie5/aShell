with Ada.Environment_Variables;

package body Shell.Environment is

   procedure Set (Name  : in     String;
                  Value : in     String) renames Ada.Environment_Variables.Set;

end Shell.Environment;
