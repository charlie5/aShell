with Ada.Strings.Unbounded, POSIX;

package EUP is
   subtype Ada_String is Standard.String;
   subtype POSIX_String is POSIX.POSIX_String;
   subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;
end EUP;
