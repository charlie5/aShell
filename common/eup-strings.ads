package EUP.Strings is
   function To_Ada_String (Item : in     POSIX_String) return Ada_String;
   --  Converts a POSIX convention string to an Ada convention string.

   function Format (Item  : in     Ada_String;
                    Width : in     Positive) return Ada_String;
   --  Printf-like string formatting.

   function Format (Item  : in     POSIX_String;
                    Width : in     Positive) return Ada_String;
   --  Printf-like string formatting.

   function To_POSIX_String (Item : in     Ada_String) return POSIX_String;
   --  Converts an Ada convention string to a POSIX convention string.

   function "=" (Left  : in     Ada_String;
                 Right : in     POSIX_String) return Boolean;
   function "=" (Left  : in     POSIX_String;
                 Right : in     Ada_String) return Boolean;
end EUP.Strings;
