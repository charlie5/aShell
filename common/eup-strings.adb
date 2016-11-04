with Ada.Characters.Latin_1;
with Ada.Strings;
with Ada.Strings.Fixed;

package body EUP.Strings is

   function To_Ada_String (Item : in     POSIX_String) return Ada_String is
      --  Converts a POSIX convention string to an Ada convention string.
      use Ada.Strings.Fixed;
      Buffer : String := POSIX.To_String (Item);
      Junk   : Natural;
   begin
      Junk := Index (Source  => Buffer,
                     Pattern => (1 => Ada.Characters.Latin_1.NUL));
      if Junk = 0 then
         return Buffer;
      else
         return Buffer (Buffer'First .. Junk - 1);
      end if;
   end To_Ada_String;

   function Format (Item  : in     Ada_String;
                    Width : in     Positive) return Ada_String is
      --  Printf-like string formatting.
      use Ada.Strings;
      use Ada.Strings.Fixed;
      Buffer : String (1 .. Width);
      Junk   : Natural;
   begin
      Junk := Index (Source  => Item,
                     Pattern => (1 => Ada.Characters.Latin_1.NUL));

      if Junk = 0 then
         Move (Source  => Item,
               Target  => Buffer,
               Drop    => Right,
               Justify => Left,
               Pad     => ' ');
      else
         Move (Source  => Item (Item'First .. Junk - 1),
               Target  => Buffer,
               Drop    => Right,
               Justify => Left,
               Pad     => ' ');
      end if;

      return Buffer;
   end Format;

   function Format (Item  : in     POSIX_String;
                    Width : in     Positive) return Ada_String is
      --  Printf-like string formatting.
   begin
      return Format (Item  => To_Ada_String (Item),
                     Width => Width);
   end Format;

   function To_POSIX_String (Item : in     Ada_String) return POSIX_String is
      --  Converts an Ada convention string to a POSIX convention string.
   begin
      return POSIX.To_POSIX_String (Item & (1 => Ada.Characters.Latin_1.NUL));
   end To_POSIX_String;

   function "=" (Left  : in     Ada_String;
                 Right : in     POSIX_String) return Boolean is
   begin
      return Left = To_Ada_String (Right);
   end "=";

   function "=" (Left  : in     POSIX_String;
                 Right : in     Ada_String) return Boolean is
   begin
      return To_Ada_String (Left) = Right;
   end "=";

end EUP.Strings;
