with Ada.IO_Exceptions, Ada.Strings.Maps, Ada.Text_IO;

with EUP.Strings;

package body EUP.Text_IO.Integer_IO is
   package Ada_Integer_Text_IO is
      new Ada.Text_IO.Integer_IO (Num => Num);

   Whitespace : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (Ada.Characters.Latin_1.CR &
                              Ada.Characters.Latin_1.LF &
                              Ada.Characters.Latin_1.HT &
                              Ada.Characters.Latin_1.Space);

   procedure Get (File  : in     File_Type;
                  Item  :    out Num;
                  Width : in     Field := 0) is
      Buffer : String (1 .. Num'Width * 10 + 100);
      Filled : Natural := Buffer'First;
      Read   : Natural;
   begin
      loop
         Get (File => File,
              Item => Buffer (Buffer'First));
         exit when not Ada.Strings.Maps.Is_In (Buffer (Buffer'First),
                                               Whitespace);
      end loop;

      loop
         Get (File => File,
              Item => Buffer (Filled + 1));
         exit when Ada.Strings.Maps.Is_In (Buffer (Buffer'First),
                                           Whitespace);
         Filled := Filled + 1;
      end loop;

      Ada_Integer_Text_IO.Get (From => Buffer (Buffer'First .. Filled),
                               Item => Item,
                               Last => Read);
      if Read /= Filled then
         raise Ada.IO_Exceptions.Data_Error;
      end if;
   end Get;

   procedure Put (File  : in     File_Type;
                  Item  : in     Num;
                  Width : in     Field := Default_Width;
                  Base  : in     Number_Base := Default_Base) is
      Buffer : String (1 .. Field'Max (Width, Num'Width));
      First  : Positive := Buffer'First;
   begin
      Ada_Integer_Text_IO.Put (To   => Buffer,
                               Item => Item,
                               Base => Base);
      loop
         exit when Buffer'Last - First < Width;
         exit when Buffer (First) /= ' ';
         First := First + 1;
      end loop;

      Put (File => File,
           Item => Buffer (First .. Buffer'Last));
   end Put;

   procedure Get (From : in     String;
                  Item :    out Num;
                  Last :    out Positive) is
      pragma Inline (Get);
   begin
      Ada_Integer_Text_IO.Get (From => From,
                               Item => Item,
                               Last => Last);
   end Get;

   procedure Get (From : in     POSIX_String;
                  Item :    out Num;
                  Last :    out Positive) is
      pragma Inline (Get);
   begin
      Ada_Integer_Text_IO.Get (From => EUP.Strings.To_Ada_String (From),
                               Item => Item,
                               Last => Last);
   end Get;

   procedure Get (From : in     Unbounded_String;
                  Item :    out Num;
                  Last :    out Positive) is
      pragma Inline (Get);
   begin
      Ada_Integer_Text_IO.Get (From => Ada.Strings.Unbounded.To_String (From),
                               Item => Item,
                               Last => Last);
   end Get;

   procedure Put (To   :    out String;
                  Item : in     Num;
                  Base : in     Number_Base := Default_Base) is
   begin
      Ada_Integer_Text_IO.Put (To   => To,
                               Item => Item,
                               Base => Base);
   end Put;
end EUP.Text_IO.Integer_IO;
