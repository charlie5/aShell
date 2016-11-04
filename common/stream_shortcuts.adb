with Ada.Characters.Latin_1, Ada.Unchecked_Conversion;

package body Stream_Shortcuts is
   package Unbounded renames Ada.Strings.Unbounded;
   package Latin_1   renames Ada.Characters.Latin_1;

   procedure Put (Target : access Ada.Streams.Root_Stream_Type'Class;
                  Item   : in     String) is
   begin
      for Index in Item'Range loop
         Character'Output (Target,
                           Item (Index));
      end loop;
   end Put;

   procedure Get (Source : access Ada.Streams.Root_Stream_Type'Class;
                  Item   :    out String) is
   begin
      for Index in Item'Range loop
         Item (Index) := Character'Input (Source);
      end loop;
   end Get;

   procedure Get_Line (Source : access Ada.Streams.Root_Stream_Type'Class;
                       Item   :    out String;
                       Last   :    out Natural) is
      Char : Character;
   begin
      Last := Item'First - 1;
      loop
         exit when Last >= Item'Last;
         Char := Character'Input (Source);
         case Char is
            when Latin_1.CR =>
               null;
            when Latin_1.LF =>
               return;
            when others =>
               Last := Last + 1;
               Item (Last) := Char;
         end case;
      end loop;
   end Get_Line;

   function Get_Line
     (Source : access Ada.Streams.Root_Stream_Type'Class)
     return Ada.Strings.Unbounded.Unbounded_String is
      Result : Unbounded.Unbounded_String;
      Char   : Character;
   begin
      loop
         Char := Character'Input (Source);
         case Char is
            when Latin_1.CR =>
               null;
            when Latin_1.LF =>
               return Result;
            when others =>
               Unbounded.Append (Source   => Result,
                                 New_Item => Char);
         end case;
      end loop;
   end Get_Line;

   procedure Skip_Until_Empty_Line
     (Source : access Ada.Streams.Root_Stream_Type'Class) is
      Line_Length : Natural := 0;
   begin
      loop
         case Character'Input (Source) is
            when Latin_1.CR =>
               null;
            when Latin_1.LF =>
               exit when Line_Length = 0;
               Line_Length := 0;
            when others =>
               Line_Length := Line_Length + 1;
         end case;
      end loop;
   end Skip_Until_Empty_Line;

   function To_String (Item : in Ada.Streams.Stream_Element_Array)
                      return String is
      function To_Character is
         new Ada.Unchecked_Conversion (Source => Ada.Streams.Stream_Element,
                                       Target => Character);

      Result : String (Natural (Item'First) .. Natural (Item'Last));
   begin
      for I in Item'Range loop
         Result (Natural (I)) := To_Character (Item (I));
      end loop;
      return Result;
   end To_String;

   function To_Stream (Item : in String)
                      return Ada.Streams.Stream_Element_Array is
      function To_Byte is
         new Ada.Unchecked_Conversion (Source => Character,
                                       Target => Ada.Streams.Stream_Element);
      subtype Offset is Ada.Streams.Stream_Element_Offset;

      Result : Ada.Streams.Stream_Element_Array
                 (Offset (Item'First) .. Offset (Item'Last));
   begin
      for I in Item'Range loop
         Result (Offset (I)) := To_Byte (Item (I));
      end loop;
      return Result;
   end To_Stream;
end Stream_Shortcuts;
