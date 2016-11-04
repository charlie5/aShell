with Ada.Streams, Ada.Strings.Unbounded;

package Stream_Shortcuts is
   procedure Put (Target : access Ada.Streams.Root_Stream_Type'Class;
                  Item   : in     String);

   procedure Get (Source : access Ada.Streams.Root_Stream_Type'Class;
                  Item   :    out String);

   procedure Get_Line (Source : access Ada.Streams.Root_Stream_Type'Class;
                       Item   :    out String;
                       Last   :    out Natural);

   function Get_Line
     (Source : access Ada.Streams.Root_Stream_Type'Class)
     return Ada.Strings.Unbounded.Unbounded_String;

   procedure Skip_Until_Empty_Line
     (Source : access Ada.Streams.Root_Stream_Type'Class);

   function To_String (Item : in Ada.Streams.Stream_Element_Array)
                      return String;

   function To_Stream (Item : in String)
                      return Ada.Streams.Stream_Element_Array;
end Stream_Shortcuts;
