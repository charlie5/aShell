with
     Ada.Strings.Unbounded;

package Shell
is

   function "+" (Item : in String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   type String_Array is array (Positive range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   type Pipe is private;

   type Process (<>) is private;

   function Start (Program   : in     String;
                   Arguments : in     String_Array;
                   Input     : in     Pipe;                   --  We should probably distinguish
                   Output    : in     Pipe;                   --  between the two ends of a pipe.
                   Errors    : in     Pipe) return Process;

private

   type Pipe is
      record
         null;
      end record;

   type Process (Valid : Boolean) is
      record
         null;
      end record;

end Shell;
