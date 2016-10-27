package body Shell
is

   function Start (Program   : in     String;
                   Arguments : in     String_Array;
                   Input     : in     Pipe;
                   Output    : in     Pipe;
                   Errors    : in     Pipe) return Process
   is
      The_Process : Process (Valid => True);
   begin
      return The_Process;
   end Start;

end Shell;
