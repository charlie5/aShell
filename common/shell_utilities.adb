with
  Ada.Characters.Latin_1,
  Ada.Strings.Maps,
  Ada.Text_IO,
  Ada.Unchecked_Conversion,
  System,
  POSIX.Process_Primitives;

with C_Signals;

with Fork, Wait, Process_Termination;

with Debugging; use Debugging;

package body Shell_Utilities is
   type chars_ptr_ptr is access constant Interfaces.C.Strings.chars_ptr;
   pragma Convention (C, chars_ptr_ptr);
   function To_Ptr is
      new Ada.Unchecked_Conversion (System.Address, chars_ptr_ptr);

   function Execute (Arguments : in     Chars_Ptr_Array_Ptr)
                    return Interfaces.C.int is
      use Ada.Text_IO, Process_Termination;

      procedure execvp (path : Interfaces.C.Strings.chars_ptr;
                        argv : chars_ptr_ptr);
      pragma Import (C, execvp, "execvp");

      use C_Signals;
      use type Interfaces.C.int;
      Child        : Interfaces.C.int;
      Child_Status : aliased Interfaces.C.int;
   begin
      if Arguments'Length = 0 then
         return 0;
      else
         Child := Fork;
         if Child < 0 then
            Put_Line (Standard_Error, "fork() failed.");
            return -1;
         elsif Child = 0 then
            Attach_Default_Handler (To => Signal_Interrupt);
            Attach_Default_Handler (To => Signal_Quit);
            execvp (path => Arguments (Arguments'First),
                    argv => To_Ptr (Arguments (Arguments'First)'Address));
            Put_Line (Standard_Error, "cannot execute command");
            POSIX.Process_Primitives.Exit_Process (Status => 1);
         else
            if Wait (Child_Status'Access) = -1 then
               Put_Line (Standard_Error, "wait() failed.");
            end if;
         end if;
         return Child_Status;
      end if;
   end Execute;

   function Split (Line : in Ada.Strings.Unbounded.Unbounded_String)
                  return Chars_Ptr_Array_Ptr is
      package Latin_1 renames Ada.Characters.Latin_1;
      use
        Ada.Strings.Unbounded,
        Ada.Strings.Maps,
        Interfaces.C,
        Interfaces.C.Strings;
      Whitespace  : constant Character_Set :=
                      To_Set (Span => (Low  => Latin_1.NUL,
                                       High => Latin_1.Space));
      Max_Fields  : constant size_t :=
                      size_t (Count (Source => Line,
                                     Set    => Whitespace) + 1);
      Arguments   : chars_ptr_array (1 .. Max_Fields + 1);
      Fields      : size_t range 0 .. Max_Fields := 0;
      First, Last : Natural := 1;
   begin
      while First in 1 .. Length (Line) loop
         if Is_In (Element (Line, First), Whitespace) then
            First := First + 1;
         else
            Last := First;
            loop
               exit when Last >= Length (Line);
               exit when Is_In (Element (Line, Last + 1), Whitespace);
               Last := Last + 1;
            end loop;
            Fields := Fields + 1;
            Arguments (Fields) := New_String (Slice (Line, First, Last));
            First := Last + 1;
         end if;
      end loop;

      Arguments (Fields + 1) := Null_Ptr;
      return new Chars_Ptr_Array'(Arguments (1 .. Fields + 1));
   end Split;

end Shell_Utilities;
