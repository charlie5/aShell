with
  Ada.Strings.Unbounded,
  Interfaces.C.Strings;

package Shell_Utilities is
   type Chars_Ptr_Array_Ptr is
     access all Interfaces.C.Strings.chars_ptr_array;

   function Execute (Arguments : in     Chars_Ptr_Array_Ptr)
                    return Interfaces.C.int;

   function Split (Line : in Ada.Strings.Unbounded.Unbounded_String)
                  return Chars_Ptr_Array_Ptr;

end Shell_Utilities;
