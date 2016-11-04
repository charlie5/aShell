with Interfaces.C.Strings;

function Get_Host_Name return String is
   use Interfaces.C, Interfaces.C.Strings;
   function gethostname (Name : chars_ptr;
                         Len  : size_t) return int;
   pragma Import (C, gethostname, "gethostname");
   Buffer_Size : constant := 1000;

   Buffer : chars_ptr;
   Status : int;
begin
   Buffer := New_String ((1 .. Buffer_Size => '_'));
   Status := gethostname (Name => Buffer,
                          Len  => Buffer_Size);
   declare
      Result : String := Value (Buffer);
   begin
      Free (Buffer);
      return Result;
   end;
end Get_Host_Name;
