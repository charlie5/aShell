with Ada.IO_Exceptions, Interfaces.C.Strings;
with Fork, Wait;
function Open_Process (Command : in POSIX.POSIX_String;
                       Mode    : in POSIX.IO.File_Mode)
                      return POSIX.IO.File_Descriptor is
   use type Interfaces.C.int;

   type Pipe_Ends is (Read_End, Write_End);
   type Pipe is array (Pipe_Ends) of POSIX.IO.File_Descriptor;

   Connection : Pipe;
   Process_ID : Interfaces.C.int;
   Parent_End : Pipe_Ends;
   Child_End  : Pipe_Ends;
   New_File   : POSIX.IO.File_Descriptor;
begin
   case Mode is
      when POSIX.IO.Read_Only =>
         Parent_End := Read_End;
         Child_End  := Write_End;
      when POSIX.IO.Write_Only =>
         Parent_End := Write_End;
         Child_End  := Read_End;
      when POSIX.IO.Read_Write =>
         raise Ada.IO_Exceptions.Mode_Error;
   end case;

   POSIX.IO.Create_Pipe (Read_End  => Connection (Read_End),
                         Write_End => Connection (Write_End));

   Process_ID := Fork;
   if Process_ID = -1 then
      POSIX.IO.Close (Connection (Read_End));
      POSIX.IO.Close (Connection (Write_End));
      raise Program_Error;
   elsif Process_ID > 0 then
      --  Parent
      POSIX.IO.Close (Connection (Child_End));
      return Connection (Parent_End);
   elsif Process_ID = 0 then
      --  Child
      POSIX.IO.Close (Connection (Parent_End));

      case Child_End is
         when Read_End =>
            New_File := POSIX.IO.Duplicate_And_Close
              (File   => Connection (Child_End),
               Target => POSIX.IO.Standard_Input);
         when Write_End =>
            New_File := POSIX.IO.Duplicate_And_Close
              (File   => Connection (Child_End),
               Target => POSIX.IO.Standard_Output);
      end case;
      POSIX.IO.Close (Connection (Child_End));

      declare
         procedure Shell (Command : in     POSIX.POSIX_String) is
            procedure Exec_2_Arguments
              (Command : in Interfaces.C.Strings.chars_ptr;
               Name    : in Interfaces.C.Strings.chars_ptr;
               Arg_1   : in Interfaces.C.Strings.chars_ptr;
               Arg_2   : in Interfaces.C.Strings.chars_ptr;
               The_End : in Interfaces.C.Strings.chars_ptr);
            pragma Import (C, Exec_2_Arguments, "execl");
            Program_Ptr : Interfaces.C.Strings.chars_ptr :=
              Interfaces.C.Strings.New_String ("/bin/sh");
            Arg_1_Ptr   : Interfaces.C.Strings.chars_ptr :=
              Interfaces.C.Strings.New_String ("-c");
            Arg_2_Ptr   : Interfaces.C.Strings.chars_ptr :=
              Interfaces.C.Strings.New_String (POSIX.To_String (Command));
         begin
            Exec_2_Arguments (Command => Program_Ptr,
                              Name    => Program_Ptr,
                              Arg_1   => Arg_1_Ptr,
                              Arg_2   => Arg_2_Ptr,
                              The_End => Interfaces.C.Strings.Null_Ptr);
         end Shell;
      begin
         Shell (Command => Command);
         raise Program_Error;
      end;
   else
      raise Program_Error;
   end if;
end Open_Process;
