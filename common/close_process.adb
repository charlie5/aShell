with Interfaces.C;
with Wait;
procedure Close_Process (Handle : in POSIX.IO.File_Descriptor) is
   Status  : aliased Interfaces.C.int;
   Process : Interfaces.C.int;
begin
   POSIX.IO.Close (File => Handle);
   Process := Wait (Status'Access);
end Close_Process;
