with POSIX.IO;
function Open_Process (Command : in POSIX.POSIX_String;
                       Mode    : in POSIX.IO.File_Mode)
                      return POSIX.IO.File_Descriptor;
