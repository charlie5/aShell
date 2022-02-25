package POSIX.Process_Primitives.Extensions is

   procedure Start_Process_Search
     (Child             :    out POSIX.Process_Identification.Process_ID;
      Filename          : in     POSIX.Filename;
      Working_Directory : in     String;
      Template          : in     Process_Template;
      Arg_List          : in     POSIX.POSIX_String_List :=
                                   POSIX.Empty_String_List);

end POSIX.Process_Primitives.Extensions;
