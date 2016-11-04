with POSIX.C;
with POSIX.Process_Identification;

package EUP.UTMP is
   --  This file has been put together by hand.
   --  It really ought to be generated based on local system parameters.

   File_Name : constant String := "/var/run/utmp";

   type Login_Types is (UT_Unknown, Run_Level, Boot_Time, New_Time, Old_Time,
                        Init_Process, Login_Process, User_Process,
                        Dead_Process, Accounting);
   for Login_Types use (UT_Unknown    => 0,
                        Run_Level     => 1,
                        Boot_Time     => 2,
                        New_Time      => 3,
                        Old_Time      => 4,
                        Init_Process  => 5,
                        Login_Process => 6,
                        User_Process  => 7,
                        Dead_Process  => 8,
                        Accounting    => 9);
   for Login_Types'Size use POSIX.C.short'Size;

   Line_Size : constant Positive :=  32;
   Name_Size : constant Positive :=  32;
   Host_Size : constant Positive := 256;

   type Exit_Status is
      record
         Termination : POSIX.C.Short;
         Exit_Code   : POSIX.C.Short;
      end record;
   for Exit_Status'Size use 32;

   type int32_t is range -2**31 .. (2**31)-1;
   for int32_t'Size use 32;

   type addr_v6_t is array (1 .. 4) of int32_t;
   for addr_v6_t'Size use 128;

   type Instance is
      record
         Login_Type : Login_Types;
         PID        : POSIX.Process_Identification.Process_ID;
         Line       : POSIX.POSIX_String (1 .. Line_Size);
         ID         : POSIX.POSIX_String (1 .. 4);
         User       : POSIX.POSIX_String (1 .. Name_Size);
         Host       : POSIX.POSIX_String (1 .. Host_Size);
         Exit_Code  : Exit_Status;
         Session    : POSIX.C.long;
         TV         : POSIX.C.struct_timeval;
         Addr_V6    : addr_v6_t;
         Unused     : POSIX.POSIX_String (1 .. 20);
      end record;
   for Instance'Size use 3168; -- 3104; -- 3072;
end EUP.UTMP;
