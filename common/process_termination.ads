--  POSIX/Unix process primitives.

with Interfaces.C;

package Process_Termination is
   type Exit_Status is range 2#0000_0000# .. 2#1111_1111#;
   for Exit_Status'Size use 8;

   type POSIX_Signal is range 2#0000_0000# .. 2#0111_1111#;
   for POSIX_Signal'Size use 8;

   function Signal (Status_Record : in Interfaces.C.int) return POSIX_Signal;

   function Core_Dumped (Status_Record : in Interfaces.C.int) return Boolean;

   function Exit_Code (Status_Record : in Interfaces.C.int) return Exit_Status;
end Process_Termination;
