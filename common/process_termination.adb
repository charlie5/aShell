--  POSIX/Unix process primitives.

package body Process_Termination is
   type Real_Status_Record is
      record
         Signal    : POSIX_Signal;
         Core      : Boolean;
         Exit_Code : Exit_Status;
      end record;
   for Real_Status_Record use
      record
         Signal    at 0 range 0 .. 6;
         Core      at 0 range 7 .. 7;
         Exit_Code at 0 range 8 .. 15;
      end record;
   for Real_Status_Record'Size use Interfaces.C.int'Size;

   function Signal (Status_Record : in Interfaces.C.int)
     return POSIX_Signal is
      pragma Inline (Signal);
      Status : Real_Status_Record;
      for Status'Address use Status_Record'Address;
   begin
      return Status.Signal;
   end Signal;

   function Core_Dumped (Status_Record : in Interfaces.C.int)
     return Boolean is
      pragma Inline (Core_Dumped);
      Status : Real_Status_Record;
      for Status'Address use Status_Record'Address;
   begin
      return Status.Core;
   end Core_Dumped;

   function Exit_Code (Status_Record : in Interfaces.C.int)
     return Exit_Status is
      pragma Inline (Exit_Code);
      Status : Real_Status_Record;
      for Status'Address use Status_Record'Address;
   begin
      return Status.Exit_Code;
   end Exit_Code;
end Process_Termination;
