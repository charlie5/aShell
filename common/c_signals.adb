with
  Ada.Unchecked_Conversion,
  POSIX.C,
  POSIX.Implementation,
  System;

package body C_Signals is

   function To_int is
      new Ada.Unchecked_Conversion (Source => Signal_Types,
                                    Target => POSIX.C.int);

   subtype sighandler_t is System.Address;

   function To_sighandler_t is
      new Ada.Unchecked_Conversion (Source => Signal_Handler,
                                    Target => sighandler_t);
   function To_sighandler_t is
      new Ada.Unchecked_Conversion (Source => POSIX.C.int,
                                    Target => sighandler_t);
   function To_int is
      new Ada.Unchecked_Conversion (Source => sighandler_t,
                                    Target => POSIX.C.int);

   function C_Signal (signum  : POSIX.C.int;
                    handler : sighandler_t) return sighandler_t;
   pragma Import (C, C_Signal, "signal");

   procedure Attach (Handler : in     Signal_Handler;
                     To      : in     Signal_Types) is
      Result : sighandler_t;
   begin
      Result := C_Signal (signum  => To_int (To),
                        handler => To_sighandler_t (Handler));
      POSIX.Implementation.Check (To_int (Result));
   end Attach;

   procedure Ignore (Signal : in     Signal_Types) is
      Result : sighandler_t;
   begin
      Result := C_Signal (signum  => To_int (Signal),
                          handler => To_sighandler_t (POSIX.C.SIG_IGN));
      POSIX.Implementation.Check (To_int (Result));
   end Ignore;

   procedure Attach_Default_Handler (To : in     Signal_Types) is
      Result : sighandler_t;
   begin
      Result := C_Signal (signum  => To_int (To),
                          handler => To_sighandler_t (POSIX.C.SIG_DFL));
      POSIX.Implementation.Check (To_int (Result));
   end Attach_Default_Handler;

   function C_Pause return POSIX.C.int;
   pragma Import (C, C_Pause, "pause");

   procedure Pause is
      pragma Inline (Pause);
      Dummy : POSIX.C.int;
   begin
      Dummy := C_Pause;
   end Pause;
end C_Signals;
