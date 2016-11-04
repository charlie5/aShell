with Interfaces.C, POSIX.C;
with C_Signals;
procedure Child_Waiter (Signal : in     C_Signals.Signal_Types) is
   type pid_t is new Interfaces.C.int;
   function waitpid (Process_ID : pid_T;
                     Status     : access Interfaces.C.int;
                     Options    : Interfaces.C.int) return pid_t;
   pragma Import (C, waitpid, "waitpid");
begin
   while waitpid (-1, null, POSIX.C.WNOHANG) > 0 loop
      null;
   end loop;
end Child_Waiter;
