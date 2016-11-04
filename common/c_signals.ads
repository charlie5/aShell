with POSIX.C;

package C_Signals is
   --  Minimal package, importing the basic POSIX signal functionality from C.

   type Signal_Types is (Signal_Null,
                         Signal_Hangup,
                         Signal_Interrupt,
                         Signal_Quit,
                         Signal_Illegal_Instruction,
                         Signal_Trap,
                         Signal_Abort,
                         Signal_Bus_Error,
                         Signal_Floating_Point_Error,
                         Signal_Kill,
                         Signal_User_1,
                         Signal_Segmentation_Violation,
                         Signal_User_2,
                         Signal_Pipe_Write,
                         Signal_Alarm,
                         Signal_Terminate,
                         Signal_Child);
   for Signal_Types use (Signal_Null                   => 0,
                         Signal_Hangup                 => POSIX.C.SIGHUP,
                         Signal_Interrupt              => POSIX.C.SIGINT,
                         Signal_Quit                   => POSIX.C.SIGQUIT,
                         Signal_Illegal_Instruction    => POSIX.C.SIGILL,
                         Signal_Trap                   => POSIX.C.SIGTRAP,
                         Signal_Abort                  => POSIX.C.SIGABRT,
                         Signal_Bus_Error              => POSIX.C.SIGBUS,
                         Signal_Floating_Point_Error   => POSIX.C.SIGFPE,
                         Signal_Kill                   => POSIX.C.SIGKILL,
                         Signal_User_1                 => POSIX.C.SIGUSR1,
                         Signal_Segmentation_Violation => POSIX.C.SIGSEGV,
                         Signal_User_2                 => POSIX.C.SIGUSR2,
                         Signal_Pipe_Write             => POSIX.C.SIGPIPE,
                         Signal_Alarm                  => POSIX.C.SIGALRM,
                         Signal_Terminate              => POSIX.C.SIGTERM,
                         Signal_Child                  => POSIX.C.SIGCHLD);
   for Signal_Types'Size use POSIX.C.Int'Size;

   type Signal_Handler is
     access procedure (Signal : in     Signal_Types);
   pragma Convention (C, Signal_Handler);

   procedure Attach (Handler : in     Signal_Handler;
                     To      : in     Signal_Types);

   procedure Ignore (Signal : in     Signal_Types);

   procedure Attach_Default_Handler (To : in     Signal_Types);

   type Seconds is new POSIX.C.Unsigned;
   function Alarm (Wait : in Seconds) return Seconds;
   pragma Import (C, Alarm, "alarm");

   procedure Pause;
end C_Signals;
