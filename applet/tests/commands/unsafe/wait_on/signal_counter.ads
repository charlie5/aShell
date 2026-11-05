with
     Interfaces.C;


package Signal_Counter
--
-- A signal handler must not refer to anything in a frame which may have ended.
--
is
   Handled : Interfaces.C.int := 0
     with Volatile;

   procedure Handler (Signal : in Interfaces.C.int)
     with Convention => C;

end Signal_Counter;
