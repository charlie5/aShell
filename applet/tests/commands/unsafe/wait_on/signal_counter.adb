package body Signal_Counter
is
   use type Interfaces.C.int;


   procedure Handler (Signal : in Interfaces.C.int)
   is
      pragma Unreferenced (Signal);
   begin
      Handled := Handled + 1;
   end Handler;

end Signal_Counter;
