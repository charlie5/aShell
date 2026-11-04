with
     Late_Killers,
     Ada.Text_IO;


procedure Test_Late_Kill
--
-- The program must end soon after this, as its programs are killed. This fails by hanging.
--
is
begin
   Late_Killers.Start_Programs;
   delay 1.0;

   Ada.Text_IO.Put_Line ("Main ending.");
end Test_Late_Kill;
