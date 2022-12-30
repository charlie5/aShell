with
     Ada.Text_IO,
     Ada.Characters.Latin_1;


package body Shell.Testing
is


   procedure Pause_Til_Tester_Is_Ready
   is
      use Ada.Text_IO;
   begin
      New_Line;
      Put_Line ("Press <Enter> to run the next test.");

      declare
         Unused : String := Get_Line;
      begin
         Clear_Screen;
      end;
   end Pause_Til_Tester_Is_Ready;



   procedure Clear_Screen
   is
      use Ada.Text_IO;
   begin
      Put (Ada.Characters.Latin_1.ESC & "[2J");     -- Clear the screen.
   end Clear_Screen;


end Shell.Testing;
