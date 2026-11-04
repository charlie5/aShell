with
     Shell.Commands.Safe,

     POSIX.Process_Identification,
     Gnat.OS_Lib,

     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Text_IO;


procedure Test_Server_Lost
--
-- When the spawn server dies, all tasks waiting on commands must get a Command_Error, as must any later calls.
-- Any failure of this test raises an exception, or hangs.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;

   This_Process : constant String := Ada.Strings.Fixed.Trim (POSIX.Process_Identification.Image (POSIX.Process_Identification.Get_Process_ID),
                                                             Ada.Strings.Both);

   procedure Check (Test : in String;
                    Ok   : in Boolean)
   is
   begin
      if not Ok
      then
         raise Program_Error with Test & " failed.";
      end if;

      Ada.Text_IO.Put_Line (Test & " passed.");
   end Check;


   protected Tally
   is
      procedure Add (Is_Command_Error : in Boolean);
      function  Count return Natural;
   private
      The_Count : Natural := 0;
   end Tally;

   protected body Tally
   is
      procedure Add (Is_Command_Error : in Boolean)
      is
      begin
         if Is_Command_Error
         then
            The_Count := The_Count + 1;
         end if;
      end Add;

      function Count return Natural
      is (The_Count);
   end Tally;


   task type Waiter;

   task body Waiter
   is
   begin
      Run ("sleep 5");
      Tally.Add (Is_Command_Error => False);
   exception
      when Command_Error =>
         Tally.Add (Is_Command_Error => True);
      when E : others =>
         Ada.Text_IO.Put_Line ("Waiter got " & Ada.Exceptions.Exception_Name (E));
   end Waiter;


   function Raises_Command_Error (Call : access procedure) return Boolean
   is
   begin
      Call.all;
      return False;
   exception
      when Command_Error =>
         return True;
   end Raises_Command_Error;


   The_Polled : Safe.Command := Forge.To_Command ("sleep 5");

   procedure Run_True  is begin Run ("true");                    end Run_True;
   procedure Wait      is begin The_Polled.Wait_On;              end Wait;
   procedure Send_Text is begin The_Polled.Send (+"Text");       end Send_Text;
   procedure Interrupt is begin The_Polled.Interrupt;            end Interrupt;

   procedure Run_Pipeline
   is
      The_Pipeline : Command_Array := Forge.To_Commands ("true | true");
   begin
      Run (The_Pipeline);
   end Run_Pipeline;

begin
   declare
      Waiters : array (1 .. 3) of Waiter with Unreferenced;
      Success : Boolean;
   begin
      The_Polled.Start;
      delay 1.0;

      Gnat.OS_Lib.Spawn ("/usr/bin/pkill",
                         [1 => new String'("-KILL"),
                          2 => new String'("-P"),
                          3 => new String'(This_Process),
                          4 => new String'("-x"),
                          5 => new String'("ashell_spawn_se")],
                         Success);

      Check ("Server killed", Success);
   end;

   Check ("All waiting tasks get Command_Error", Tally.Count = 3);
   Check ("'Has_Terminated' is true",            The_Polled.Has_Terminated);
   Check ("'Normal_Exit' is false",              not The_Polled.Normal_Exit);
   Check ("'Wait_On' raises",                    Raises_Command_Error (Wait'Access));
   Check ("Later 'Run' raises",                  Raises_Command_Error (Run_True'Access));
   Check ("Later pipeline 'Run' raises",         Raises_Command_Error (Run_Pipeline'Access));
   Check ("Later 'Send' raises",                 Raises_Command_Error (Send_Text'Access));
   Check ("Later 'Interrupt' raises",            Raises_Command_Error (Interrupt'Access));

   Stop_Spawn_Client;
   Check ("'Stop_Spawn_Client' returns", True);
end Test_Server_Lost;
