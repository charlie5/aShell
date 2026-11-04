with
     Shell.Commands.Safe,
     Ada.Task_Identification;


package body Late_Killers
is
   use Shell.Commands.Safe;

   The_Programs : Command_Array := [for Each in 1 .. 8 => Forge.To_Command ("sleep 25")];


   task type Killer
   is
      entry Set (Index : in Positive);
   end Killer;


   task body Killer
   is
      Which : Positive;
   begin
      accept Set (Index : in Positive)
      do
         Which := Index;
      end Set;

      while Ada.Task_Identification.Is_Callable (Ada.Task_Identification.Environment_Task)
      loop
         delay 0.001;
      end loop;

      delay 0.07;     -- About one pass of the spawn client, which then has more than one call to accept while shutting down.

      The_Programs (Which).Kill;
   end Killer;


   Killers : array (The_Programs'Range) of Killer;


   procedure Start_Programs
   is
   begin
      for Each in The_Programs'Range
      loop
         The_Programs (Each).Start;
         Killers (Each).Set (Each);
      end loop;
   end Start_Programs;

end Late_Killers;
