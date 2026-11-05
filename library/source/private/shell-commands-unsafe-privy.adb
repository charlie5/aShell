package body Shell.Commands.Unsafe.Privy
is

   function To_Command_With_Pipes return Command
   is
   begin
      return Result : Command
      do
         Result.Owns_Output_Pipe := True;
         Result.     Output_Pipe := To_Pipe (Blocking => False);
         Result.      Error_Pipe := To_Pipe (Blocking => False);
      end return;
   end To_Command_With_Pipes;



   procedure Is_Within_A_Pipeline (The_Command : in out Command)
   is
   begin
      The_Command.Is_Within_A_Pipeline := True;
   end Is_Within_A_Pipeline;



   function Is_Within_A_Pipeline (The_Command : in Command) return Boolean
   is
   begin
      return The_Command.Is_Within_A_Pipeline;
   end Is_Within_A_Pipeline;



   procedure Stop (The_Command : in out Command)
   is
   begin
      Unsafe.Stop (The_Command);
   end Stop;



   procedure Interrupt_Without_Waiting (The_Command : in out Command)
   is
   begin
      Send_Interrupt (The_Command.Process);
   end Interrupt_Without_Waiting;


end Shell.Commands.Unsafe.Privy;
