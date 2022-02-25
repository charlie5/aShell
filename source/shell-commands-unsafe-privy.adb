package body Shell.Commands.Unsafe.Privy
is

   function To_Command (Command_Line : in String) return Command
   is
   begin
      return Result : Command
      do
         Define (Result, Command_Line);

         Result.Owns_Output_Pipe := True;
         Result.     Output_Pipe := To_Pipe (Blocking => False);
         Result.      Error_Pipe := To_Pipe (Blocking => False);

      end return;
   end to_Command;



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



   function Output_Of (Pipe : in Shell.Pipe) return Data
   is
   begin
      delay 0.05;
      return Shell.Output_Of (Pipe);
   end Output_Of;


end Shell.Commands.Unsafe.Privy;
