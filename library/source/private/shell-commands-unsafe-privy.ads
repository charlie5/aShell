package Shell.Commands.Unsafe.Privy
--
-- Private support for the spawn server.
--
is
   function  To_Command_With_Pipes return Command;     -- The server gives it a name and arguments.

   procedure Is_Within_A_Pipeline (The_Command : in out Command);
   function  Is_Within_A_Pipeline (The_Command : in     Command) return Boolean;

   procedure Stop (The_Command : in out Command);

   -- The server has other commands to serve, and sees this command end when it next checks it.
   --
   procedure Interrupt_Without_Waiting (The_Command : in out Command);

end Shell.Commands.Unsafe.Privy;
