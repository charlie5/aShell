with
     Ada.Text_IO;


procedure Shell.Test_Stale_Ready
--
-- A pipe found ready may be closed before it is read, and its descriptors may then be given to a new pipe.
-- It is a child of 'Shell' as these operations are in the private part, for the use of the commands.
--
is
   use type POSIX.IO.File_Descriptor;

   Old_Pipe : constant Pipe := To_Pipe (Blocking => False);
   Ready    : Pipe_Id_Sets.Set;
begin
   Write_To (Old_Pipe, To_Data ("Old"));
   Wait_For_Output ([1 => Old_Pipe], Timeout => 1.0, Ready => Ready);

   if not Ready.Contains (Old_Pipe.Id)
   then
      raise Program_Error with "The pipe should be ready.";
   end if;

   Close (Old_Pipe);

   if To_String (Output_If_Ready (Old_Pipe, Ready)) /= ""
   then
      raise Program_Error with "A closed pipe should give no data.";
   end if;

   Ada.Text_IO.Put_Line ("Reading a closed pipe which was ready gives no data passed.");

   declare
      New_Pipe : constant Pipe := To_Pipe (Blocking => False);
   begin
      if New_Pipe.Read_End /= Old_Pipe.Read_End
      then
         raise Program_Error with "The new pipe should reuse the old read end, for this test to mean anything.";
      end if;

      Write_To (New_Pipe, To_Data ("New"));

      if To_String (Output_If_Ready (Old_Pipe, Ready)) /= ""
      then
         raise Program_Error with "A closed pipe should not read the pipe which reused its descriptor.";
      end if;

      if To_String (Output_Of (New_Pipe)) /= "New"
      then
         raise Program_Error with "The new pipe should keep its data.";
      end if;

      Close (New_Pipe);
   end;

   Ada.Text_IO.Put_Line ("A closed pipe doesn't read a reused descriptor passed.");
   Ada.Text_IO.Put_Line ("End test.");
end Shell.Test_Stale_Ready;
