with
     Shell.Commands.Safe,
     Ada.Streams,
     Ada.Text_IO;


procedure Test_Safe_Large_Input
--
-- Input larger than a pipe's capacity must reach the program, without blocking, and all of its output must be returned.
-- Any failure raises an exception.
--
is
   use Shell,
       Shell.Commands,
       Shell.Commands.Safe;

   use type Ada.Streams.Stream_Element_Array,
            Ada.Streams.Stream_Element_Offset;

   type Data_Access is access Data;

   The_Size  : constant Data_Offset := 1_000_000;
   The_Input : constant Data_Access := new Data (1 .. The_Size);


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

begin
   for i in The_Input'Range
   loop
      The_Input (i) := Ada.Streams.Stream_Element (i mod 251);
   end loop;

   declare
      The_Output : constant Data := Output_Of (Run ("cat", The_Input.all));
   begin
      Check ("Output is the same as the input", The_Output = The_Input.all);
   end;

   Check ("Pipeline gets all of the input", +Output_Of (Run ("cat | cat | wc -c", The_Input.all)) = "1000000" & ASCII.LF);
   Check ("Program which only reads a little", Output_Of (Run ("head -c 10", The_Input.all)) = The_Input (1 .. 10));
   Check ("Program which reads nothing", Output_Of (Run ("true", The_Input.all)) = No_Data);
end Test_Safe_Large_Input;
