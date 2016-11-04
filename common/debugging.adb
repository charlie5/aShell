------------------------------------------------------------------------------
--
--  package Debugging (body)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.04.05 (Jacob Sparre Andersen)
--    Written.
--
--  1996.05.07 (Jacob S. A. and Jesper H. V. L.)
--    Modified formatting.
--
--  1996.07.26 (Jacob Sparre Andersen)
--    Removed the dependence of Debug on the redirection.
--    Removed the use of unbounded strings in the redirection routine.
--
--  1996.09.09 (Jacob Sparre Andersen)
--    Added exception reporting to the initialization.
--
--  1996.12.03 (Jacob Sparre Andersen)
--    Specified the Mode parameter for opening the Error_Log
--
--  1998.03.14 (Jacob Sparre Andersen)
--    Commented the calls to Ada.Text_IO.Flush out.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

package body Debugging is

   ---------------------------------------------------------------------------
   --  procedure Message:
   --
   --  Writes a message to Current_Error if debugging is activated.

   procedure Message (Item : in     String) is

      use Ada.Text_IO;

   begin --  Message
      if Debug then
         Put (Current_Error, Item);
         --  Flush (Current_Error);
      end if;
   exception
      when others =>
         Put_Line ("Unexpected exception raised in Debugging.Message");
         raise;
   end Message;

   ---------------------------------------------------------------------------
   --  procedure Message_Line:
   --
   --  Writes a message and a new line to Current_Error if debugging is
   --  activated.

   procedure Message_Line (Item : in     String) is

      use Ada.Text_IO;

   begin --  Message_Line
      if Debug then
         Put_Line (Current_Error, Item);
         --  Flush (Current_Error);
      end if;
   exception
      when others =>
         Put_Line ("Unexpected exception raised in Debugging.Message_Line");
         raise;
   end Message_Line;

   ---------------------------------------------------------------------------

   use Ada.Command_Line;
   use Ada.Text_IO;

   Error_Log               : File_Type;
   Log_To_Current_Error    : Boolean := True;
   Log_File_Argument_Index : Positive;

begin --  Debugging
   for Index in 1 .. Argument_Count - 1 loop
      if Argument (Index) = "-errorlog" then
         Log_To_Current_Error := False;
         Log_File_Argument_Index := Index + 1;
         exit;
      end if;
   end loop;

   if not Log_To_Current_Error then
      if Argument (Log_File_Argument_Index) = "-" then
         Set_Error (Standard_Output);
      else
         Create (File => Error_Log,
                 Name => Argument (Log_File_Argument_Index),
                 Mode => Out_File);
         Set_Error (Error_Log);
      end if;
   end if;
exception
   when others =>
      Put_Line (Current_Error,
                "Unexpected exception raised while initializing package " &
                  "Debugging.");
      raise;
end Debugging;
