with
     Shell,
     Ada.IO_Exceptions,
     Ada.Real_Time,
     Ada.Streams,
     Ada.Strings.Fixed,
     Ada.Text_IO;


package body Pipe_Tests
is
   use Ahven,
       Ahven.Framework;


   procedure Test_Uninitialized_Pipe
   is
      use type Shell.Pipe;
      Pipe : Shell.Pipe;
   begin
      Assert (Pipe = Shell.Null_Pipe,
              "Uninitialized pipe should be a null pipe.");
   end Test_Uninitialized_Pipe;



   procedure Test_Initialized_Pipe
   is
      use type Shell.Pipe;
      Pipe : constant Shell.Pipe := Shell.To_Pipe;
   begin
      Assert (Pipe /= Shell.Null_Pipe,
              "Initialized pipe should not be a null pipe.");
      Shell.Close (Pipe);
   end Test_Initialized_Pipe;



   procedure Test_Pipe_Image
   is
      use Shell;
      Pipe : Shell.Pipe;
   begin
      Assert (Image (Pipe) = Image (Null_Pipe),
              "Pipe image should be '" & Image (Null_Pipe) & "' not '" & Image (Pipe) & "'.");

      Pipe := Shell.To_Pipe;
      declare
         Image : constant String := Shell.Image (Pipe);
      begin
         Assert (Image = "(Write_End => 4, Read_End => 3)",
                 "Pipe image should be '(Write_End => 4, Read_End => 3)' not '" & Image & "'.");
      end;

      Shell.Close (Pipe);
   end Test_Pipe_Image;



   procedure Test_Pipe_Is_Readable
   is
      use Shell;
      Pipe : Shell.Pipe;
   begin
      declare
         Unused : Boolean;
      begin
         Unused := Is_Readable (Pipe);
         Fail ("Null_Pipe_Error expected.");
      exception
         when Null_Pipe_Error => null;
      end;

      Pipe := To_Pipe;
      Assert (Is_Readable (Pipe),
              "Initialized pipe should be readable.");

      Close (Pipe);
      Assert (not Is_Readable (Pipe),
              "Closed pipe should not be readable.");

      Pipe := To_Pipe;
      Close (Pipe, Only_Read_End => True);
      Assert (not Is_Readable (Pipe),
              "After closing 'read end' of pipe, the pipe should not be readable.");

      Close (Pipe);
   end Test_Pipe_Is_Readable;



   procedure Test_Pipe_Is_Writeable
   is
      use Shell;
      Pipe : Shell.Pipe;
   begin
      declare
         Unused : Boolean;
      begin
         Unused := Is_Writeable (Pipe);
         Fail ("Null_Pipe_Error expected.");
      exception
         when Null_Pipe_Error => null;
      end;

      Pipe := To_Pipe;
      Assert (Is_Writeable (Pipe),
              "Initialized pipe should be writeable.");

      Close (Pipe);
      Assert (not Is_Writeable (Pipe),
              "Closed pipe should not be writeable.");

      Pipe := To_Pipe;
      Close (Pipe, Only_Write_End => True);
      Assert (not Is_Writeable (Pipe),
              "After closing 'write end' of pipe, the pipe should not be writeable.");

      Close (Pipe);
   end Test_Pipe_Is_Writeable;



   procedure Test_Pipe_Is_Empty
   is
      use Shell;
      Pipe : Shell.Pipe;
   begin
      declare
         Unused : Boolean;
      begin
         Unused := Is_Writeable (Pipe);
         Fail ("Null_Pipe_Error expected.");
      exception
         when Null_Pipe_Error => null;
      end;


      Pipe := To_Pipe;
      Assert (Is_Empty (Pipe),
              "Initialized pipe should be empty.");

      Write_To (Pipe, Input => (1 => 0));
      Assert (not Is_Empty (Pipe),
              "Pipe should not be empty.");

      declare
         Output : Data := Output_Of (Pipe) with Unreferenced;
      begin
         Assert (Is_Empty (Pipe),
                 "Pipe should be empty.");
      end;

      Close (Pipe);
   end Test_Pipe_Is_Empty;



   procedure Test_Write_To_Pipe
   is
      use Shell;
      use type Data_Offset;

      Pipe : Shell.Pipe;
   begin
      declare
         Unused : Boolean;
      begin
         Write_To (Pipe, Input => (1 => 0));
         Fail ("Null_Pipe_Error expected.");
      exception
         when Null_Pipe_Error => null;
      end;


      Pipe := To_Pipe;
      Write_To (Pipe, Input => (1 => 0));

      Close (Pipe);
   end Test_Write_To_Pipe;



   procedure Test_Output_Of_Pipe
   is
      use Shell;
      Pipe : Shell.Pipe;
   begin
      begin
         declare
            Unused : Data := Output_Of (Pipe);
         begin
            null;
         end;

         Fail ("Null_Pipe_Error expected.");
      exception
         when Null_Pipe_Error => null;
      end;


      Pipe := To_Pipe;
      declare
         subtype   Null_Data is Shell.Data (1 .. 0);
         procedure Assert_Equals
           is new Ahven.Assert_Equal (Data_Type => Null_Data,
                                      Image     => Shell.To_String);

         Output : constant Data := Output_Of (Pipe);
      begin
         Assert_Equals (Output, No_Data,
                        "Unexpected output of pipe.");
      end;


      Write_To (Pipe, Input => (1 => 0));
      declare
         subtype   Datum is Shell.Data (1 .. 1);
         procedure Assert_Equals
           is new Ahven.Assert_Equal (Data_Type => Datum,
                                      Image     => Shell.To_String);

         Output : constant Data := Output_Of (Pipe);
      begin
         Assert_Equals (Output, (1 => 0),
                        "Bad output of pipe.");
      end;


      declare
         subtype   Null_Data is Shell.Data (1 .. 0);
         procedure Assert_Equals
           is new Ahven.Assert_Equal (Data_Type => Null_Data,
                                      Image     => Shell.To_String);

         Output : constant Data := Output_Of (Pipe);
      begin
         Assert_Equals (Output, No_Data,
                        "Unexpected output of pipe.");
      end;


      Close (Pipe);
   end Test_Output_Of_Pipe;



   procedure Test_Pipe_Close_Options
   is
      use Shell;
      Pipe : constant Shell.Pipe := To_Pipe;
   begin
      Close (Pipe,
             Only_Write_End => True,
             Only_Read_End  => True);
      Fail ("Pipe_Error expected. Only_Write_End and Only_Read_End are mutually exclusive.");

   exception
      when Pipe_Error =>
         Close (Pipe);
   end Test_Pipe_Close_Options;



   procedure Test_Pipe_Close
   is
      use Shell;
      Pipe : Shell.Pipe;
   begin
      begin
         Close (Pipe);
         Fail ("Null_Pipe_Error expected.");
      exception
         when Null_Pipe_Error => null;
      end;

      Pipe := To_Pipe;
      Close (Pipe);
      Close (Pipe);
   end Test_Pipe_Close;



   -- Descriptors are reused, so a closed pipe will have the same descriptor numbers as the next pipe.
   --
   procedure Test_Closed_Pipe
   is
      use Shell;
      use type Data_Offset;

      Closed      : constant Shell.Pipe := To_Pipe;
      Closed_Copy : constant Shell.Pipe := Closed;
   begin
      Close (Closed);

      declare
         Reused : constant Shell.Pipe := To_Pipe;
      begin
         Assert (Image (Reused) = Image (Closed),
                 "The new pipe should reuse the descriptors of the closed pipe.");

         Assert (not Is_Readable (Closed), "A closed pipe should not be readable.");
         Assert (not Is_Writeable (Closed), "A closed pipe should not be writeable.");
         Assert (not Is_Readable (Closed_Copy), "A copy of a closed pipe should not be readable.");

         begin
            Write_To (Closed, Input => (1 => 1));
            Fail ("Pipe_Error expected, when writing to a closed pipe.");
         exception
            when Pipe_Error => null;
         end;

         begin
            declare
               Unused : constant Boolean := Is_Empty (Closed, Timeout => 0.01);
            begin
               Fail ("Pipe_Not_Readable expected, when checking a closed pipe.");
            end;
         exception
            when Pipe_Not_Readable => null;
         end;

         Assert (Is_Empty (Reused, Timeout => 0.01),
                 "Writing to a closed pipe should not reach the new pipe.");

         Close           (Closed);
         Close           (Closed_Copy);
         Close_Write_End (Closed);

         Assert (Is_Readable (Reused), "Closing a closed pipe should leave the new pipe readable.");
         Assert (Is_Writeable (Reused), "Closing a closed pipe should leave the new pipe writeable.");

         Write_To (Reused, Input => (1 => 7));
         Assert (Output_Of (Reused)'Length = 1,
                 "The new pipe should still carry data.");

         Close (Reused);
      end;
   end Test_Closed_Pipe;



   procedure Test_Conversions
   is
      use Shell;
      use type Ada.Streams.Stream_Element_Array,
               Ada.Streams.Stream_Element_Offset;

      All_Bytes : Data (1 .. 256);
   begin
      for Each in All_Bytes'Range
      loop
         All_Bytes (Each) := Ada.Streams.Stream_Element (Each - 1);
      end loop;

      Assert (To_String (To_Data ("Some text")) = "Some text", "String should survive being converted to data.");
      Assert (To_Data (To_String (All_Bytes))   = All_Bytes,   "Every byte should survive being converted to a string.");
      Assert (String'(+Unbounded_String'(+"Some text")) = "Some text", "String should survive being converted to unbounded.");
      Assert (To_String (No_Data) = "",                        "No data should convert to an empty string.");
      Assert (To_Data ("")'Length = 0,                         "An empty string should convert to no data.");
      Assert (Nil_Strings'Length = 0,                          "'Nil_Strings' should be empty.");
   end Test_Conversions;



   procedure Test_Standard_Pipes
   is
      use Shell;
   begin
      Assert (Is_Readable  (Standard_Input),  "Standard input should be readable.");
      Assert (Is_Writeable (Standard_Output), "Standard output should be writeable.");
      Assert (Is_Writeable (Standard_Error),  "Standard error should be writeable.");

      Assert (not Is_Writeable (Standard_Input),  "Standard input should not be writeable.");
      Assert (not Is_Readable  (Standard_Output), "Standard output should not be readable.");

      Close (Standard_Output);
      Close (Standard_Input);
      Close_Write_End (Standard_Error);

      Assert (Is_Writeable (Standard_Output), "Closing a standard pipe should do nothing.");
      Assert (Is_Readable  (Standard_Input),  "Closing a standard pipe should do nothing.");
      Assert (Is_Writeable (Standard_Error),  "Closing a standard pipe should do nothing.");
   end Test_Standard_Pipes;



   procedure Test_Closing_One_End
   is
      use Shell;
      Pipe : constant Shell.Pipe := To_Pipe;
   begin
      Write_To (Pipe, To_Data ("Kept"));

      Close_Write_End (Pipe);
      Assert (not Is_Writeable (Pipe), "The write end should be closed.");
      Assert (Is_Readable (Pipe),      "The read end should still be open.");
      Assert (not Is_Empty (Pipe, Timeout => 0.01), "Data should remain after closing the write end.");
      Assert (To_String (Output_Of (Pipe)) = "Kept", "Data should be readable after closing the write end.");

      Assert (not Is_Empty (Pipe, Timeout => 0.01), "A pipe at its end should not read as empty, as a read would not block.");
      Assert (Output_Of (Pipe)'Length = 0,          "A pipe at its end should give no data.");

      Assert (Close_Write_End (Pipe), "The function form should give True.");

      Close (Pipe, Only_Read_End => True);
      Assert (not Is_Readable (Pipe), "The read end should be closed.");

      begin
         declare
            Unused : constant Boolean := Is_Empty (Pipe, Timeout => 0.01);
         begin
            Fail ("Pipe_Not_Readable expected.");
         end;
      exception
         when Pipe_Not_Readable => null;
      end;

      Assert (Output_Of (Pipe)'Length = 0, "A closed pipe should give no data.");
   end Test_Closing_One_End;



   procedure Test_Timeouts
   is
      use Shell,
          Ada.Real_Time;

      Pipe   : constant Shell.Pipe := To_Pipe;
      Before : constant Time       := Clock;
   begin
      Assert (Is_Empty (Pipe, Timeout => 0.1),       "A new pipe should be empty.");
      Assert (To_Duration (Clock - Before) >= 0.09,  "The timeout should be waited for.");
      Assert (To_Duration (Clock - Before) <  1.0,   "The timeout should end the wait.");

      Write_To (Pipe, To_Data ("x"));

      declare
         Ready_At : constant Time := Clock;
      begin
         Assert (not Is_Empty (Pipe, Timeout => 5.0),  "A pipe with data should not be empty.");
         Assert (To_Duration (Clock - Ready_At) < 1.0, "A pipe with data should not wait.");
         Assert (not Is_Empty (Pipe),                  "The default timeout should not wait for a pipe with data.");
      end;

      Close (Pipe);
   end Test_Timeouts;



   procedure Test_Non_Blocking_Pipe
   is
      use Shell,
          Ada.Real_Time;

      Pipe   : constant Shell.Pipe := To_Pipe (Blocking => False);
      Before : constant Time       := Clock;
   begin
      Assert (Output_Of (Pipe)'Length = 0,         "An empty pipe should give no data.");
      Assert (To_Duration (Clock - Before) < 1.0,  "An empty pipe should not block.");

      Write_To (Pipe, To_Data ("Data"));
      Assert (To_String (Output_Of (Pipe)) = "Data", "Data should be read.");

      Close (Pipe);
   end Test_Non_Blocking_Pipe;



   procedure Test_Large_Output
   is
      use Shell;
      use type Ada.Streams.Stream_Element_Array;

      Pipe  : constant Shell.Pipe := To_Pipe;
      Block : constant Data (1 .. 60_000) := [others => 7];
   begin
      Write_To (Pipe, Block);     -- A little under a pipe's capacity.
      Assert (Output_Of (Pipe) = Block, "All of the data should be read at once.");

      Close (Pipe);
   end Test_Large_Output;



   procedure Test_Streams
   is
      use Shell;

      subtype Text_Size is Natural range 0 .. 100;

      type Message (Size : Text_Size := 0) is
         record
            Value : Integer := 0;
            Text  : String (1 .. Size) := [others => ' '];
         end record;

      Pipe   : constant Shell.Pipe := To_Pipe;
      Stream : aliased Pipe_Stream := Shell.Stream (Pipe);

      Sent : constant Message := (Size => 5, Value => 42, Text => "Hello");
   begin
      Message'Output (Stream'Access, Sent);
      Message'Output (Stream'Access, (Size => 0, Value => -1, Text => ""));
      Integer'Write  (Stream'Access, 7);

      Assert (Message'Input (Stream'Access) = Sent, "A message should survive a stream.");
      Assert (Message'Input (Stream'Access).Value = -1, "A second message should follow the first.");

      declare
         Number : Integer;
      begin
         Integer'Read (Stream'Access, Number);
         Assert (Number = 7, "A value should follow the messages.");
      end;

      Close_Write_End (Pipe);

      begin
         Integer'Write (Stream'Access, 7);
         Fail ("Pipe_Error expected, when writing to a closed write end.");
      exception
         when Pipe_Error => null;
      end;

      begin
         declare
            Unused : Integer;
         begin
            Integer'Read (Stream'Access, Unused);
            Fail ("End_Error expected, when reading from a pipe at its end.");
         end;
      exception
         when Ada.IO_Exceptions.End_Error => null;
      end;

      Close (Pipe, Only_Read_End => True);

      begin
         declare
            Unused : Integer;
         begin
            Integer'Read (Stream'Access, Unused);
            Fail ("Pipe_Not_Readable expected, when reading from a closed read end.");
         end;
      exception
         when Pipe_Not_Readable => null;
      end;
   end Test_Streams;



   procedure Test_Short_Stream_Reads
   is
      use Shell;

      Reader : constant Shell.Pipe := To_Pipe (Blocking => False);
      Stream : aliased Pipe_Stream := Shell.Stream (Reader);

      Slow   : Process := Start (Program   => "bash",
                                 Arguments => [+"-c", +"printf abc; sleep 0.2; printf def"],
                                 Output    => Reader);
      Item   : String (1 .. 6);
   begin
      String'Read (Stream'Access, Item);     -- Arrives in two parts, with nothing to read between them.
      Assert (Item = "abcdef", "A stream read should wait for the whole item.");

      Wait_On (Slow);
      Close   (Reader);
   end Test_Short_Stream_Reads;



   procedure Test_Logging
   is
      use Shell,
          Ada.Text_IO;

      Name : constant String := "test_pipes_logging.log";
      File : File_Type;
      Pipe : constant Shell.Pipe := To_Pipe;
   begin
      begin
         Close_Log;
         Fail ("Program_Error expected, when closing a log which isn't open.");
      exception
         when Program_Error => null;
      end;

      begin
         Open_Log ("/non_existent_folder/log");
         Fail ("An error was expected, when opening a log in a missing folder.");
      exception
         when Ada.IO_Exceptions.Name_Error | Ada.IO_Exceptions.Use_Error => null;
      end;

      Open_Log (Name);     -- A failure to open must not leave the log unusable.

      begin
         Open_Log (Name);
         Fail ("Program_Error expected, when opening a log twice.");
      exception
         when Program_Error => null;
      end;

      Close (Pipe);
      Assert (Output_Of (Pipe)'Length = 0, "A closed pipe should give no data.");     -- This logs.

      Close_Log;

      Open (File, In_File, Name);
      Assert (not End_Of_File (File), "Something should be logged.");
      Assert (Ada.Strings.Fixed.Index (Get_Line (File), "not readable") /= 0, "The log should have the message.");
      Delete (File);
   end Test_Logging;



   overriding
   procedure Initialize (T : in out Test)
   is
   begin
      T.Set_Name ("Pipe Tests");

      T.Add_Test_Routine (Test_Uninitialized_Pipe'Access, "Uninitialized Pipe");
      T.Add_Test_Routine (Test_Initialized_Pipe  'Access,   "Initialized Pipe");
      T.Add_Test_Routine (Test_Pipe_Image        'Access, "Pipe Image");
      T.Add_Test_Routine (Test_Pipe_Is_Readable  'Access, "Pipe Is Readable");
      T.Add_Test_Routine (Test_Pipe_Is_Writeable 'Access, "Pipe Is Writeable");
      T.Add_Test_Routine (Test_Pipe_Is_Empty     'Access, "Pipe Is Empty");
      T.Add_Test_Routine (Test_Write_To_Pipe     'Access, "Write  To Pipe");
      T.Add_Test_Routine (Test_Output_Of_Pipe    'Access, "Output Of Pipe");
      T.Add_Test_Routine (Test_Pipe_Close_Options'Access, "Pipe Close Options");
      T.Add_Test_Routine (Test_Pipe_Close        'Access, "Pipe Close");
      T.Add_Test_Routine (Test_Closed_Pipe       'Access, "Closed Pipe");
      T.Add_Test_Routine (Test_Conversions       'Access, "Conversions");
      T.Add_Test_Routine (Test_Standard_Pipes    'Access, "Standard Pipes");
      T.Add_Test_Routine (Test_Closing_One_End   'Access, "Closing One End");
      T.Add_Test_Routine (Test_Timeouts          'Access, "Timeouts");
      T.Add_Test_Routine (Test_Non_Blocking_Pipe 'Access, "Non Blocking Pipe");
      T.Add_Test_Routine (Test_Large_Output      'Access, "Large Output");
      T.Add_Test_Routine (Test_Streams           'Access, "Streams");
      T.Add_Test_Routine (Test_Short_Stream_Reads'Access, "Short Stream Reads");
      T.Add_Test_Routine (Test_Logging           'Access, "Logging");
   end Initialize;



  function Get_Test_Suite return Ahven.Framework.Test_Suite
  is
     Suite : Test_Suite     := Ahven.Framework.Create_Suite ("Pipes");
     Test  : Pipe_Tests.Test;
  begin
      Suite.Add_Static_Test (Test);
      return Suite;
   end Get_Test_Suite;


end Pipe_Tests;
