with
     Shell;


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
