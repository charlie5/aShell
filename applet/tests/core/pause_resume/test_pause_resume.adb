with
     Shell,
     Ada.Text_IO;


procedure Test_Pause_Resume
is
   use Ada.Text_IO;
begin
   Shell.Open_Log ("my_Log.txt");

   Put_Line ("Begin 'Test_Pause_Resume' test.");
   New_Line (2);

   declare
      use Shell;
      ls : Shell.Process := Start (Program   => "ls",
                                      Arguments => (1 => (+"-R"),
                                                    2 => (+"/")));
   begin
      for i in 1 .. 5
      loop
         Put_Line (i'Image & Image (ls));
         delay 2.0;

         new_Line (2);
         Put_Line ("Pausing process ('ls -R /') ...");
         Pause (ls);
         Put_Line (i'Image & Image (ls));
         delay 2.0;

         Put_Line ("Resuming process ('ls -R /') ...");
         delay 2.0;
         Resume (ls);
         Put_Line (i'Image & Image (ls));
      end loop;

      delay 2.0;
      New_Line (2);
      Put_Line ("Killing 'ls -R /' process.");
      Kill (ls);
   end;

   Put_Line ("End 'Test_Pause_Resume' test.");

   shell.Close_Log;
end Test_Pause_Resume;
