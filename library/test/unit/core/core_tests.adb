with
     Pipe_Tests,
     Process_Tests;


package body Core_Tests
is
   use Ahven,
       Ahven.Framework;

   function Get_Test_Suite return Framework.Test_Suite
   is
      Suite : Test_Suite := Framework.Create_Suite ("Core");
   begin
      Suite.Add_Static_Test (Pipe_Tests   .Get_Test_Suite);
      Suite.Add_Static_Test (Process_Tests.Get_Test_Suite);

      return Suite;
   end Get_Test_Suite;

end Core_Tests;
