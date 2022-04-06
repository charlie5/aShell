with
     Core_Tests,
     Ahven.Framework,
     Ahven.Text_Runner;


procedure Test_All
is
   use Ahven;

   Suite : Framework.Test_Suite := Framework.Create_Suite ("All");
begin
   Suite.Add_Static_Test (Core_Tests.Get_Test_Suite);
   Text_Runner.Run (Suite);
end Test_All;
