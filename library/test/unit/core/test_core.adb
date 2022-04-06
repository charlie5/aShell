with
     Core_Tests,
     Ahven.Framework,
     Ahven.Text_Runner;


procedure Test_Core
is
   use Ahven;

   Suite : Framework.Test_Suite := Core_Tests.Get_Test_Suite;
begin
   Text_Runner.Run (Suite);
end Test_Core;
