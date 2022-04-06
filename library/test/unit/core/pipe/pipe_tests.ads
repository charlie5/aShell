with
     Ahven.Framework;


package Pipe_Tests
is
   type Test is new Ahven.Framework.Test_Case with null record;

   function Get_Test_Suite return Ahven.Framework.Test_Suite;



private

   overriding
   procedure Initialize (T : in out Test);

end Pipe_Tests;
