package Late_Killers
--
-- Library level tasks which kill their programs as the main program completes. These calls arrive just
-- as the spawn client begins to shut down, and none of them may be lost.
--
is
   procedure Start_Programs;
end Late_Killers;
