with Interfaces.C;

function Wait (Status_Ptr : access Interfaces.C.int) return Interfaces.C.int;
pragma Import (C, Wait, "wait");
