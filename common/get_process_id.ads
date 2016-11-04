with Interfaces.C;

function Get_Process_ID return Interfaces.C.int;
pragma Import (C, Get_Process_ID, "getpid");
