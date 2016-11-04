with Interfaces.C;

function Fork return Interfaces.C.int;
pragma Import (C, Fork, "fork");
