with GNAT.Sockets;

package EUP.Sockets is
   subtype Socket_Type     is GNAT.Sockets.Socket_Type;
   subtype IP_Address_Type is GNAT.Sockets.Inet_Addr_Type;

   function To_IP_Address (Host : in String) return IP_Address_Type;

   function Make_Server
     (Port         : in GNAT.Sockets.Port_Type;
      Mode         : in GNAT.Sockets.Mode_Type := GNAT.Sockets.Socket_Stream;
      Queue_Length : in Positive := 15) return Socket_Type;

   function Connect_To_Server
     (Host : in String;
      Port : in GNAT.Sockets.Port_Type) return Socket_Type;
end EUP.Sockets;
