package body EUP.Sockets is
   function To_IP_Address (Host : in String) return IP_Address_Type is
      function Is_An_IP_Address (Host : in String) return Boolean is
      begin
         if Host'Length < 7 or Host'Length > 15 Then
            return False;
         else
            for Index in Host'Range loop
               case Host (Index) is
                  when '.' | '0' .. '9' =>
                     null;
                  when others =>
                     return False;
               end case;
            end loop;
            return True;
         end if;
      end Is_An_IP_Address;
   begin
      if Is_An_IP_Address (Host) then
         return GNAT.Sockets.Inet_Addr (Host);
      else
         return GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name (Host),
                                        1);
      end if;
   end To_IP_Address;

   function Make_Server
     (Port         : in GNAT.Sockets.Port_Type;
      Mode         : in GNAT.Sockets.Mode_Type := GNAT.Sockets.Socket_Stream;
      Queue_Length : in Positive := 15) return Socket_Type is
      use type GNAT.Sockets.Mode_Type;
      Server : Socket_Type;
   begin
      GNAT.Sockets.Create_Socket (Socket => Server,
                                  Mode   => Mode);
      GNAT.Sockets.Set_Socket_Option
        (Socket => Server,
         Option => (Name    => GNAT.Sockets.Reuse_Address,
                    Enabled => True));
      GNAT.Sockets.Bind_Socket
        (Socket  => Server,
         Address => (Family => GNAT.Sockets.Family_Inet,
                     Addr   => GNAT.Sockets.Any_Inet_Addr,
                     Port   => Port));

      if Mode = GNAT.Sockets.Socket_Stream then
         GNAT.Sockets.Listen_Socket (Socket => Server,
                                     Length => Queue_Length);
      end if;

      return Server;
   end Make_Server;

   function Connect_To_Server (Host : in String;
                               Port : in GNAT.Sockets.Port_Type)
     return Socket_Type is
      Client : Socket_Type;
      Server : GNAT.Sockets.Sock_Addr_Type :=
                 (Family => GNAT.Sockets.Family_Inet,
                  Addr   => To_IP_Address (Host),
                  Port   => Port);
   begin
      GNAT.Sockets.Create_Socket (Socket => Client);
      GNAT.Sockets.Set_Socket_Option
        (Socket => Client,
         Option => (Name    => GNAT.Sockets.Reuse_Address,
                    Enabled => True));
      GNAT.Sockets.Connect_Socket (Socket => Client,
                                   Server => Server);
      return Client;
   end Connect_To_Server;
end EUP.Sockets;
