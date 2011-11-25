with Ada.Text_IO;
with GNAT.Sockets;
with Stream_Shortcuts;
with Handlers;
with Protocol;
with Ada.Exceptions;

procedure Wormgameserver is
   use GNAT.Sockets;
   use Ada.Text_IO;
   use Handlers;


   Address        : Sock_Addr_Type;
   Server,Client  : Socket_Type;

begin
   Address := (Family => Family_Inet,
               Addr   => Any_Inet_Addr,
               Port   => 22144);

   -- start serveren
   Create_Socket (Server);
   Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
   Bind_Socket (Server, Address);
   Listen_Socket (Server);
   Put_Line ("Server: Server oppe");

   loop
      Accept_Socket (Server, Client, Address);
      Put_Line ("Server: Ny klient modtaget");
      declare
         S_Listener : Socket_Listener_Access;
      begin
         S_Listener := new Socket_Listener;
         S_Listener.Attach_Socket(Client);
      end;
   end loop;
end Wormgameserver;
