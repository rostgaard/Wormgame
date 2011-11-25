with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO,Ada.Integer_Text_IO;
with Stream_Shortcuts;
with Ada.Strings.Unbounded;

package body Handlers is
   use Ada.Strings.Unbounded;

   Global_Queue : Queue_Access;
   Queue_Closed : Boolean := False;

   -- Tilnærmelsesvis en beskyttet variabel, ref. Programming in Ada 2005 s. 545
   pragma Atomic(Queue_Closed);

   task body Socket_Listener is
        Handler : Handlers.Input_Handler_Access;
        Socket  : Socket_Type;
        Channel : Stream_Access;
        Buffer  : String (1 .. 256);
        Filled  : Natural;
        Request : Protocol.Client_Request;
   begin
        Handler := new Handlers.Input_Handler;
        Handler.Self_Reference (Handler);

      accept Attach_Socket (S : in Socket_Type) do
         Ada.Text_IO.Put_Line("Socket_Listener: Socket sat på");
           Socket  := S;
           Channel := Stream(Socket);
      end Attach_Socket;

        Handler.Attach_Socket (Socket);
        loop
           Stream_Shortcuts.Get_Line (Source => Channel,
                                     Item   => Buffer,
                                      Last   => Filled);
           Protocol.Parse_Line(Buffer (Buffer'First .. Filled), Request);
           Ada.Text_IO.Put("Socket_Listener (rå data): ");
           Ada.Text_IO.Put (Buffer (Buffer'First .. Filled) );
           Ada.Text_IO.New_Line;
           Handler.Receive(Request);
        end loop;
        Ada.Text_IO.Put("Socket_Listener: slutter");
   exception
      when E : others =>
         Ada.Text_IO.Put ("Socket_Listener: ");
         Ada.Text_IO.Put (Ada.Exceptions.Exception_Name(E));
         Ada.Text_IO.Put_Line (" opstod, kalder Log_Out rekursivt");
         Handler.Log_Out;
   end Socket_Listener;




   task body Input_Handler is
      Self                    : Input_Handler_Access;
      Current_Command_Handler : access Command_Handler'Class;
      Current_State           : Client_States := Not_Logged_In;
      Channel                 : Stream_Access;
      Socket                  : Socket_Type;
      Worm                    : Worm_Type_Access;
--      Direction_Input_Handler : Direction_Handler_Access;
   begin
      accept Self_Reference (R : in Input_Handler_Access) do
         Ada.Text_IO.Put_Line("Input_handler: Self_Reference sat");
         Self := R;
      end Self_Reference;

      accept Attach_Socket (S : Socket_Type ) do
         Socket  := S;
         Channel := Stream(Socket);
      end Attach_Socket;

      -- første command_handler er en login task
      Current_Command_Handler := new Login_Task;

      -- og sætter inputhandleren på
      Current_Command_Handler.Attach_Parent(Self);

      while Current_State /= Quitting loop
         select
            accept Change_State(State : in Client_States) do
               Ada.Text_IO.Put_Line("Input_handler: Change_State kaldt");
               case State is
                  when Logged_In =>
                     Ada.Text_IO.Put_Line("Input_handler: Tilstand ændret til Logged_In");
                     Current_Command_Handler := new Logged_In_Task;
                     Current_Command_Handler.Attach_Parent(Self);
                     Current_State := Logged_In;

                  when Quitting =>
                     Ada.Text_IO.Put_Line("Input_handler: Tilstand ændret til Not_Logged_In");
                     Current_Command_Handler := null;
                     Current_State := Quitting;

                     -- I kø
                  when In_Queue =>
                     Current_Command_Handler := new Queue_Task;
                     if Global_Queue = null or Queue_Closed then
                        Ada.Text_IO.Put_Line ("Input_handler: Ingen kø fundet, opretter en ny");
                        Global_Queue := new Game_Queue;
                     else
                        Ada.Text_IO.Put_Line ("Input_handler: Kø fundet, Tilslutter dén");
                     end if;
                     Current_State := In_Queue;
                     Ada.Text_IO.Put_Line ("Input_handler: Sætter i kø");
                     Global_Queue.Attach_Parent(Self);

                  -- I spil
                  when In_Game =>
                     Ada.Text_IO.Put_Line("Input_handler: Tilstand ændret Til In_Game");
                     Current_State := In_Game;
                  when others =>
                     Ada.Text_IO.Put_Line("Input_handler: Ugyldig tilstand ");
               end case;
            end Change_State;
         or
            accept Receive (R : in Client_Request) do
               Ada.Text_IO.Put_Line("Input_handler: Recieve kaldt");

               if Current_State /= In_Game then
                  Current_Command_Handler.Handle(R);
               else
                  case R.Command is
                     when Turn_Left =>
                        Ada.Text_IO.Put("Input_handler.Receive: Left");
                        Turn_Left(Worm);

                        Ada.Integer_Text_IO.Put(Worm.Number, Width => 2);
                        Ada.Text_IO.New_Line;

                     when Turn_Right =>
                        Turn_Right(Worm);
                        Ada.Text_IO.Put("Input_handler.Receive: Right");
                        Ada.Integer_Text_IO.Put(Worm.Number, Width => 2);
                        Ada.Text_IO.New_Line;

                     when Log_Out =>
                        Ada.Text_IO.Put("Input_handler.Receive: Log_Out");
                        Ada.Integer_Text_IO.Put(Worm.Number, Width => 2);
                        Kill(Worm);
                        Ada.Text_IO.New_Line;
                        Current_Command_Handler := null;
                        Current_State := Quitting;

                     when others =>
                        Ada.Text_IO.Put_Line("Input_handler.Receive: Other");
                        Ada.Integer_Text_IO.Put(Worm.Number, Width => 2);
                        Ada.Text_IO.New_Line;
                  end case;
               end if;
            end Receive;
         or
            accept Send (Msg : in String) do
               String'Write (Channel,Msg);
            end Send;
         or
            accept Log_Out do
               Ada.Text_IO.Put_Line("Input_handler: Log_Out kaldt");
               Current_State := Quitting;
            end Log_Out;
         or
            accept Attach_Worm (W : in Worm_Type_Access) do
               Ada.Text_IO.Put_Line("Input_handler: Attach kaldt");
               Worm := W;
            end Attach_Worm;
         end select;
      end loop;
      Ada.Text_IO.Put_Line("Input_handler: Slutter Task");
      Close_Socket(Socket);
   exception
      when E : others =>
         Ada.Text_IO.Put("Input_handler: ");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(E));
   end Input_Handler;


   -- --------------------------- --
   -- Log ind dialog med klienten --
   -- --------------------------- --
   task body Login_Task is
      Parent      : Input_Handler_Access;
      Local_State : Client_States := Not_Logged_In;
      Request     : Client_Request;
   begin
      accept Attach_Parent(P : in Input_Handler_Access) do
         Ada.Text_IO.Put_Line("Login_Task: Parent sat");
         Parent := P;
      end Attach_Parent;

      while Local_State = Not_Logged_In loop
         accept Handle (R : in Client_Request) do
            Request := R;
         end Handle;

         Ada.Text_IO.Put_Line("Login_Task: Handle kaldt");
         if Request.Command = Log_In then
            Ada.Text_IO.Put_Line("Login_Task: Log_In kommando modtaget");
            if User.Authenticate(To_String(Request.Argument(0)),
                                 To_String(Request.Argument(1))) then

               Ada.Text_IO.Put_Line("Login_Task: " &
                                    To_String(Request.Argument(0)) &
                                    " blev logget ind");

               -- sætter tilstanden til logget ind
               Local_State := Logged_In;
               -- Og giver klienten klienten svar tilbage
               Parent.Send (Protocol.Accept_Login (To_String (Request.Argument(0))) );
            end if;

         elsif Request.Command = New_User then
            if User.Create(To_String(Request.Argument(0)),
                                 To_String(Request.Argument(1))) then

               Ada.Text_IO.Put_Line("Login_Task: " &
                                    To_String(Request.Argument(0)) &
                                    " blev logget ind");

               -- sætter tilstanden til logget ind
               Local_State := Logged_In;
               -- Og giver klienten klienten svar tilbage
               Parent.Send (Protocol.Accept_Login (To_String (Request.Argument(0))) );
            end if;
            Ada.Text_IO.Put("Login_Task: New_User modtaget(");
         elsif Request.Command = Log_Out then
            Ada.Text_IO.Put_Line("Login_Task: Quit kommando modtaget");
            Local_State := Quitting;
         else
            Ada.Text_IO.Put_Line("Login_Task: Kommando Ikke tilladt her: ");
         end if;

      end loop;
      Parent.Change_State(Local_State);
      Ada.Text_IO.Put_Line("Login_Task: Slutter Task");
   exception
      when E : others =>
         Ada.Text_IO.Put("Login_Task: ");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(E));
   end Login_Task;


   -- ---------- --
   -- Logget ind --
   -- ---------- --
   task body Logged_In_Task is
      Parent      : Input_Handler_Access;
      Local_State : Client_States := Logged_In;
      Request     : Client_Request;
   begin
      accept Attach_Parent(P : in Input_Handler_Access) do
         Ada.Text_IO.Put_Line("Logged_In_Task: Parent sat");
         Parent := P;
      end Attach_Parent;

      while Local_State = Logged_In loop
         accept Handle (R : in Client_Request) do
            Request := R;
         end Handle;
            Ada.Text_IO.Put_Line("Logged_In_Task: Handle kaldt");

            if Request.Command = Join_Game then
               Ada.Text_IO.Put_Line("Logged_In_Task: Join_Game kommando modtaget");
               Local_State := In_Queue;

            elsif Request.Command = Request_Highscore then
               Ada.Text_IO.Put_Line("Logged_In_Task: Request_Highscore kommando modtaget");
               Parent.Send(Protocol.Highscore);

            elsif Request.Command = Log_Out then
               Ada.Text_IO.Put_Line("Logged_In_Task: Log ud kommando modtaget");
               Local_State := Quitting;
            else
               Ada.Text_IO.Put_Line("Logged_In_Task: Kommando Ikke tilladt her: ");
            end if;
      end loop;
         Ada.Text_IO.Put_Line("Logged_In_Task: kalder Parent.Change_state");
         Parent.Change_State(Local_State);
         Ada.Text_IO.Put_Line("Logged_In_Task: Slutter Task");
   exception
      when E :
        others =>
         Ada.Text_IO.Put("Logged_In_Task:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(E));
   end Logged_In_Task;

   -- ------- --
   -- Spil_kø --
   -- ------- --
   task body Game_Queue is
      Player       : Players;
      Index        : Natural := 0;
      Start        : Boolean := False;
      Game         : Game_Task_Access;

   begin

      Queue_Closed := False;
      while not Start loop
         select
            accept Attach_Parent (P : in Input_Handler_Access) do
               Index                     := Index+1;
               Ada.Text_IO.Put("Game_Queue: ny spiller modtaget, index: ");
               Ada.Integer_Text_IO.Put(Index);
               Ada.Text_IO.New_Line;


               Player (Index) := new Player_Type;

               Player (Index).IO_Handler := P;

            end Attach_Parent;
         or
            delay 2.0;
            Start := True;
         end select;
      end loop;
      Ada.Text_IO.Put_Line("Game_Queue: Kø slut, starter spil");

      Ada.Text_IO.Put_Line("Game_Queue: opretter nyt spil");
      Game := new Game_Task;

      for I in 1 .. Index loop
         Ada.Text_IO.Put("Game_Queue: sender startsignal index: ");
         Ada.Integer_Text_IO.Put(I, Width => 0);
         Ada.Text_IO.New_Line;
         Player (I).IO_Handler.Change_State (In_Game);

         Player (I).Worm := new Worm_Type;
         Player (I).Worm.Direction := Random_Course;
         Player (I).Worm.Number := I;


         Player (I).IO_Handler.Attach_Worm (Player(I).Worm);
      end loop;


      Game.Clients(Player,Index);

      -- luk køen
      Queue_Closed := True;
   exception
      when E :
        others =>
         Ada.Text_IO.Put("Game_Queue:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(E));
   end Game_Queue;


   -- ------------------------------------------- --
   -- Kø Task: Bruges til at logge beskeder sendt --
   -- mens klienten står i kø, hvis ikke dette    --
   -- bliver gjort, opstår der tasking error      --
   -- ------------------------------------------- --
   task body Queue_Task is
   begin
      loop
         select
            accept Attach_Parent (P : in Input_Handler_Access) do
               null;
            end Attach_Parent;
         or
            accept Handle (R : in Client_Request) do
               Ada.Text_IO.Put_Line ("Queue_Task: kommandoer ikke tilladt mens i kø");
            end Handle;
         end select;
      end loop;
      Ada.Text_IO.Put_Line ("Queue_Task: Slutter Queue_Task");
   end Queue_Task;


   -- ------------------------ --
   -- Spil Task: selve spillet --
   -- ------------------------ --
   task body Game_Task is
      use Worm_Position_Container;
      Player     : Players;
      Index      : Positive;
      The_Level  : Level_Access;
      Death_Toll : Natural := 0;       -- hvor mange orme er døde

      Temporary_Cake_Position : Position;
   begin
      --vent på klienter
      accept Clients(P : in Players; Idx : in Natural) do
         Player := P;
         Index  := Idx;
         Ada.Text_IO.Put_Line ("Game_Task: Klienter modtaget");
      end Clients;

      -- opret bane
      Ada.Text_IO.Put_Line ("Game_Task: Opretter bane");
      The_Level := new Level(64,64);
      Empty (The_Level);
      Ada.Text_IO.Put_Line ("Game_Task: Sætter mure omkring");
      Put_Walls_Around (The_Level);

      -- send bane
      for I in 1 .. Index loop
         Player (I).IO_handler.Send (Protocol.Send_Level(The_Level.Number_Of_Cols,
                                                         The_Level.Number_Of_Rows));

         Player (I).IO_handler.Send (Protocol.Blocked_Positions(The_Level));
         Player (I).IO_handler.Send (Protocol.Start_Game(I));

         Ada.Text_IO.Put_Line ("Game_Task: Opretter Orm");
         Prepend(Player (I).Worm.Worm_Body,Random_Worm_Position(The_Level,Player (I).Worm.Direction ));



         Ada.Text_IO.Put_Line ("Game_Task: Opretter Kager");
         Temporary_Cake_Position := Random_Cake_Position(The_Level);
         Set_Field (The_level,
                    Temporary_Cake_Position,
                    Kage);

         Ada.Text_IO.Put_Line ("Game_Task: Sender Kage positioner");
         Player (I).IO_Handler.Send(Protocol.Cake_Position(Temporary_Cake_Position));


      end loop;

      -- game
      Ada.Text_IO.Put_Line ("Game_Task: Starter spil");
      loop
         exit when Death_Toll = Index;
--         Ada.Text_IO.Put_Line ("Game_Task: Game Update");
--         Put_Level (The_Level);
         for I in 1 .. Index loop
            -- opdatér kun levende orme

            if Player (I).Worm.State = Alive then
               begin
                  Update_Worm (Player (I).Worm,The_Level);
               exception
                  when others =>
                     Ada.Text_IO.Put_Line ("ERROR in update worm ");
               end;

            elsif Player (I).Worm.State = Dead then
               Death_Toll := Death_Toll +1;
               Player (I).Worm.State := Observing;
            end if;

            -- hvis ormen er mæt, må den have spist en kage og så skal kages erstattes med en ny
            if Player (I).Worm.Full then
               Update_Worm (Player (I).Worm,The_Level);
               Temporary_Cake_Position := Random_Cake_Position(The_Level);
               Set_Field (The_level,
                          Temporary_Cake_Position,
                          Kage);
               Player (I).IO_Handler.Send(Protocol.Cake_Position(Temporary_Cake_Position));

               -- når en ny kage er lagt, er ormen ikke længere mæt ... ad ..
               Player (I).Worm.Full := False;

            end if;




            -- sender opdateringerne til alle:
  --          Ada.Text_IO.Put_Line ("Game_Task: Update broadcast");
            for Receiver in 1 .. Index loop
               begin
                  Player (Receiver).IO_Handler.Send
                    (Protocol.Worm_State
                     ((I),First_Element(Player (I).Worm.Worm_Body),Positive(Length (Player (I).Worm.Worm_Body))));
               exception
                  when others =>
                     Ada.Text_IO.Put_Line ("ERROR in sending update");
               end;

               end loop;
         end loop;
         delay 0.07;
      end loop;


      Ada.Text_IO.Put_Line ("Game_Task: Alle orme er døde, sender game over");
      for I in 1 .. Index loop
         Player (I).IO_Handler.Change_State (Logged_In);
         Player (I).IO_Handler.Send (Protocol.Game_Over);
      end loop;
      Ada.Text_IO.Put_Line ("Game_Task: Slutter Game_Task");
   exception
      when E :
        others =>
         Ada.Text_IO.Put("Game_Task:");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name(E));
   end Game_Task;
end Handlers;
