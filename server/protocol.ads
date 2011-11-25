with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Wormlevel;
package Protocol is
   use Ada.Strings.Unbounded;
   use Wormlevel;

   Seperator : Character := ',';
   Line_Feed : Character := Ada.Characters.Latin_1.LF;


   -- Accepterede beskeder
   type Accepted_Messages is (New_User,          -- C,Bruger,Password   - Opret Bruger
                              Log_In,            -- U,Bruger,Password   - Login
                              Log_Out,           -- Q                   - Log ud
                              Join_Game,         -- J                   - Nyt spil
                              Turn_Left,         -- L                   - Venstre
                              Turn_Right,        -- R                   - Højre
                              Request_Highscore, -- H                   - Vis High Score
                              Not_Implemented,   -- Intern Fejlbesked
                              Logged_In,         -- A,Brugernavn        - Logget ind / Oprettet
                              New_Level,         -- T,Bredde, Højde     - Ny Tom Bane
                              Blocked,           -- B,X,Y{,X,Y}         - Blokerede Positioner
                              Cake,              -- F,X,Y{,X,Y}         - KAGE (slurp :-P )
                              Empty,             -- E,X,Y{,X,Y}         - Tomme felter
                              Game_Start,        -- N,Slangenummer      - Spillet Starter
                              Worm,              -- S,-||-,hX,hY,Længde - Slange
                              Kill,              -- D,slangenummer      - Dræb Orm
                              End_Game,          -- G                   - Slut Spil
                              Send_Highscore,    -- P,Brugernavn, Point - High score
                              Message);          -- M,Besked            - Send besked

   -- Hvilke typer skal være tilladt af server og klient
   subtype Client_Messages is Accepted_Messages range New_User .. Not_Implemented;
   subtype Server_Messages is Accepted_Messages range Logged_In .. Message;

   -- argumeter bruger vi i vores forespørgsler og svar, egentlig burde det være en String'access
   type Arguments is array(0 .. 8) of UnBounded_String;

   -- Klient forespørgsel record
   type Client_Request is
      record
         Command     : Client_Messages;
         Argument    : Arguments;
      end record;

   -- Vores array giver hver eneste karakter en værdi, så vi slipper for at checke på værdien senere
   Command_Of : constant array (Character) of Client_Messages :=
     ( 'C'    => New_User,
       'U'    => Log_In,
       'Q'    => Log_Out,
       'J'    => Join_Game,
       'L'    => Turn_Left,
       'R'    => Turn_Right,
       'H'    => Request_Highscore,
       others => Not_Implemented);

   -- Tilsvarende en tildeling af vores server beskeder
   Character_Of : constant array (Server_Messages) of Character :=
     ( Logged_In       => 'A',
       New_Level       => 'T',
       Blocked         => 'B',
       Cake            => 'F',
       Empty           => 'E',
       Game_Start      => 'N',
       Worm            => 'S',
       Kill            => 'D',
       End_Game        => 'G',
       Send_Highscore  => 'P',
       Message         => 'M');


--   function Character_Of (M : in Accepted_Messages) return Character;

   -- abstraktionsmetode til at konvertere linier (rå forespørselser) til noget formaliseret
   procedure Parse_Line (Line   : in     String;
                         Result :    out Client_Request);

   -- ---------- --
   -- serversvar --
   -- ---------- --
   function Accept_Login (Username : in String) return String;
   function Send_Level (Width : in Positive; Heigth : in Positive) return String;
   function Blocked_Positions (Lvl : in Level_Access) return String;
   function Cake_Position (C : in Position) return String;
   function Empty_Positions (Lvl : in Level_Access) return String;
   function Start_Game (Idx : in Natural) return String;
   function Worm_State (Number : in Positive;
                        Pos    : in Position;
                        Len    : in Positive) return String;

   function Death_Of (Worm_Number : in Positive) return String;
   function Game_Over return String;
   function Highscore return String;
   -- dette kald er ikke en del af den oprindelige protokol
   function Message (A_Message : in String) return String;

end Protocol;

