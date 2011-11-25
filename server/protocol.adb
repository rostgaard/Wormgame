
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
package body Protocol is
      use Ada.Strings;
      use Ada.Characters.Handling;
      use Ada.Strings.Fixed;

      procedure Parse_Line(Line   : in     String;
                           Result :    out Client_Request) is
      use Ada.Text_IO;
      use Ada.Integer_Text_IO;
      Offset : Positive :=2;
      Resnum : Natural  :=0;
   begin
      -- Hvis linien er over ét tegn lang betyder det argumenter
      if Line'Length > 1 then
         Result.Command := Command_Of(Line(Line'First));
         -- men kun hvis tegn 2 er en seperator
         if Line(Line'First + 1) = Seperator then
            for I in Offset .. Line'Length loop
               if Line(I) = Seperator and Offset /= I then
                  Result.Argument(Resnum) :=
                    To_Unbounded_String(Line(Offset+1 .. I-1));
                  Offset := I;
                  Resnum := Resnum + 1;

               elsif I = Line'Length then
                  Result.Argument(Resnum) :=
                    To_Unbounded_String(Line(Offset+1 .. I));
                  Offset := I;
                  Resnum := Resnum + 1;
               end if;
            end loop;
         end if;

      elsif Line'Length = 1 then
        -- Put("længde lig 1");
         Result.Command := Command_Of(Line(Line'First));
      else
         Put_Line ("Protocol.Parse_Line: Fejl! (måske tom linie)");
      end if;
   end Parse_Line;


   -- -- Gammel metode til at finde karakterer
--     function Character_Of (M : in Accepted_Messages) return Character is
--        Found : Boolean := False;
--     begin
--        for Value in Character loop
--           if M = Command_Of(Value) then
--              return Value;
--           end if;
--        end loop;
--        return '0';
--     end Character_Of;


   -- ----------------- --
   -- server til klient --
   -- ----------------- --
   function Accept_Login (Username : in String) return String is
   begin
      return (Character_Of (Logged_In) & Seperator & Username & Line_Feed);
   end Accept_Login;

   -- send ny bane
   function Send_Level (Width : in Positive; Heigth : in Positive) return String is
   begin
      return (Character_Of (New_Level) &
              Seperator &
              Trim (Source => (Positive'Image(Width)),
                    Side   => Both) &
              Seperator &
              Trim (Source => (Positive'Image(Heigth)),
                    Side   => Both) &
              Line_Feed);
   end Send_Level;

   function Blocked_Positions (Lvl : in Level_Access) return String is
      use Ada.Text_IO;
      Buffer : Unbounded_String;
   begin

      -- den nordlige og sydlige mur (springer den første og sidste over);
      for I in 2 .. Lvl.Number_Of_Cols-1 loop
         if Lvl.Field_Value (Lvl.Field_Value'First, I) = Mur then
            Buffer := (Buffer &
                       Seperator &
                       Trim (Source => Positive'Image(I),
                             Side   => Left) &
                       Seperator &
                       Trim (Source => Positive'Image(Lvl.Field_Value'First),
                             Side   => Left));
         end if;
         if Lvl.Field_Value (Lvl.Field_Value'Last, I) = Mur then
            Buffer := (Buffer &
                       Seperator &
                       Trim (Source => Positive'Image(I),
                             Side   => Left) &
                       Seperator &
                       Trim (Source => Positive'Image(Lvl.Field_Value'Last),
                             Side   => Left));
         end if;
      end loop;
      -- den vestlige og østlige mur
      for I in 1 .. Lvl.Number_Of_Rows loop
         if Lvl.Field_Value (I,1) = Mur then
            Buffer := (Buffer &
                       Seperator & "1" &
                       Seperator &
                       Trim (Source => Positive'Image(I),
                             Side   => Left));
         end if;
         if Lvl.Field_Value (I, Lvl.Number_Of_Cols) = Mur then
            Buffer := (Buffer &
                       Seperator &
                       Trim (Source => Positive'Image(Lvl.Number_Of_Cols),
                             Side   => Left) &
                       Seperator &
                       Trim (Source => Positive'Image(I),
                             Side   => Left));
         end if;
      end loop;

      return (Character_Of (Blocked) &
              To_String (Buffer & Line_Feed));
   end Blocked_Positions;


   function Cake_Position (C : in Position) return String is
   begin
      return (Character_Of (Cake) &
              Seperator &
              Trim (Source => Positive'Image(C.X),
                    Side   => Left) &
              Seperator &
              Trim (Source => Positive'Image(C.Y),
                    Side   => Left) &
              Line_Feed);
   end Cake_Position;

   function Empty_Positions (Lvl : in Level_Access) return String is
   begin
      return (Character_Of (Empty) & Line_Feed);
   end Empty_Positions;


   function Start_Game (Idx : in Natural) return String is
   begin
      return (Character_Of (Game_Start) &
              Seperator &
              Trim (Source => Positive'Image(Idx),
                    Side   => Left) &
              Line_Feed);
   end Start_Game;

   function Worm_State (Number : in Positive;
                        Pos    : in Position;
                        Len    : in Positive) return String is

   begin
      return (Character_Of (Worm) &
              Seperator &
              Trim (Source => Positive'Image(Number),
                    Side   => Left) &
              Seperator &
              Trim (Source => Positive'Image(Pos.X),
                    Side   => Left) &
              Seperator &
              Trim (Source => Positive'Image(Pos.Y),
                    Side   => Left) &
              Seperator &
              Trim (Source => Positive'Image(Len),
                    Side   => Left) &
              Line_Feed);
   end Worm_State;

   function Death_Of (Worm_Number : in Positive) return String is
   begin
      return (Character_Of (Kill) &
              Trim (Source => Positive'Image(Worm_Number),
                    Side   => Left) &
              Line_Feed);
   end Death_Of;

   function Game_Over return String is
   begin
      return (Character_Of (End_Game) & Line_Feed);
   end Game_Over;


   -- statiske værdier indtil implementeret
   function Highscore return String is
   begin
      return (Character_Of (Send_Highscore) &
              Seperator &
              "Superormen" &
              Seperator &
              "1002" &
              Seperator &
              "Ikke-lige-så-superormen"  &
              Seperator &
              "200"  &
              Seperator &
              "Highscore"  &
              Seperator &
              "200"  &

              Line_Feed);
   end HighScore;

   function Message (A_Message : in String) return String is
   begin
      return (Character_Of (Message) &
              Seperator &
              A_Message &
              Line_Feed);
   end Message;

end Protocol;

