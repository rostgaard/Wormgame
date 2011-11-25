with Ada.Text_IO;


package body Worm is
   procedure Update_Worm (Worm : in Worm_Type_Access;
                          Lvl  : in Level_Access) is
      Last_Head_Position : Position;
      New_Head_Position  : Position;
      New_Head_Field     : Field;
   begin
      Last_Head_Position := First_Element(Worm.Worm_Body);
      case Worm.Direction is
         when North =>
            New_Head_Position.X :=  Last_Head_Position.X;
            New_Head_Position.Y := Last_Head_Position.Y + 1;
         when East =>
            New_Head_Position.X := Last_Head_Position.X + 1;
            New_Head_Position.Y :=  Last_Head_Position.Y;
         when South =>
            New_Head_Position.X :=  Last_Head_Position.X;
            New_Head_Position.Y := Last_Head_Position.Y - 1;
         when West =>
            New_Head_Position.X := Last_Head_Position.X - 1;
            New_Head_Position.Y :=  Last_Head_Position.Y;
      end case;

      Ada.Text_IO.Put_Line("Worm.Update_Worm: updating worm" & Natural'Image(Worm.Number));

      New_Head_Field := Lvl.Field_Value (New_Head_Position.X, New_Head_Position.Y);
      if New_Head_Field = Orm or New_Head_Field = Mur then
         Ada.Text_IO.Put_Line ("Orm død");
         Kill (Worm);
      else
         if New_Head_Field = Kage then
            Ada.Text_IO.Put_Line ("HAPS!!");
            Worm.Points := Worm.Points + 1;
            Worm.Full := True;
         elsif New_Head_Field = Luft then
            Set_Field (Lvl,
                       Last_Element (Worm.Worm_Body),
                       Luft);
            Delete_Last (Worm.Worm_Body);
         end if;

         Prepend(Worm.Worm_Body,
                 New_Head_Position);
         Set_Field (Lvl,
                    New_Head_Position,
                    Orm);
      end if;
   end Update_Worm;

   procedure Turn_Left (Worm : in Worm_Type_Access) is
   begin
      case Worm.Direction is
         when North => Worm.Direction := West;
         when East  => Worm.Direction := North;
         when South => Worm.Direction := East;
         when West  => Worm.Direction := South;
      end case;
   end Turn_Left;

   procedure Turn_Right (Worm : in Worm_Type_Access) is
   begin
      case Worm.Direction is
         when North => Worm.Direction := East;
         when East  => Worm.Direction := South;
         when South => Worm.Direction := West;
         when West  => Worm.Direction := North;
      end case;
   end Turn_Right;

   procedure Kill (Worm : in Worm_Type_Access) is
   begin
      Worm.State := Dead;
   end Kill;
end Worm;
