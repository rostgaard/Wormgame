package body Wormlevel is

   procedure Empty (L : in Level_access) is
   begin
      for Y in L.Field_Value'range loop
         for X in 1 .. L.Number_Of_Cols loop
            L.Field_value(X,Y) := Luft;
         end loop;
      end loop;
   end Empty;

   procedure Put_Level (L : in Level_access) is
     use Ada.Text_IO,Ada.Integer_Text_IO;
   begin
      for Y in reverse L.Field_Value'range loop
         for X in 1 .. L.Number_Of_Cols loop
            Put ("[");
            case L.Field_value(X,Y) is
               when Luft =>
                  Put (" ");
               when Mur =>
                  Put ("M");
               when Kage =>
                  Put ("K");
               when Orm =>
                  Put ("O");
               when others =>
                  Put ("?");
            end case;
            Put ("]");
         end loop;
         New_Line;
      end loop;
   end Put_Level;

   procedure Put_Walls_Around(L : in Level_Access) is
   begin
      -- den vestlige og den østlige mur
      for I in L.Field_Value'Range loop
         L.Field_Value (I,1) := Mur;
         L.Field_Value (I, L.Number_Of_Cols) := Mur;
      end loop;

      -- den nordlige og den sydlig mur (springer den første og sidste over);
      for I in 2 .. L.Number_Of_Cols-1 loop
         L.Field_Value (L.Field_Value'First, I) := Mur;
         L.Field_Value (L.Field_Value'Last,  I) := Mur;
      end loop;

   end Put_Walls_Around;

   function Random_Course return Course is
      package Random_C is new Ada.Numerics.Discrete_Random (Course);
      use Random_C;

      C_Gen : Random_C.Generator;
   begin
      return Random(C_Gen);
   end Random_Course;

   function Random_Worm_Position (L : in Level_Access;
                                  D : in Course) return Position is
      Pos            : Position;
      Okay           : Boolean;
      Temp_X, Temp_Y : Integer;

      subtype X_Coordinates is Integer range 1 .. L.Number_Of_Cols;
      subtype Y_Coordinates is Integer range 1 .. L.Number_Of_Rows;

      package Random_X is new Ada.Numerics.Discrete_Random (X_Coordinates);
      package Random_Y is new Ada.Numerics.Discrete_Random (Y_Coordinates);
      use Random_X, Random_Y;

      X_Gen : Random_X.Generator;
      Y_Gen : Random_Y.Generator;
   begin
      loop
         Okay := True;

         Reset(X_Gen);
         Reset(Y_Gen);

         Pos.X := Random(X_Gen);
         Pos.Y := Random(Y_Gen);

         Temp_X := Pos.X;
         Temp_Y := Pos.Y;

         case D is
            when North =>
              for I in Temp_Y .. (Temp_Y + 3) mod L.Number_Of_Rows loop
                 exit when not Okay;
                 if L.Field_Value (Temp_X, I) = Orm or L.Field_Value (Temp_X, I) = Mur then
                    Okay := False;
                 end if;
              end loop;
            when East  =>
              for I in Temp_X .. (Temp_X + 3) mod L.Number_Of_Cols loop
                 exit when not Okay;
                 if L.Field_Value (I, Temp_Y) = Orm or L.Field_Value (I, Temp_Y) = Mur then
                    Okay := False;
                 end if;
              end loop;
            when South =>
              for I in (Temp_Y - 3) mod L.Number_Of_Rows .. Temp_Y loop
                 exit when not Okay;
                 if L.Field_Value (Temp_X, I) = Orm or L.Field_Value (Temp_X, I) = Mur then
                    Okay := False;
                 end if;
              end loop;
            when West  =>
              for I in (Temp_X - 3) mod L.Number_Of_Cols .. Temp_X loop
                 exit when not Okay;
                 if L.Field_Value (I, Temp_Y) = Orm or L.Field_Value (I, Temp_Y) = Mur then
                    Okay := False;
                 end if;
              end loop;
         end case;

         exit when Okay;
      end loop;

      return Pos;
   end Random_Worm_Position;

   function Random_Cake_Position (L : in Level_Access) return Position is
      subtype X_Coordinates is Integer range 1 .. L.Number_Of_Cols;
      subtype Y_Coordinates is Integer range 1 .. L.Number_Of_Rows;

      package Random_X is new Ada.Numerics.Discrete_Random (X_Coordinates);
      package Random_Y is new Ada.Numerics.Discrete_Random (Y_Coordinates);
      use Random_X, Random_Y;

      X_Gen : Random_X.Generator;
      Y_Gen : Random_Y.Generator;
      Pos : Position;
   begin
      Ada.Text_IO.Put_Line ("Random_Cake_Position: Prøver at finde en tom plads at lægge en kage");
      loop
         Pos.X := Random(X_Gen);
         Pos.Y := Random(Y_Gen);

         exit when L.Field_Value (Pos.X, Pos.Y) = Luft;
         Ada.Text_IO.Put_Line ("Random_Cake_Position: No good, prøver igen");
      end loop;

      return Pos;
   end Random_Cake_Position;

   procedure Set_Field (L : in Level_Access;
                        P : in Position;
                        F : in Field) is
   begin
      L.Field_Value (P.X, P.Y) := F;
   end Set_Field;

end Wormlevel;
