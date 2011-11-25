with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Numerics.Discrete_Random;
Package Wormlevel is

   type Field  is (Luft,Mur,Kage,Orm);
   type Fields is array (Positive range <>, Positive range <>) of Field ;
   type Course is (North, East, South, West);
   type Level(Width: Positive; Height: Positive) is
      record
         Number_Of_Cols : Positive := Width;
         Number_Of_Rows : Positive := Height;
         Field_value    : Fields (1 .. Width, 1 .. Height);
      end record;
   type Level_Access is access Level;
   type Position is
      record
         X : Positive;
         Y : Positive;
      end record;
   type Positions is array (Positive range <>) of Position;

   procedure Empty (L : in Level_access);
   procedure Put_Level (L : in Level_access);
   procedure Put_Walls_Around (L : in Level_Access);

   function Random_Course return Course;
   function Random_Worm_Position (L : in Level_Access;
                                  D : in Course) return Position;
   function Random_Cake_Position (L : in Level_Access) return Position;
   procedure Set_Field(L : in Level_Access;
                       P : in Position;
                       F : in Field);
end Wormlevel;
