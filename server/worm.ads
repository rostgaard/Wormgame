with Ada.Containers.Doubly_Linked_Lists;
with Wormlevel, Protocol; --, Handlers;

package Worm is
   use Wormlevel, Protocol; --, Handlers;

   package Worm_Position_Container is new Ada.Containers.Doubly_Linked_Lists(Position);
   use Worm_Position_Container;

   type Worm_State is (Alive,Dead,Observing);

   type Worm_Type is
      record
         Worm_Body : List;
         Direction : Course  := North;
         Points    : Natural := 0;
         Number    : Positive;
         State     : Worm_State := Alive;
         Full      : Boolean := False;
      end record;
   type Worm_Type_Access is access Worm_Type;
   procedure Update_Worm (Worm : in Worm_Type_Access;
                          Lvl  : in Level_Access);
   procedure Turn_Left   (Worm : in Worm_Type_Access);
   procedure Turn_Right  (Worm : in Worm_Type_Access);
   procedure Kill        (Worm : in Worm_Type_Access);
end Worm;
