with Handlers;

package Game is
   protected type State is new Handlers.Command_Handler with
      entry Turn_Left (Worm_Number : in Natural);
      entry Turn_Right (Worm_Number : in Natural);
      entry Update;
   end State;
end Game;
