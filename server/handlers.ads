with Protocol;
with Ada.Exceptions;
with GNAT.Sockets;
with Stream_Shortcuts;
with Protocol,User,Wormlevel,Worm;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics.Discrete_Random;


package Handlers is
   use GNAT.Sockets;
   use Protocol,Wormlevel,Worm;

   type Input_Handler;
   type Input_Handler_Access is access Input_Handler;


   type Player_Type is
      record
         IO_Handler : Input_Handler_Access;
         Worm       : Worm_Type_Access;
      end record;

   type Players is array(1 .. 10) of access Player_Type;


   task type Socket_Listener is
      entry Attach_Socket (S : in Socket_Type);
   end Socket_Listener;
   type Socket_Listener_Access is access Socket_Listener;

   type Client_States is (Not_Logged_In, Logged_In, In_Queue, In_Game, Quitting);

   type Command_Handler is Synchronized Interface;
   procedure Attach_Parent (Obj : in out Command_Handler;
                            P : in Input_Handler_Access) is null;
   procedure Handle (Obj : in out Command_Handler;
                     R   : in     Client_Request) is null;

   task type Login_Task is new Command_Handler with
     entry Attach_Parent (P : in Input_Handler_Access);
     entry Handle (R : in Client_Request);
   end Login_Task;


   task type Logged_In_Task is new Command_Handler with
     entry Attach_Parent (P : in Input_Handler_Access);
     entry Handle (R : in Client_Request);
   end Logged_In_Task;


   task type Queue_Task is new Command_Handler with
     entry Attach_Parent (P : in Input_Handler_Access);
     entry Handle (R : in Client_Request);
   end Queue_Task;


   task type Game_Queue is
      entry Attach_Parent (P : in Input_Handler_Access);
   end Game_Queue;
   type Queue_Access is access Game_Queue;



   task type Game_Task is new Command_Handler with
     entry Clients (P : in Players; Idx : in Natural);
--     entry Handle (R : in Client_Request);
   end Game_Task;

   type Game_Task_Access is access Game_Task;



--     task type Direction_Handler is
--        entry Clients (P : Players);
--        entry Turn_Left (Worm_Number : in Positive);
--        entry Turn_Right (Worm_Number : in Positive);
--        entry Kill;
--     end Direction_Handler;

--     type Direction_Handler_Access is access Direction_Handler;


   type Command_Handler_Access is access all Command_Handler'Class;
   task type Input_Handler is
      entry Self_Reference (R : in Input_Handler_Access);
      entry Attach_Socket (S : Socket_Type);
      entry Change_State (State : in Client_States);
      entry Receive (R : in Client_Request);
      entry Send(Msg : in String);
      entry Log_Out;
      entry Attach_Worm (W : in Worm_Type_Access);
   end Input_Handler;



end Handlers;
