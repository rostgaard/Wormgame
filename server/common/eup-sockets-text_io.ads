with
  Ada.Characters.Latin_1,
  Ada.Strings.Unbounded,
  POSIX.IO,
  POSIX.Permissions;

package EUP.Sockets.Text_IO is
   procedure New_Line (Socket  : in     Socket_Type;
                       Spacing : in     Positive := 1);

   procedure Skip_Line (Socket  : in     Socket_Type;
                        Spacing : in     Positive := 1);

   procedure Get (Socket : in     Socket_Type;
                  Item   :    out Character);
   procedure Get (Socket : in     Socket_Type;
                  Item   :    out POSIX.POSIX_Character);

   procedure Put (Socket : in     Socket_Type;
                  Item   : in     Character);
   procedure Put (Socket : in     Socket_Type;
                  Item   : in     POSIX.POSIX_Character);

   procedure Get (Socket : in     Socket_Type;
                  Item   :    out Ada_String);
   procedure Get (Socket : in     Socket_Type;
                  Item :    out POSIX_String);

   procedure Put (Socket : in     Socket_Type;
                  Item : in     Ada_String);
   procedure Put (Socket : in     Socket_Type;
                  Item : in     POSIX_String);
   procedure Put (Socket : in     Socket_Type;
                  Item : in     Unbounded_String);

   procedure Get_Line (Socket : in     Socket_Type;
                       Item :    out Ada_String;
                       Last :    out Natural);
   procedure Get_Line (Socket : in     Socket_Type;
                       Item :    out POSIX_String;
                       Last :    out Natural);
   procedure Get_Line (Socket : in     Socket_Type;
                       Item :    out Unbounded_String);

   procedure Put_Line (Socket : in     Socket_Type;
                       Item : in     Ada_String);
   procedure Put_Line (Socket : in     Socket_Type;
                       Item : in     POSIX_String);
   procedure Put_Line (Socket : in     Socket_Type;
                       Item : in     Unbounded_String);
end EUP.Text_IO;
