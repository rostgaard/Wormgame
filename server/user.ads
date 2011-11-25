with Ada.Text_IO,Ada.Strings.Unbounded;
with Protocol;
with Generic_Memory_Mapped_Arrays,
  Ada.Strings.Bounded;


package User is
   package Bounded_String is new Ada.Strings.Bounded.Generic_Bounded_Length ( Max => 30 );


   type A_User is record
     Username : Bounded_String.Bounded_String := Bounded_String.To_Bounded_String("");
     Password : Bounded_String.Bounded_String := Bounded_String.To_Bounded_String("");
   end record;

   type User_List Is array (Positive range <>) of A_User;

   package Usermap is new Generic_Memory_Mapped_Arrays (Element_Type  => A_User,
                                                        Index_Range   => Positive,
                                                        Element_Array => User_List,
                                                        Default_Value => (others => <>));

   MMap_File_Name   : String := "userdb.bin";
   MMap_File_Handle : Usermap.File_Type;


   procedure Load_Usermap_File;

   function Authenticate(Username: in String; Password: in String) return Boolean;
   function Create(Username: in String; Password: in String) return Boolean;
end User;
