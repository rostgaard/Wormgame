package body User is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   procedure Load_Usermap_File is
   begin

      Usermap.Open(MMap_File_Handle, MMap_File_name);

      if Usermap.Last(MMap_File_Handle)  = 1 then
         Usermap.Resize(MMap_File_Handle,10);
      end if;
   end Load_Usermap_File;


   function Seperator(Line : in String) return Natural is
      Position : Natural := Line'First;
   begin
      -- find positionen af seperatoren
      for Seperator in Line'Range loop
         exit when Line (Position) = ',';
         Position := Position + 1;
      end loop;
      return Position;
   end Seperator;

   -- function Find_User(Username: in String) return Usermap.Index_Range is

   function Find_User(Username: in String) return Integer is
      use Bounded_String;
      Position  : Integer := 0;
      User      : A_User;
   begin

      Put_Line("Find_User kaldt");

      for U in Usermap.First(MMap_File_Handle) .. Usermap.Last(MMap_File_Handle) loop
         User := Usermap.Element(MMap_File_Handle,U);

         if User.Username = Bounded_String.To_Bounded_String(Username) then
            return Position;
         end if;
         Position := Position + 1;
      end loop;
      return -1;

   exception
      when others =>
         raise Program_Error;

   end Find_User;

   function Authenticate(Username: in String; Password: in String) return Boolean is
      Found_User_Index : Integer := -1;
      Found_User : A_User;

   begin
      Put_Line("Auth kaldt");

--        Found_User_Index := Find_User(Username);

--        if (Found_User_Index = -1) then
--           Put("no user");
--           return False;
--        else
--           Found_User := Usermap.Element(MMap_File_Handle,Found_User_Index);

--           if Password /= "" and  Password = Bounded_String.To_String(Found_User.Password) then
--              return True;
--           else
--              return False;
--           end if;
--        end if;
      return True;
   end Authenticate;

   function Find_Open_Slot return Natural is
      Lookup_Username : String := "";
      Index_Snot : Natural := 0;
   begin

      Put_Line("open slut kaldt");

      for U in Usermap.First(MMap_File_Handle) .. Usermap.Last(MMap_File_Handle) loop
         Lookup_Username := Bounded_String.To_String(Usermap.Element(MMap_File_Handle,U).Username);

         if Lookup_Username = "" then
            return Index_Snot;
         end if;
      end loop;

      Usermap.Resize(MMap_File_Handle, Usermap.Last(MMap_File_Handle)*2);
      return Index_Snot+1;


   end Find_Open_Slot;

   function Create(Username: in String; Password: in String) return Boolean is
      New_User : A_User;
   begin

      Put_Line("Create kaldt");
--        New_User.Username := Bounded_String.To_Bounded_String(Username);
--        New_User.Password := Bounded_String.To_Bounded_String(Password);



--        if (Find_User(Username) = -1) then
--           Usermap.Replace_Element( MMap_File_Handle,
--                                    Find_Open_Slot,
--                                    New_User);
--           return True;
--        else
--           return False;
--        end if;
      return True;
   end Create;


end User;
