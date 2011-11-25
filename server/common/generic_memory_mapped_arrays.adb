with Ada.IO_Exceptions, POSIX.Files, POSIX.Memory_Mapping, System.Address_To_Access_Conversions;

with EUP.Text_IO;

package body Generic_Memory_Mapped_Arrays is
   function "*" (Left  : in     Natural;
                 Right : in     System.Storage_Elements.Storage_Offset)
                return System.Storage_Elements.Storage_Offset is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return System.Storage_Elements.Storage_Offset (Left) * Right;
   end "*";

   function Storage_Per_Element
     return System.Storage_Elements.Storage_Offset is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Element_Type'Size / System.Storage_Elements.Storage_Element'Size;
   end Storage_Per_Element;

   function Records_In (File : in POSIX.IO.File_Descriptor) return Natural is
   begin
      return
        Natural (POSIX.IO.File_Size (File)) / Natural (Storage_Per_Element);
   end Records_In;

   function Records_Up_To (Index : in Index_Range'Base) return Natural is
   begin
      return Index_Range'Base'Pos (Index) -
             Index_Range'Pos (Index_Range'First) + 1;
   end Records_Up_To;

   function Last_Index (Record_Count : in Natural) return Index_Range'Base is
   begin
      return Index_Range'Base'Val (Index_Range'Pos (Index_Range'First) +
                                   Record_Count - 1);
   end Last_Index;

   procedure Open (File : in out File_Type;
                   Name : in     String) is
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;
   begin
      if File.Open then
         raise Ada.IO_Exceptions.Status_Error;
      else
         if POSIX.Files.Is_File_Present (POSIX.To_POSIX_String (Name)) then
            EUP.Text_IO.Open (File => File.POSIX_File,
                              Name => Name,
                              Mode => POSIX.IO.Read_Write);
         else
            EUP.Text_IO.Create (File => File.POSIX_File,
                                Name => Name,
                                Mode => POSIX.IO.Read_Write);
         end if;

         declare
            Record_Count : Natural;
         begin
            Record_Count := Natural'Min (Records_Up_To (Index_Range'Last),
                                         Records_In (File.POSIX_File));

            File.Allocated_Size := Record_Count * Storage_Per_Element;
            File.Last := Last_Index (Record_Count);
         end;

         if File.Allocated_Size = 0 then
            File.Mapped_Address := System.Null_Address;
            File.Open := True;
         else
            declare
               use POSIX.Memory_Mapping;
            begin
               File.Mapped_Address :=
                 Map_Memory (Length     => File.Allocated_Size,
                             Protection => Allow_Read + Allow_Write,
                             Mapping    => Map_Shared,
                             File       => File.POSIX_File,
                             Offset     => 0);
            end;

            if File.Mapped_Address = System.Null_Address then
               POSIX.IO.Close (File.POSIX_File);
               File.Allocated_Size := 0;
               File.Mapped_Address := System.Null_Address;
               File.Last := Index_Range'Base'First;
               raise Ada.IO_Exceptions.Use_Error;
            else
               File.Open := True;
            end if;
         end if;
      end if;
   end Open;

   procedure Close  (File : in out File_Type) is
      use type System.Address;
   begin
      if File.Open then
         if File.Mapped_Address /= System.Null_Address then
            POSIX.Memory_Mapping.Unmap_Memory (First  => File.Mapped_Address,
                                               Length => File.Allocated_Size);
         end if;
         POSIX.IO.Close (File => File.POSIX_File);
         File.Open := False;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end Close;

   function Is_Open (File : in     File_Type) return Boolean is
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;
   begin
      return File.Open;
   end Is_Open;

   function First (File : in     File_Type) return Index_Range is
   begin
      if File.Open then
         return Index_Range'First;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end First;

   function Last (File : in     File_Type) return Index_Range'Base is
   begin
      if File.Open then
         return File.Last;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end Last;

   function Element (Source : in     File_Type;
                     Index  : in     Index_Range) return Element_Type is
      use type System.Address;
   begin
      if Source.Open then
         if Source.Mapped_Address = System.Null_Address then
            raise Constraint_Error;
         else
            declare
               subtype Constrained_Element_Array is Element_Array
                 (Index_Range'First .. Source.Last);
               package Conversions is
                  new System.Address_To_Access_Conversions
                 (Object => Constrained_Element_Array);
               Data : constant Conversions.Object_Pointer :=
                 Conversions.To_Pointer (Source.Mapped_Address);
            begin
               return Data (Index);
            end;
         end if;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end Element;

   function Slice (Source : in     File_Type;
                   Low    : in     Index_Range;
                   High   : in     Index_Range'Base) return Element_Array is
   begin
      if Source.Open then
         declare
            subtype Constrained_Element_Array is Element_Array (Index_Range'First .. Source.Last);
            package Conversions is new System.Address_To_Access_Conversions (Object => Constrained_Element_Array);
            Data : constant Conversions.Object_Pointer := Conversions.To_Pointer (Source.Mapped_Address);
         begin
            return Data (Low .. High);
         end;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end Slice;

   procedure Replace_Element (Source : in out File_Type;
                              Index  : in     Index_Range;
                              By     : in     Element_Type) is
   begin
      if Source.Open then
         declare
            subtype Constrained_Element_Array is Element_Array (Index_Range'First .. Source.Last);
            package Conversions is new System.Address_To_Access_Conversions (Object => Constrained_Element_Array);
            Data : constant Conversions.Object_Pointer := Conversions.To_Pointer (Source.Mapped_Address);
         begin
            Data (Index) := By;
         end;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end Replace_Element;

   procedure Replace_Slice (Source : in out File_Type;
                            Low    : in     Index_Range;
                            High   : in     Index_Range'Base;
                            By     : in     Element_Array) is
   begin
      if Source.Open then
         declare
            subtype Constrained_Element_Array is Element_Array (Index_Range'First .. Source.Last);
            package Conversions is new System.Address_To_Access_Conversions (Object => Constrained_Element_Array);
            Data : constant Conversions.Object_Pointer := Conversions.To_Pointer (Source.Mapped_Address);
         begin
            Data (Low .. High) := By;
         end;
      else
         raise Ada.IO_Exceptions.Status_Error;
      end if;
   end Replace_Slice;

   procedure Resize (File     : in out File_Type;
                     New_Last : in     Index_Range) is
      use type System.Address;
      use type System.Storage_Elements.Storage_Offset;
      procedure Write is new POSIX.IO.Generic_Write (Element_Type);
      New_Position : POSIX.IO.IO_Offset;
   begin
      if not File.Open then
         raise Ada.IO_Exceptions.Status_Error;
      end if;

      if File.Mapped_Address /= System.Null_Address then
         POSIX.Memory_Mapping.Unmap_Memory (First  => File.Mapped_Address,
                                            Length => File.Allocated_Size);
      end if;

      POSIX.IO.Seek (File   => File.POSIX_File,
                     Offset => POSIX.IO.IO_Offset (Records_Up_To (File.Last) *
                                                   Storage_Per_Element),
                     Result => New_Position);

      while Records_In (File.POSIX_File) < Records_Up_To (New_Last) loop
         Write (File => File.POSIX_File,
                Item => Default_Value);
      end loop;

      File.Allocated_Size := Records_Up_To (New_Last) * Storage_Per_Element;
      File.Last := New_Last;

      if File.Allocated_Size = 0 then
         File.Mapped_Address := System.Null_Address;
      else
         declare
            use POSIX.Memory_Mapping;
         begin
            File.Mapped_Address :=
              Map_Memory (Length     => File.Allocated_Size,
                          Protection => Allow_Read + Allow_Write,
                          Mapping    => Map_Shared,
                          File       => File.POSIX_File,
                          Offset     => 0);
         end;
      end if;

      if File.Mapped_Address = System.Null_Address then
         POSIX.IO.Close (File.POSIX_File);
         File.Allocated_Size := 0;
         File.Mapped_Address := System.Null_Address;
         File.Last := Index_Range'Base'First;
         File.Open := False;
         raise Ada.IO_Exceptions.Use_Error;
      end if;
   end Resize;
end Generic_Memory_Mapped_Arrays;
