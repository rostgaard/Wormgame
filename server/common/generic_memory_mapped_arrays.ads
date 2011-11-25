with POSIX.IO, System.Storage_Elements;

generic
   type Element_Type is private;
   type Index_Range is (<>);
   type Element_Array is array (Index_Range range <>) of Element_Type;
   Default_Value : Element_Type;
package Generic_Memory_Mapped_Arrays is
   type File_Type is limited private;

   procedure Open   (File : in out File_Type;
                     Name : in     String);
   procedure Close  (File : in out File_Type);
   function Is_Open (File : in     File_Type) return Boolean;

   function First (File : in     File_Type) return Index_Range;
   function Last (File : in     File_Type) return Index_Range'Base;

   function Element (Source : in     File_Type;
                     Index  : in     Index_Range) return Element_Type;
   function Slice (Source : in     File_Type;
                   Low    : in     Index_Range;
                   High   : in     Index_Range'Base) return Element_Array;

   procedure Replace_Element (Source : in out File_Type;
                              Index  : in     Index_Range;
                              By     : in     Element_Type);
   procedure Replace_Slice (Source : in out File_Type;
                            Low    : in     Index_Range;
                            High   : in     Index_Range'Base;
                            By     : in     Element_Array);

   procedure Resize (File     : in out File_Type;
                     New_Last : in     Index_Range);
private
   type File_Type is limited
      record
         Open           : Boolean := False;
         POSIX_File     : POSIX.IO.File_Descriptor;
         Allocated_Size : System.Storage_Elements.Storage_Offset := 0;
         Mapped_Address : System.Address := System.Null_Address;
         Last           : Index_Range'Base := Index_Range'Base'First;
      end record;
end Generic_Memory_Mapped_Arrays;
