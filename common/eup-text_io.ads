with
  Ada.Characters.Latin_1,
  Ada.Strings.Unbounded,
  POSIX.IO,
  POSIX.Permissions;

package EUP.Text_IO is
   subtype File_Type is POSIX.IO.File_Descriptor;
   subtype File_Mode is POSIX.IO.File_Mode;
   subtype Permission_Set is POSIX.Permissions.Permission_Set;

   subtype Field       is Natural;
   subtype Number_Base is Integer range 2 .. 16;

   function Default return Permission_Set;

   procedure Create (File        :    out File_Type;
                     Mode        : in     File_Mode := POSIX.IO.Write_Only;
                     Name        : in     Ada_String;
                     Permissions : in     Permission_Set := Default);
   procedure Create (File        :    out File_Type;
                     Mode        : in     File_Mode := POSIX.IO.Write_Only;
                     Name        : in     POSIX_String;
                     Permissions : in     Permission_Set := Default);
   procedure Create (File        :    out File_Type;
                     Mode        : in     File_Mode := POSIX.IO.Write_Only;
                     Name        : in     Unbounded_String;
                     Permissions : in     Permission_Set := Default);

   procedure Open (File :    out File_Type;
                   Mode : in     File_Mode;
                   Name : in     Ada_String);
   procedure Open (File :    out File_Type;
                   Mode : in     File_Mode;
                   Name : in     POSIX_String);
   procedure Open (File :    out File_Type;
                   Mode : in     File_Mode;
                   Name : in     Unbounded_String);

   procedure Close (File : in     File_Type);

   Standard_Input  : constant File_Type := POSIX.IO.Standard_Input;
   Standard_Output : constant File_Type := POSIX.IO.Standard_Output;
   Standard_Error  : constant File_Type := POSIX.IO.Standard_Error;

   procedure New_Line (File    : in     File_Type;
                       Spacing : in     Positive := 1);

   procedure Skip_Line (File    : in     File_Type;
                        Spacing : in     Positive := 1);

   procedure Get (File : in     File_Type;
                  Item :    out Character);
   procedure Get (File : in     File_Type;
                  Item :    out POSIX.POSIX_Character);

   procedure Put (File : in     File_Type;
                  Item : in     Character);
   procedure Put (File : in     File_Type;
                  Item : in     POSIX.POSIX_Character);

   procedure Get (File : in     File_Type;
                  Item :    out Ada_String);
   procedure Get (File : in     File_Type;
                  Item :    out POSIX_String);

   procedure Put (File : in     File_Type;
                  Item : in     Ada_String);
   procedure Put (File : in     File_Type;
                  Item : in     POSIX_String);
   procedure Put (File : in     File_Type;
                  Item : in     Unbounded_String);

   procedure Get_Line (File : in     File_Type;
                       Item :    out Ada_String;
                       Last :    out Natural);
   procedure Get_Line (File : in     File_Type;
                       Item :    out POSIX_String;
                       Last :    out Natural);
   procedure Get_Line (File : in     File_Type;
                       Item :    out Unbounded_String);

   procedure Put_Line (File : in     File_Type;
                       Item : in     Ada_String);
   procedure Put_Line (File : in     File_Type;
                       Item : in     POSIX_String);
   procedure Put_Line (File : in     File_Type;
                       Item : in     Unbounded_String);
end EUP.Text_IO;
