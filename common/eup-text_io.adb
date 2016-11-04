with Ada.IO_Exceptions, Interfaces.C.Strings, POSIX.C;

with EUP.Strings;

pragma Elaborate (POSIX.C);
--pragma Elaborate (POSIX.IO);
--pragma Elaborate (POSIX.Permissions);

package body EUP.Text_IO is
   function Default return Permission_Set is
   begin
      return (POSIX.Permissions.Owner_Read  => True,
              POSIX.Permissions.Owner_Write => True,
              POSIX.Permissions.Group_Read  => True,
              POSIX.Permissions.Others_Read => True,
              others                        => False);
   end Default;

   procedure Create (File        :    out File_Type;
                     Mode        : in     File_Mode := POSIX.IO.Write_Only;
                     Name        : in     Ada_String;
                     Permissions : in     Permission_Set := Default) is
      pragma Inline (Create);
   begin
      Create (File        => File,
              Mode        => Mode,
              Name        => POSIX.To_POSIX_String (Name),
              Permissions => Permissions);
   end Create;

   procedure Create (File        :    out File_Type;
                     Mode        : in     File_Mode := POSIX.IO.Write_Only;
                     Name        : in     POSIX_String;
                     Permissions : in     Permission_Set := Default) is
      use type Interfaces.C.int;
      use type POSIX.C.mode_t;

      function C_Open
        (Name  : in Interfaces.C.Strings.chars_ptr;
         Flags : in Interfaces.C.int;
         Mode  : in POSIX.C.mode_t) return POSIX.IO.File_Descriptor;
      pragma Import (C, C_Open, "open");

      C_Flags : constant array (File_Mode) of Interfaces.C.int :=
        (POSIX.IO.Read_Only  => POSIX.C.O_RDONLY,
         POSIX.IO.Write_Only => POSIX.C.O_WRONLY,
         POSIX.IO.Read_Write => POSIX.C.O_RDWR);

      C_Permissions : constant array (POSIX.Permissions.Permission)
        of POSIX.C.mode_t :=
        (POSIX.Permissions.Others_Execute => 2#000_000_000_001#,
         POSIX.Permissions.Others_Write   => 2#000_000_000_010#,
         POSIX.Permissions.Others_Read    => 2#000_000_000_100#,
         POSIX.Permissions.Group_Execute  => 2#000_000_001_000#,
         POSIX.Permissions.Group_Write    => 2#000_000_010_000#,
         POSIX.Permissions.Group_Read     => 2#000_000_100_000#,
         POSIX.Permissions.Owner_Execute  => 2#000_001_000_000#,
         POSIX.Permissions.Owner_Write    => 2#000_010_000_000#,
         POSIX.Permissions.Owner_Read     => 2#000_100_000_000#,
--       POSIX.Permissions.Sticky         => 2#001_000_000_000#,
         POSIX.Permissions.Set_Group_ID   => 2#010_000_000_000#,
         POSIX.Permissions.Set_User_ID    => 2#100_000_000_000#);

      C_Name       : Interfaces.C.Strings.chars_ptr;
      C_Permission : POSIX.C.mode_t := 0;
   begin
      C_Name := Interfaces.C.Strings.New_String (Strings.To_Ada_String (Name));

      for Index in Permissions'Range loop
         if Permissions (Index) then
            C_Permission := C_Permission or C_Permissions (Index);
         end if;
      end loop;

      File := C_Open (Name  => C_Name,
                      Flags =>
                        C_Flags (Mode) + POSIX.C.O_CREAT + POSIX.C.O_TRUNC,
                      Mode  => C_Permission);

      Interfaces.C.Strings.Free (C_Name);
   end Create;

   procedure Create (File        :    out File_Type;
                     Mode        : in     File_Mode := POSIX.IO.Write_Only;
                     Name        : in     Unbounded_String;
                     Permissions : in     Permission_Set := Default) is
      pragma Inline (Create);
   begin
      Create (File        => File,
              Mode        => Mode,
              Name        => Ada.Strings.Unbounded.To_String (Name),
              Permissions => Permissions);
   end Create;

   procedure Open (File :    out File_Type;
                   Mode : in     File_Mode;
                   Name : in     Ada_String) is
      pragma Inline (Open);
   begin
      Open (File => File,
            Mode => Mode,
            Name => POSIX.To_POSIX_String (Name));
   end Open;

   procedure Open (File :    out File_Type;
                   Mode : in     File_Mode;
                   Name : in     POSIX_String) is
      pragma Inline (Open);
   begin
      File := POSIX.IO.Open (Mode => Mode,
                             Name => Name);
   end Open;

   procedure Open (File :    out File_Type;
                   Mode : in     File_Mode;
                   Name : in     Unbounded_String) is
      pragma Inline (Open);
   begin
      Open (File => File,
            Mode => Mode,
            Name => Ada.Strings.Unbounded.To_String (Name));
   end Open;

   procedure Close (File : in     File_Type) is
   begin
      POSIX.IO.Close (File => File);
   end Close;

   procedure New_Line (File    : in     File_Type;
                       Spacing : in     Positive := 1) is
      Buffer : POSIX.POSIX_String (1 .. Spacing) := (others => POSIX.LF);
   begin
      Put (File => File,
           Item => Buffer);
   end New_Line;

   procedure Skip_Line (File    : in     File_Type;
                        Spacing : in     Positive := 1) is
      use type POSIX.POSIX_Character;
      Skipped : Natural := 0;
      Char    : POSIX.POSIX_Character;
   begin
      loop
         Get (File => File,
              Item => Char);
         if Char = POSIX.LF then
            Skipped := Skipped + 1;
            exit when Skipped = Spacing;
         end if;
      end loop;
   end Skip_Line;

   procedure Get (File : in     File_Type;
                  Item :    out Character) is
      pragma Inline (Get);
      Buffer : POSIX.POSIX_Character;
      for Buffer'Address use Item'Address;
   begin
      Get (File => File,
           Item => Buffer);
   end Get;

   procedure Get (File : in     File_Type;
                  Item :    out POSIX.POSIX_Character) is
      pragma Inline (Get);
      Buffer : POSIX_String (1 .. 1);
      Last   : Natural;
   begin
      POSIX.IO.NONSTANDARD_Read (File   => File,
                                 Buffer => Buffer,
                                 Last   => Last);
      if Last < Buffer'First then
         raise Ada.IO_Exceptions.End_Error;
      else
         Item := Buffer (Buffer'First);
      end if;
   end Get;

   procedure Put (File : in     File_Type;
                  Item : in     Character) is
      pragma Inline (Put);
   begin
      Put (File => File,
           Item => POSIX.POSIX_Character (Item));
   end Put;

   procedure Put (File : in     File_Type;
                  Item : in     POSIX.POSIX_Character) is
      pragma Inline (Put);
      procedure Write_POSIX_Character is
         new POSIX.IO.Generic_Write (T => POSIX.POSIX_Character);
   begin
      Write_POSIX_Character (File => File,
                             Item => Item);
   end Put;

   procedure Get (File : in     File_Type;
                  Item :    out Ada_String) is
      pragma Inline (Get);
      Buffer : POSIX_String (Item'Range);
      for Buffer'Address use Item'Address;
   begin
      Get (File => File,
           Item => Buffer);
   end Get;

   procedure Get (File : in     File_Type;
                  Item :    out POSIX.POSIX_String) is
      Last : Natural := Item'First - 1;
      Read : POSIX.IO_Count;
   begin
      while Last < Item'Last loop
         POSIX.IO.Read (File   => File,
                        Buffer => Item (Last + 1 .. Item'Last),
                        Last   => Read);
         Last := Natural (Read);
      end loop;
   end Get;

   procedure Put (File : in     File_Type;
                  Item : in     Ada_String) is
      pragma Inline (Put);
   begin
      Put (File => File,
           Item => POSIX.To_POSIX_String (Item));
   end Put;

   procedure Put (File : in     File_Type;
                  Item : in     POSIX_String) is
      Last    : Natural := Item'First - 1;
      Written : POSIX.IO_Count;
   begin
      while Last < Item'Last loop
         POSIX.IO.Write (File   => File,
                         Buffer => Item (Last + 1 .. Item'Last),
                         Last   => Written);
         Last := Natural (Written);
      end loop;
   end Put;

   procedure Put (File : in     File_Type;
                  Item : in     Unbounded_String) is
      pragma Inline (Put);
   begin
      Put (File => File,
           Item => Ada.Strings.Unbounded.To_String (Item));
   end Put;

   procedure Get_Line (File : in     File_Type;
                       Item :    out Ada_String;
                       Last :    out Natural) is
   begin
      Last := Item'First;
      while Last <= Item'Last loop
         Get (File => File,
              Item => Item (Last));

         if Item (Last) = Ada.Characters.Latin_1.CR then
            null;
         elsif Item (Last) = Ada.Characters.Latin_1.LF then
            Last := Last - 1;
            return;
         else
            Last := Last + 1;
         end if;
      end loop;
      Last := Item'Last;
   end Get_Line;

   procedure Get_Line (File : in     File_Type;
                       Item :    out POSIX_String;
                       Last :    out Natural) is
      use type POSIX.POSIX_Character;
   begin
      Last := Item'First;
      while Last <= Item'Last loop
         Get (File => File,
              Item => Item (Last));

         if Item (Last) = POSIX.CR then
            null;
         elsif Item (Last) = POSIX.LF then
            Last := Last - 1;
            return;
         else
            Last := Last + 1;
         end if;
      end loop;
      Last := Item'Last;
   end Get_Line;

   procedure Get_Line (File : in     File_Type;
                       Item :    out Unbounded_String) is
      Char : Character;
   begin
      Item := Ada.Strings.Unbounded.Null_Unbounded_String;
      loop
         Get (File => File,
              Item => Char);

         if Char = Ada.Characters.Latin_1.CR then
            null;
         elsif Char = Ada.Characters.Latin_1.LF then
            return;
         else
            Ada.Strings.Unbounded.Append (Source   => Item,
                                          New_Item => Char);
         end if;
      end loop;
   end Get_Line;

   procedure Put_Line (File : in     File_Type;
                       Item : in     Ada_String) is
      pragma Inline (Put_Line);
   begin
      Put (File => File,
           Item => Item & Ada.Characters.Latin_1.LF);
   end Put_Line;

   procedure Put_Line (File : in     File_Type;
                       Item : in     POSIX_String) is
      pragma Inline (Put_Line);
      use type POSIX_String;
   begin
      Put (File => File,
           Item => Item & POSIX.LF);
   end Put_Line;

   procedure Put_Line (File : in     File_Type;
                       Item : in     Unbounded_String) is
      pragma Inline (Put_Line);
   begin
      Put_Line (File => File,
                Item => Ada.Strings.Unbounded.To_String (Item));
   end Put_Line;
end EUP.Text_IO;
