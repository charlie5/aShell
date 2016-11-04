generic
   type Num is range <>;
package EUP.Text_IO.Integer_IO is
   Default_Width : Field := Num'Width;
   Default_Base  : Number_Base := 10;

   procedure Get (File  : in     File_Type;
                  Item  :    out Num;
                  Width : in     Field := 0);

   procedure Put (File  : in     File_Type;
                  Item  : in     Num;
                  Width : in     Field := Default_Width;
                  Base  : in     Number_Base := Default_Base);

   procedure Get (From : in     String;
                  Item :    out Num;
                  Last :    out Positive);
   procedure Get (From : in     POSIX_String;
                  Item :    out Num;
                  Last :    out Positive);
   procedure Get (From : in     Unbounded_String;
                  Item :    out Num;
                  Last :    out Positive);

   procedure Put (To   :    out String;
                  Item : in     Num;
                  Base : in     Number_Base := Default_Base);
end EUP.Text_IO.Integer_IO;
