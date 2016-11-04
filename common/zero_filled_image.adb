------------------------------------------------------------------------------
--
--  function Zero_Filled_Image (body)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.04.05 (Jacob Sparre Andersen)
--    Written.
--
--  1996.12.02 (Jacob Sparre Andersen)
--    Correcting some formal bugs.
--
--  2004.03.01 (Jacob Sparre Andersen)
--    Extended to support other bases than just base 10.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

function Zero_Filled_Image (Item : in Num) return String is

   package Num_IO is new Ada.Text_IO.Integer_IO (Num => Num);

begin -- Zero_Filled_Image
   if Base = 10 then
  Base_Ten:
      declare
         Result : String(1 .. Width);
      begin
         Num_IO.Put(To   => Result,
                    Item => Item,
                    Base => Base);

         for Index in Result'Range loop
            if Result(Index) = ' ' then
               Result(Index) := '0';
            end if;
         end loop;

         return Result;
      end Base_Ten;
   else
  Other_Base:
      declare
         Result : String(1 .. Width + 4);
         Index  : Natural := Result'First + 1;
      begin
         Num_IO.Put (To   => Result,
                     Item => Item,
                     Base => Base);

         loop
            if Result (Index) = '#' then
               Result (Index) := '0';
               exit;
            else
               Result (Index) := '0';
               Index := Index + 1;
            end if;
         end loop;

         return Result (4 .. Width + 3);
      end Other_Base;
   end if;
end Zero_Filled_Image;
