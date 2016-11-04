------------------------------------------------------------------------------
--
--  function Zero_Filled_Image (spec)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.04.05 (Jacob Sparre Andersen)
--    Written.
--
--  2004.03.01 (Jacob Sparre Andersen)
--    Extended to support other bases than just base 10.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------
--  Standard packages:

with Ada.Text_IO;

------------------------------------------------------------------------------

generic

   type Num is range <>;

   Width : Ada.Text_IO.Field;
   Base  : Ada.Text_IO.Number_Base := 10;

function Zero_Filled_Image (Item : in Num) return String;
