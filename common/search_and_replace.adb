------------------------------------------------------------------------------
--
--  procedure Search_And_Replace (body)
--
------------------------------------------------------------------------------
--  Update information:
--
--  1998.03.17 (Jacob Sparre Andersen)
--    Written.
--
--  2007.11.13 (Jacob Sparre Andersen)
--    Substituted "Replace_By" with "By", to clarify the interface.
--
--  (Add update information above this line.)
------------------------------------------------------------------------------

procedure Search_And_Replace
  (Source  : in out Ada.Strings.Unbounded.Unbounded_String;
   Pattern : in     String;
   By      : in     String) is
   Location : Natural;
begin --  Search_And_Replace
   loop
      Location := Ada.Strings.Unbounded.Index (Source  => Source,
                                               Pattern => Pattern);

      exit when Location = 0;

      Ada.Strings.Unbounded.Replace_Slice
        (Source => Source,
         Low    => Location,
         High   => Location - 1 + Pattern'Length,
         By     => By);
   end loop;
end Search_And_Replace;
