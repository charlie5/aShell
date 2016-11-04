------------------------------------------------------------------------------
--
--  procedure Search_And_Replace (spec)
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

------------------------------------------------------------------------------
--  Standardbiblioteker:

with Ada.Strings.Unbounded;

------------------------------------------------------------------------------

procedure Search_And_Replace
  (Source  : in out Ada.Strings.Unbounded.Unbounded_String;
   Pattern : in     String;
   By      : in     String);
