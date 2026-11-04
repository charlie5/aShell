procedure Contract_Tests;
--
-- What 'Safe' and 'Unsafe' commands have in common. The same source is built against each, as they can't share a program.
-- Each folder gives 'Under_Test', which renames the package, and 'Flavour', for the few documented differences.
-- Any failure raises an exception, or hangs.
