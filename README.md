# aShell

A component to aid in writing shell-like applications in Ada.

Under an ISC license.

## Example

    declare
       Output : constant String := Shell.Output_Of ("ps -A | grep bash | wc");
    begin
       Put_Line (Output);
    end;

## Example

A trivial shell script:

    #! /bin/sh
    tr '\000' '\n' | ds_harmonize

Might be written like this in Ada:

    with Shell;
    procedure SDS_to_DS is
       DS_Pipe         : Shell.Pipe;
       Zero_To_Newline : Shell.Process := Start (Program   => "tr",
                                                 Arguments => (+"\000", +"\n"),
                                                 Output    => DS_Pipe);
       DS_Harmonize    : Shell.Process := Start (Program   => "ds_harmonize",
                                                 Input     => DS_Pipe);
    begin
       null;
    end SDS_to_DS;

This does not give us any gain, but once you can mix and match Ada entities with
external filter commands, it may be easier to save time, as you don't have to
reimplement the algorithm of any external filter in Ada, to finish your program.
