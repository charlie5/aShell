# aShell

A component to aid in writing shell-like applications in Ada.

Under an ISC license.

## Install
Add the following lines to your ~/.bashrc or equivalent ...

ASHELL=/path/to/ashell/root

export GPR_PROJECT_PATH=$ASHELL/library:$GPR_PROJECT_PATH


Alire support is pending.


## Example

    declare
       use Shell.Commands;
       Output : constant String := +Output_Of ("ps -A | grep bash | wc");
    begin
       Put_Line (Output);
    end;

This will produce output similar to ...

     14      56     434
     
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
