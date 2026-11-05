# aShell
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/ashell.json)](https://alire.ada.dev/crates/ashell.html)

A component to aid in writing shell-like applications in Ada.

Under an ISC license.

## Install
Add the following lines to your ~/.bashrc or equivalent ...

```shell
ASHELL=/path/to/ashell/root

export GPR_PROJECT_PATH=$ASHELL/library:$GPR_PROJECT_PATH
```

Alire is supported.

`Shell.Commands.Safe` also needs `ashell_spawn_server` on your `PATH`. Run `applet/spawn_server/install_spawn_server.sh` to build it and copy it to `/usr/bin`.


## Example

```ada
    declare
       use Shell, Shell.Commands, Shell.Commands.Unsafe;
       Output : constant String := +Output_Of (Run ("ps -A | grep bash | wc"));
    begin
       Put_Line (Output);
    end;
```

This will produce output similar to ...

     14      56     434
     
## Example

A trivial shell script:

```shell
    #! /bin/sh
    tr '\000' '\n' | ds_harmonize
```

Might be written like this in Ada 2022 (for earlier versions, use '( )' rather than '[ ]'):

```ada
    with Shell;
    procedure SDS_to_DS is
       use Shell;
       DS_Pipe         : constant Pipe := To_Pipe;
       Zero_To_Newline : Process := Start (Program   => "tr",
                                           Arguments => [+"\000", +"\n"],
                                           Output    => DS_Pipe,
                                           Pipeline  => True);
       DS_Harmonize    : Process := Start (Program   => "ds_harmonize",
                                           Arguments => Nil_Strings,
                                           Input     => DS_Pipe);
    begin
       Close_Write_End (DS_Pipe);

       Wait_On (Zero_To_Newline);
       Wait_On (DS_Harmonize);
    end SDS_to_DS;
```
