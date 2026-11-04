# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

aShell is an Ada 2022 library for starting and interacting with OS processes, pipes and pipelines (POSIX only, via Florist).

## Build

There is no top-level project or Makefile. Each `.gpr` is built from its own directory with `gprbuild`. `library/` must be on `GPR_PROJECT_PATH`. Dependencies are `lib_florist` (library) and `ahven` (unit tests only, and not declared in `alire.toml`).

```shell
cd library && gprbuild -P ashell.gpr                       # Library, debug mode (objects in build/debug).
cd library && gprbuild -P ashell.gpr -XaShell_Build_Mode=fast
cd applet/spawn_server && ./install_spawn_server.sh        # Builds and copies ashell_spawn_server to /usr/bin (uses sudo).
```

- The build mode variable is `aShell_Build_Mode` (`debug`, `fast`, `profile`, `small`). Apart from that name, `library/ashell_shared.gpr` deliberately mirrors lace's `lace_shared.gpr`. Projects importing aShell must set it separately from `Lace_Build_Mode`.
- All switches live in `ashell_shared.gpr`, and other projects rename its packages. There is no separate lint step: `-gnatwa` and the `-gnaty` style checks run on every compile. They only warn, so read the compiler output.
- `config/` and `alire/` are Alire-generated and gitignored. `ashell.gpr` does not use `config/ashell_config.gpr`, so edit switches in `ashell_shared.gpr`.

## Tests

**Ahven unit tests** (`library/test/unit`) cover pipes and processes:

```shell
cd library/test/unit && gprbuild -P test_all.gpr && ./test_all
```

`core/pipe/test_pipes.gpr` and `core/process/test_processes.gpr` build the two suites separately, and `core/test_core.gpr` builds both.

**Applet tests** (`applet/tests`) are standalone programs, one per directory:

```shell
cd applet/tests/core/piped_processes && gprbuild -P test_piped_processes.gpr && ./test_piped_processes
```

- Directories: `core/` (`Shell`), `commands/safe/`, `commands/unsafe/`, `commands/interactive/`, `directories/`, `stress/`.
- `all_core_tests`, `all_safe_command_tests` and `all_unsafe_command_tests` call the individual test procedures from one main. Their project files `with` the individual test projects.
- No applet test sets an exit status. A failure shows up as an unhandled exception or in the printed output, so read the output as well as the exit code.
- `test_all_core`, `test_all_unsafe_commands` and `test_ashell` wait for `<Enter>` between steps (`Shell.Testing.Pause_Til_Tester_Is_Ready`).
- `test_spawn_server` loops 100,000 times, as does `test_all_safe_commands` while it calls that test. Run them with a timeout.
- `stress/max_processes` needs `ulimit -s unlimited`, so use its `run_test.sh`.
- `show.sh` and `show_server.sh` run `lsof` on a running test, which is useful for finding leaked file descriptors.
- Each test binary is listed by name in `.gitignore`, so add new ones there.

## Architecture

Three layers, each a child of the one before (see the diagrams in `library/document/`):

- **`Shell`** wraps Florist with `Pipe` and `Process`. Pipe open and close go through the protected object `Safe_Pipes`. Most of the rest is not task safe. A process that feeds a pipe must be started with `Pipeline => True`, and the caller closes the write end once the reader has started (see `test_piped_processes.adb`). With the default `False`, the write end closes straight away and starting the reader raises `Process_Error`. `Start` checks both ends of each non-standard pipe before forking, because a child that can't set up its pipes exits without the parent being told.
- **`Shell.Commands`** defines the abstract `Command` type (controlled, with a shared `Copy_Count` so copies can share one underlying command), `Command_Results`, command line and pipeline parsing, and the spawn server protocol types in its private part.
- **`Shell.Commands.Unsafe` and `Shell.Commands.Safe`** are the two implementations, with almost the same API:
  - `Unsafe` runs commands directly in the calling process, from a single task. It owns its pipes and closes them on finalization.
  - `Safe` can be called from any task, and never spawns from the application process.

### Safe commands and the spawn server

`Safe` works across two processes (`library/document/safe_commands_architecture.png`):

1. The `Spawn_Client` task in `shell-commands-safe.adb` is the only part of the application that talks to the server. It starts when `Safe` elaborates and launches `ashell_spawn_server`, found through `PATH`.
2. The client and server exchange `Server_Action` and `Client_Action` records over the server's stdin and stdout, using `'Output` and `'Input` on `Pipe_Stream`.
3. The server (`applet/spawn_server/shell-commands-spawn_server.adb`) runs each request as an `Unsafe` command, using `Shell.Commands.Unsafe.Privy`, and keeps a map from `Command_Id` to command. It polls in a loop, sending output as it arrives and then `Command_Done`.
4. Results reach the calling task through the protected `Client_Outputs` object held by each `Safe.Command`.

What this means when changing code:

- **The server is a separate binary.** After changing `Unsafe`, `Shell`, or the action types in `shell-commands.ads`, rebuild the server. Otherwise tests keep running the old installed one, and the client and server may disagree on the stream format. To avoid installing it, put `applet/spawn_server` first on `PATH`.
- **Shutdown is automatic.** `Spawn_Client` polls `Is_Callable (Environment_Task)`, which the runtime clears when the main subprogram completes. It then asks the server to shut down, which kills paused commands and waits for running ones. Library-level finalization can't do this, as it runs only after the tasks have ended. Call `Safe.Stop_Spawn_Client` only to shut down earlier. Library-level tasks can't run commands after the main subprogram completes; they get `Command_Error` or `Tasking_Error`.
- **`Safe` and `Unsafe` cannot be in the same program.** Whichever elaborates second raises `Program_Error`, so this fails at startup rather than at compile time. It is also why the safe and unsafe tests have separate aggregates. The server itself uses only `Unsafe`.
- **Failures must not hang callers.** The server reports a command that can't start through `Command_Done`, and logs any other failed action. If the server itself fails it must exit, as its fetcher task would otherwise keep it alive. The client releases all callers with `Command_Error` when it sees any failure, so every call to a `Spawn_Client` entry must also turn `Tasking_Error` into `Command_Error`.
- **Missing server:** elaborating `Safe` raises `Process_Error` if `ashell_spawn_server` is not on `PATH`, even if no command is ever run.

### Debugging

`Shell.Open_Log` and `Close_Log` turn on logging through `Shell.Log`. The server always logs to `aShell_spawn_Server.log` in its working directory, and tests typically log the client side to `aShell_spawn_Client.log`.
