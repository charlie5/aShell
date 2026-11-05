# Changelog

## 2.0.1 (2026-11-06)

### Fixed

- `alire.toml` depended on a `gcc` crate which doesn't exist, so Alire couldn't resolve the dependencies. It now depends on `gnat` 12.1 or later, and `florist_blady` 6.1.0 or later.
- `shell.adb` built only with GNAT 16, as earlier versions rejected one line. It now builds with GNAT 12.1 and later.

## 2.0.0 (2026-11-06)

This release makes commands report failure accurately, and fixes many hangs and lost results. Some programs which built against 1.3.0 will need changes, and some will behave differently.

### Upgrading

- **Reinstall the spawn server.** `Safe` programs need the matching `ashell_spawn_server`. With an older or newer one, callers get a `Command_Error` saying to reinstall. Run `applet/spawn_server/install_spawn_server.sh`.
- **Check calls to `Run (Command_Line)`.** It now raises `Command_Error` if any command in a pipeline fails, not only the last. This includes a command ended by SIGPIPE, as in `yes | head -1`, so a pipeline like that one now raises.

### Changes which can stop code compiling

- `Commands.Failed`, and `Unsafe.Failed` and `Unsafe.Which_Failed` for pipelines, now take their parameter as `in out`, so they can't be given a constant.
- `Command` has a new abstract operation, `Close_Input`, which any type derived from it outside this library must implement.

### Changes in behaviour

- `Run` on a command or pipeline treats a program which can't start, such as one not found on `PATH`, as a failure. `Failed` is then true and the errors hold the reason. It raises only if `Raise_Error` is true, and then with `Command_Error`. `Start` still raises `Process_Error`.
- `Failed` is False until a command has ended. `Which_Failed` passes over commands which haven't ended or started.
- `To_Commands` raises `Command_Error` if a command is missing, such as after a final `|`.
- `Start` raises `Process_Error` if an end of a given pipe is not open, if the `Input` pipe is also the `Output` or `Errors` pipe, or if the `Working_Directory` is not a folder. `Output` and `Errors` may now be the same pipe.
- `Wait_On` waits until a process has ended, as a stopped process hasn't.
- In `Safe`, `Kill` and `Pause` use the command's current status, and `Interrupt` and `Send` raise `Command_Error` on a command which hasn't been started, as in `Unsafe`.
- In `Safe`, pipeline commands stopped by the server are reported as `Killed`, as in `Unsafe`.
- Pipes are close-on-exec, so programs inherit only the pipes given to them.

### Added

- `Close_Input` lets a command started with `Accepts_Input` see the end of its input.
- The spawn server and client check a protocol version at startup. A server which reports the wrong version, or nothing within about 10 seconds, gives callers a `Command_Error`.

### Fixed

- `Send` no longer deadlocks on large input, as input is queued and written in the background.
- `Wait_On` reads a command's output while waiting, so a program can't hang on a full pipe. It also retries when interrupted by a signal.
- Pipelines no longer hang on pipes inherited by other processes, and a pipeline's remaining commands are stopped when one fails.
- `Run` on a pipeline returns the errors of all its commands.
- Output is no longer lost when a command ends.
- Pipes no longer use descriptors which have since been reused, and the descriptor limit on waiting has been lifted.
- A second `Start` no longer kills the program.
- A working directory which doesn't exist no longer leaves a forked copy of the calling program running.
- `Safe` callers are released with `Command_Error` when a command or the server fails, rather than hanging. Output gathered when a pipeline fails to start is kept.
- The `Safe` client and server shut down automatically when the main subprogram completes, so `Stop_Spawn_Client` is needed only to shut down earlier.
- Safe commands' arguments keep what was escaped, and a `|` within an argument no longer makes another command.
- Pipelines are no longer limited to 50 commands.
- Client code may be compiled under any Ada version.
